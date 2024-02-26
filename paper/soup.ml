(*---------------------------------------------------------------------------
   Copyright (c) 2024 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* Definitions from the soup.tex paper *)

module Type = struct (* Can be deleted with OCaml >= 5.1 *)
  type (_, _) eq = Equal : ('a, 'a) eq
  module Id = struct
    type _ id = ..
    module type ID = sig type t type _ id += Id : t id end
    type 'a t = (module ID with type t = 'a)

    let make (type a) () : a t =
      (module struct type t = a type _ id += Id : t id end)

    let provably_equal
        (type a b) ((module A) : a t) ((module B) : b t) : (a, b) eq option
      =
      match A.Id with B.Id -> Some Equal | _ -> None

    let uid (type a) ((module A) : a t) =
      Obj.Extension_constructor.id (Obj.Extension_constructor.of_val A.Id)
  end
end

module String_map = Map.Make (String)

(* Generic representation *)

module Json = struct
  type t =
  | Null of unit | Bool of bool | Number of float | String of string
  | Array of t list | Obj of obj and obj = mem list and mem = string * t
end

(* The finally tagged datatype *)

type ('ret, 'f) dec_fun =
| Dec_fun : 'f -> ('ret, 'f) dec_fun
| Dec_app : ('ret, 'a -> 'b) dec_fun * 'a Type.Id.t -> ('ret, 'b) dec_fun

type ('a, 'b) base_map = { dec : 'a -> 'b; enc : 'b -> 'a;  }

type _ jsont =
| Null : (unit, 'b) base_map -> 'b jsont
| Bool : (bool, 'b) base_map -> 'b jsont
| Number : (float, 'b) base_map -> 'b jsont
| String : (string, 'b) base_map -> 'b jsont
| Array : ('a, 'elt, 'builder) array_map -> 'a jsont
| Obj : ('o, 'o) obj_map -> 'o jsont
| Any : 'a any_map -> 'a jsont
| Map : ('a, 'b) map -> 'b jsont
| Rec : 'a jsont Lazy.t -> 'a jsont

and ('array, 'elt, 'builder) array_map =
{ elt : 'elt jsont;
  dec_empty : 'builder;
  dec_skip : 'builder -> int -> bool;
  dec_add : 'builder -> int -> 'elt -> 'builder;
  dec_finish : 'builder -> 'array;
  enc : 'acc. ('acc -> 'elt -> 'acc) -> 'acc -> 'array -> 'acc; }

and ('o, 'dec) obj_map =
{ dec : ('o, 'dec) dec_fun;
  mem_decs :  mem_dec String_map.t;
  mem_encs : 'o mem_enc list;
  shape : 'o obj_shape; }

and mem_dec = Mem_dec : ('o, 'a) mem_map -> mem_dec
and 'o mem_enc = Mem_enc : ('o, 'a) mem_map -> 'o mem_enc
and ('o, 'a) mem_map =
{ name : string;
  type' : 'a jsont;
  id : 'a Type.Id.t;
  dec_absent : 'a option;
  enc : 'o -> 'a;
  enc_omit : 'a -> bool; }

and 'o obj_shape =
| Obj_basic : ('o, 'mems, 'builder) unknown_mems -> 'o obj_shape
| Obj_cases : ('o, 'cases, 'tag) obj_cases -> 'o obj_shape

and ('o, 'mems, 'builder) unknown_mems =
| Unknown_skip : ('o, unit, unit) unknown_mems
| Unknown_error : ('o, unit, unit) unknown_mems
| Unknown_keep :
    ('mems, 'a, 'builder) mems_map * ('o -> 'mems) ->
    ('o, 'mems, 'builder) unknown_mems

and ('mems, 'a, 'builder) mems_map =
{ mems_type : 'a jsont;
  id : 'mems Type.Id.t;
  dec_empty : 'builder;
  dec_add : string -> 'a -> 'builder -> 'builder;
  dec_finish : 'builder -> 'mems;
  enc : 'acc. (string -> 'a -> 'acc -> 'acc) -> 'mems -> 'acc -> 'acc }

and ('o, 'cases, 'tag) obj_cases =
{ tag : ('o, 'tag) mem_map; (* 'o is irrelevant, 'tag is not stored *)
  tag_compare : 'tag -> 'tag -> int;
  id : 'cases Type.Id.t;
  cases : ('cases, 'tag) case list;
  enc : 'o -> 'cases;
  enc_case : 'cases -> ('cases, 'tag) case_value; }

and ('cases, 'tag) case =
| Case : ('cases, 'case, 'tag) case_map -> ('cases, 'tag) case

and ('cases, 'case, 'tag) case_map =
{ tag : 'tag;
  obj_map : ('case, 'case) obj_map;
  dec : 'case -> 'cases; }

and ('cases, 'tag) case_value =
| Case_value :
    ('cases, 'case, 'tag) case_map * 'case -> ('cases, 'tag) case_value

and 'a any_map =
{ dec_null : 'a jsont option;
  dec_bool : 'a jsont option;
  dec_number : 'a jsont option;
  dec_string : 'a jsont option;
  dec_array : 'a jsont option;
  dec_obj : 'a jsont option;
  enc : 'a -> 'a jsont; }

and ('a, 'b) map =
{ dom : 'a jsont;
  map : ('a, 'b) base_map }

(* Errors  *)

let type_error () = failwith "type error"
let unexpected_member n = failwith ("Unexpected member " ^ n)
let missing_member n = failwith ("Missing member " ^ n)
let unknown_case_tag () = failwith "Unknown case tag"

(* Any examples *)

let option : 'a jsont -> 'a option jsont = fun t ->
  let none = Null { dec = Fun.const None; enc = Fun.const () } in
  let some = Map { dom = t; map = {dec = Option.some; enc = Option.get}}in
  let enc = function None -> none | Some _ -> some in
  let none = Some none and some = Some some in
  Any { dec_null = none; dec_bool = some; dec_number = some;
        dec_string = some; dec_array = some; dec_obj = some; enc; }

let json : Json.t jsont = (* left as an exercise in the paper *)
  let null =
    Null { dec = (fun () -> Json.Null ());
           enc = (function Json.Null () -> () | j -> type_error ()) }
  in
  let bool =
    Bool { dec = (fun b -> Json.Bool b);
           enc = (function Json.Bool b -> b | j -> type_error ()) }
  in
  let number =
    Number { dec = (fun n -> Json.Number n);
             enc = (function Json.Number n -> n | j -> type_error ()) }
  in
  let string =
    String  { dec = (fun s -> Json.String s);
              enc = (function Json.String s -> s | j -> type_error ()) }
  in
  let rec array =
    let dec_empty = [] and dec_add a _i v = v :: a in
    let dec_finish elts = Json.Array (List.rev elts) in
    let dec_skip _ _ = false in
    let enc f acc = function
    | Json.Array vs -> List.fold_left f acc vs | _ -> type_error ()
    in
    Array { elt = Rec json; dec_empty; dec_add; dec_skip; dec_finish; enc }
  and obj =
    let mems_id = Type.Id.make () in
    let mems =
      let dec_empty = [] in
      let dec_add n v ms = (n, v) :: ms in
      let dec_finish ms = Json.Obj (List.rev ms) in
      let enc f j acc = match j with
      | Json.Obj ms -> List.fold_left (fun acc (n, v) -> f n v acc) acc ms
      | _ -> type_error ()
      in
      { mems_type = Rec json; id = mems_id; dec_empty; dec_add; dec_finish; enc}
    in
    Obj { dec = Dec_app (Dec_fun Fun.id, mems_id);
          mem_decs = String_map.empty; mem_encs = [];
          shape = Obj_basic (Unknown_keep (mems, Fun.id)) }
  and json =
    let enc = function
    | Json.Null _ -> null | Json.Bool _ -> bool | Json.Number _ -> number
    | Json.String _ -> string | Json.Array _ -> array | Json.Obj _ -> obj
    in
    lazy (Any { dec_null = Some null; dec_bool = Some bool;
                dec_number = Some number; dec_string = Some string;
                dec_array = Some array; dec_obj = Some obj; enc })
  in
  Lazy.force json

(* Heterogeneous key-value maps *)

module Dict = struct
  module M = Map.Make (Int)
  type binding = B : 'a Type.Id.t * 'a -> binding
  type t = binding M.t
  let empty = M.empty
  let add k v m = M.add (Type.Id.uid k) (B (k, v)) m
  let find : type a. a Type.Id.t -> t -> a option =
  fun k m -> match M.find_opt (Type.Id.uid k) m with
  | None -> None
  | Some B (k', v) ->
      match Type.Id.provably_equal k k' with
      | Some Type.Equal -> Some v | None -> assert false
end

type ('ret, 'f) app =
| Fun : 'f -> ('ret, 'f) app
| App : ('ret, 'a -> 'b) app * 'a -> ('ret, 'b) app

let rec apply_dict : type ret f. (ret, f) dec_fun -> Dict.t -> f =
fun dec dict -> match dec with
| Dec_fun f -> f
| Dec_app (f,arg) -> (apply_dict f dict) (Option.get (Dict.find arg dict))

(* Decode *)

let rec decode : type a. a jsont -> Json.t -> a =
fun t j -> match t with
| Null map -> (match j with Json.Null v -> map.dec v | _ -> type_error ())
| Bool map -> (match j with Json.Bool b -> map.dec b | _ -> type_error ())
| Number map ->
    (match j with
    | Json.Number n -> map.dec n | Json.Null _ -> map.dec Float.nan
    | _ -> type_error ())
| String map -> (match j with Json.String s -> map.dec s | _ -> type_error ())
| Array map ->
    (match j with Json.Array vs -> decode_array map vs | j -> type_error ())
| Obj map ->
    (match j with Json.Obj mems -> decode_obj map mems | j -> type_error ())
| Map map -> map.map.dec (decode map.dom j)
| Any map -> decode_any t map j
| Rec t -> decode (Lazy.force t) j

and decode_array : type a e b. (a, e, b) array_map -> Json.t list -> a =
fun map vs ->
  let add (i, a) v =
    i + 1, (if map.dec_skip a i then a else map.dec_add a i (decode map.elt v))
  in
  map.dec_finish (snd (List.fold_left add (0, map.dec_empty) vs))

and decode_obj : type o. (o, o) obj_map -> Json.obj -> o =
fun map mems ->
  apply_dict map.dec @@
  decode_obj_map map String_map.empty String_map.empty Dict.empty mems

and decode_obj_map : type o.
  (o, o) obj_map -> mem_dec String_map.t -> mem_dec String_map.t -> Dict.t ->
  Json.obj -> Dict.t
=
fun map mem_miss mem_decs dict mems ->
  let u n _ _ = invalid_arg (n ^ "member defined twice") in
  let mem_miss = String_map.union u mem_miss map.mem_decs in
  let mem_decs = String_map.union u mem_decs map.mem_decs in
  match map.shape with
  | Obj_cases cases -> decode_obj_case cases mem_miss mem_decs dict [] mems
  | Obj_basic u ->
      match u with
      | Unknown_skip -> decode_obj_basic u () mem_miss mem_decs dict mems
      | Unknown_error -> decode_obj_basic u () mem_miss mem_decs dict mems
      | Unknown_keep (map, _) ->
          decode_obj_basic u map.dec_empty mem_miss mem_decs dict mems

and decode_obj_basic : type o map builder.
  (o, map, builder) unknown_mems -> builder -> mem_dec String_map.t ->
  mem_dec String_map.t -> Dict.t -> Json.obj -> Dict.t
=
fun u umap mem_miss mem_decs dict -> function
| [] ->
    let dict = match u with
    | Unknown_skip | Unknown_error -> dict
    | Unknown_keep (map, _) -> Dict.add map.id (map.dec_finish umap) dict
    in
    let add_default _ (Mem_dec m) dict = match m.dec_absent with
    | Some v -> Dict.add m.id v dict | None -> missing_member m.name
    in
    String_map.fold add_default mem_miss dict
| (n, v) :: mems ->
    match String_map.find_opt n mem_decs with
    | Some (Mem_dec m) ->
        let dict = Dict.add m.id (decode m.type' v) dict in
        let mem_miss = String_map.remove n mem_miss in
        decode_obj_basic u umap mem_miss mem_decs dict mems
    | None ->
        match u with
        | Unknown_skip -> decode_obj_basic u umap mem_miss mem_decs dict mems
        | Unknown_error -> unexpected_member n
        | Unknown_keep (map, _) ->
            let umap = map.dec_add n (decode map.mems_type v) umap in
            decode_obj_basic u umap mem_miss mem_decs dict mems

and decode_obj_case : type o cases tag.
  (o, cases, tag) obj_cases -> mem_dec String_map.t -> mem_dec String_map.t ->
  Dict.t -> Json.obj -> Json.obj -> Dict.t
=
fun cases mem_miss mem_decs dict delay mems ->
  let decode_case_tag tag =
    let eq_tag (Case c) = cases.tag_compare c.tag tag = 0 in
    match List.find_opt eq_tag cases.cases with
    | None -> unknown_case_tag ()
    | Some (Case case) ->
        let mems = List.rev_append delay mems in
        let dict = decode_obj_map case.obj_map mem_miss mem_decs dict mems in
        Dict.add cases.id (case.dec (apply_dict case.obj_map.dec dict)) dict
  in
  match mems with
  | [] ->
      (match cases.tag.dec_absent with
      | Some t -> decode_case_tag t | None -> missing_member cases.tag.name)
  | (n, v as mem) :: mems ->
      if n = cases.tag.name then decode_case_tag (decode cases.tag.type' v) else
      match String_map.find_opt n mem_decs with
      | None -> decode_obj_case cases mem_miss mem_decs dict (mem :: delay) mems
      | Some (Mem_dec m) ->
          let dict = Dict.add m.id (decode m.type' v) dict in
          let mem_miss = String_map.remove n mem_miss in
          decode_obj_case cases mem_miss mem_decs dict delay mems

and decode_any : type a. a jsont -> a any_map -> Json.t -> a =
fun t map j ->
  let dec t m j = match m with Some t -> decode t j | None -> type_error () in
  match j with
  | Json.Null _ -> dec t map.dec_null j
  | Json.Bool _ -> dec t map.dec_bool j
  | Json.Number _ -> dec t map.dec_number j
  | Json.String _ -> dec t map.dec_string j
  | Json.Array _ -> dec t map.dec_array j
  | Json.Obj _ -> dec t map.dec_obj j

(* Encode *)

let rec encode : type a. a jsont -> a -> Json.t =
fun t v -> match t with
| Null map -> Json.Null (map.enc v)
| Bool map -> Json.Bool (map.enc v)
| Number map -> Json.Number (map.enc v)
| String map -> Json.String (map.enc v)
| Array map ->
    let encode_elt a elt = (encode map.elt elt) :: a in
    Json.Array (List.rev (map.enc encode_elt [] v))
| Obj map -> Json.Obj (List.rev (encode_obj map v []))
| Any map -> encode (map.enc v) v
| Map map -> encode map.dom (map.map.enc v)
| Rec t -> encode (Lazy.force t) v

and encode_obj : type o. (o, o) obj_map -> o -> Json.obj -> Json.obj =
fun map o obj ->
  let encode_mem obj (Mem_enc map) =
    let v = map.enc o in
    if map.enc_omit v then obj else (map.name, encode map.type' v) :: obj
  in
  let obj = List.fold_left encode_mem obj map.mem_encs in
  match map.shape with
  | Obj_basic (Unknown_keep (map, enc)) ->
      let encode_mem n v obj = (n, encode map.mems_type v) :: obj in
      map.enc encode_mem (enc o) obj
  | Obj_basic _ -> obj
  | Obj_cases cases ->
      let Case_value (case, c) = cases.enc_case (cases.enc o) in
      let obj =
        if cases.tag.enc_omit case.tag then obj else
        (cases.tag.name, encode cases.tag.type' case.tag) :: obj
      in
      encode_obj case.obj_map c obj

(* Object construction *)

let obj_mem :
  string -> 'a jsont -> enc:('o -> 'a) ->
  ('o, 'a -> 'b) obj_map -> ('o, 'b) obj_map
=
fun name type' ~enc obj_map ->
  let id = Type.Id.make () in
  let dec_absent = None and enc_omit = Fun.const false in
  let mm = { name; type'; id; dec_absent; enc; enc_omit } in
  let dec = Dec_app (obj_map.dec, mm.id) in
  let mem_decs = String_map.add mm.name (Mem_dec mm) obj_map.mem_decs in
  let mem_encs = Mem_enc mm :: obj_map.mem_encs in
  { obj_map with dec; mem_decs; mem_encs; }

let bool = Bool { dec = Fun.id; enc = Fun.id }
let string = String { dec = Fun.id; enc = Fun.id }
let obj_finish o = Obj { o with mem_encs = List.rev o.mem_encs }
let obj_map : 'dec -> ('o, 'dec) obj_map = fun make ->
  let dec = Dec_fun make and shape = Obj_basic Unknown_skip in
  { dec; mem_decs = String_map.empty; mem_encs = []; shape }

module Message = struct
  type t = { content : string; public : bool }
  let make content public = { content; public }
  let content msg = msg.content
  let public msg = msg.public
  let jsont : t jsont =
    obj_map make
    |> obj_mem "content" string ~enc:content
    |> obj_mem "public" bool ~enc:public
    |> obj_finish
end

(* Queries and updates *)

type 'a query = 'a jsont
let query : 'a query -> Json.t -> 'a = decode

let get_mem : string -> 'a query -> 'a query = fun name q ->
  obj_map Fun.id |> obj_mem name q ~enc:Fun.id |> obj_finish

let get_nth : int -> 'a query -> 'a query = fun nth q ->
  let dec_empty = None and dec_add _ _ v = Some v in
  let dec_skip _ k = nth <> k in
  let dec_finish = function None -> failwith "too short" | Some v -> v in
  let enc f acc v = f acc v (* Singleton array with the query result *) in
  Array { elt = q; dec_empty; dec_add; dec_skip; dec_finish; enc }

let update_mem : string -> 'a jsont -> Json.t jsont = fun name q ->
  let dec = function
  | Json.Obj ms ->
      let update (n, v as m) =
        if n = name then (n, encode q (decode q v)) else m
      in
      Json.Obj (List.map update ms)
  | _ -> failwith "type error"
  in
  Map { dom = json; map = { dec; enc = Fun.id } }

let delete_mem : string -> Json.t query = fun name ->
  let dec = function
  | Json.Obj ms -> Json.Obj (List.filter (fun (n, _) -> n <> name) ms)
  | _ -> type_error ()
  in
  Map { dom = json; map = { dec; enc = Fun.id } }

let const : 'a jsont -> 'a -> 'a jsont = fun t v ->
  let dec _ = v and enc _ = encode t v in
  Map { dom = json; map = { dec; enc } }

(* Implementations not in the paper *)

let map : ('a -> 'b) -> ('b -> 'a) -> 'a jsont -> 'b jsont =
fun f g t -> Map { dom = t; map = { dec = f; enc = g }}

let update_nth : int -> 'a jsont -> Json.t jsont = fun nth q ->
  let dec = function
  | Json.Array vs ->
      let update i v = if i = nth then encode q (decode q v) else v in
      Json.Array (List.mapi update vs)
  | _ -> failwith "type error"
  in
  Map { dom = json; map = { dec; enc = Fun.id } }

let delete_nth : int -> Json.t query = fun nth ->
  let dec = function
  | Json.Array vs ->
      let add (i, acc) v = i + 1, (if i = nth then acc else v :: acc) in
      Json.Array (List.rev (snd (List.fold_left add (0, []) vs)))
  | _ -> type_error ()
  in
  Map { dom = json; map = { dec; enc = Fun. id }}
