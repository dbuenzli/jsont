(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Fmt = Jsont_base.Fmt
type 'a fmt = 'a Fmt.t
let pp_kind = Fmt.code
let pp_kind_opt ppf kind = if kind = "" then () else pp_kind ppf kind
let pp_name = Fmt.code
let pp_int ppf i = Fmt.code ppf (Int.to_string i)

module Textloc = Jsont_base.Textloc
module Meta = Jsont_base.Meta
type 'a node = 'a * Meta.t

module Path = Jsont_base.Path
module Sort = Jsont_base.Sort

type error_kind = string
type context_index = string node * Path.index
type context = context_index list
type error = context * Meta.t * error_kind
exception Error of error

module Error = struct

  (* Kinds of errors *)

  type kind = error_kind
  let kind_to_string k = k

  (* Errors *)

  module Context = struct
    type index = context_index
    type t = context
    let empty = []
    let is_empty ctx = ctx = []
    let push_array kinded_sort n ctx = (kinded_sort, Path.Nth n) :: ctx
    let push_object kinded_sort n ctx = (kinded_sort, Path.Mem n) :: ctx
    let pp ppf ctx =
      let pp_meta ppf meta =
        if Meta.is_none meta then () else
        Fmt.pf ppf "%a: " Textloc.pp (Meta.textloc meta)
      in
      let pp_el ppf (kind, index) = match index with
      | Path.Nth (n, meta) ->
          Fmt.pf ppf "@[<v>%aat index %a of@,%a%a@]"
            pp_meta meta pp_int n
            pp_meta (snd kind) pp_kind (fst kind)
      | Path.Mem (name, meta) ->
          Fmt.pf ppf "@[<v>%ain member %a of@,%a%a@]"
            pp_meta meta pp_name name
            pp_meta (snd kind) pp_kind (fst kind)
      in
      if ctx = [] then () else
      Fmt.pf ppf "@,@[<v>%a@]" (Fmt.list pp_el) (List.rev ctx)
  end

  type t = error

  let make_msg ctx meta msg = ctx, meta, msg
  let raise ctx meta msg = raise_notrace (Error (ctx, meta, msg))
  let msg meta msg = raise_notrace (Error (Context.empty, meta, msg))
  let msgf meta fmt = Format.kasprintf (fun m -> msg meta m) fmt
  let push_array kinded_sort n (ctx, meta, e) =
    raise_notrace (Error (Context.push_array kinded_sort n ctx, meta, e))

  let push_object kinded_sort n (ctx, meta, e) =
    raise_notrace (Error (Context.push_object kinded_sort n ctx, meta, e))

  let adjust_context ~first_byte ~first_line (ctx, meta, e) = match ctx with
  | [] -> raise_notrace (Error (ctx, meta, e))
  | ((sort, smeta), idx) :: is ->
      let textloc = Meta.textloc smeta in
      let textloc =
        if Textloc.is_none textloc then textloc else
        Textloc.set_first textloc ~first_byte ~first_line
      in
      let smeta = Meta.with_textloc smeta textloc in
      let ctx = ((sort, smeta), idx) :: is in
      raise_notrace (Error (ctx, meta, e))

  let pp ppf (ctx, m, msg) =
    let pp_meta ppf m =
      if not (Meta.is_none m)
      then Fmt.pf ppf "@,%a:" Textloc.pp (Meta.textloc m)
    in
    Fmt.pf ppf "@[<v>%a%a%a@]" Fmt.lines msg pp_meta m Context.pp ctx

  let to_string e = Format.asprintf "%a" pp e

  let puterr = Fmt.puterr
  let disable_ansi_styler = Fmt.disable_ansi_styler

  (* Predefined errors *)

  let sort meta ~exp ~fnd =
    msgf meta "Expected %a but found %a" Sort.pp exp Sort.pp fnd

  let kinded_sort meta ~exp ~fnd =
    msgf meta "Expected %a but found %a" Fmt.code exp Sort.pp fnd

  let missing_mems meta ~kinded_sort ~exp ~fnd =
    let pp_miss ppf m =
      Fmt.pf ppf "@[%a%a@]" Fmt.code m Fmt.similar_mems (m, fnd)
    in
    match exp with
    | [n] ->
        msgf meta "@[<v>Missing member %a in %a%a@]"
          Fmt.code n Fmt.code kinded_sort Fmt.similar_mems (n, fnd)
    | exp ->
        msgf meta "@[<v1>Missing members in %a:@,%a@]"
          Fmt.code kinded_sort (Fmt.list pp_miss) exp

  let unexpected_mems meta ~kinded_sort ~exp ~fnd =
    let pp_unexp ppf m =
      Fmt.pf ppf " @[%a%a@]" Fmt.code m Fmt.should_it_be_mem (m, exp)
    in
    match fnd with
    | [(u, _)] ->
        msgf meta "@[<v>Unexpected member %a for %a%a@]"
          Fmt.code u Fmt.code kinded_sort Fmt.should_it_be_mem (u, exp)
    | us ->
        msgf meta "@[<v1>Unexpected members for %a:@,%a@]"
          Fmt.code kinded_sort (Fmt.list pp_unexp) (List.map fst us)

  let unexpected_case_tag meta ~kinded_sort ~mem_name ~exp ~fnd =
    let pp_kind ppf () =
      Fmt.pf ppf "member %a value in %a" Fmt.code mem_name Fmt.code kinded_sort
    in
    msgf meta "@[%a@]" (Fmt.out_of_dom ~pp_kind ()) (fnd, exp)

  (* Numbers *)

  let index_out_of_range meta ~n ~len =
    msgf meta "Index %a out of range [%a;%a]" pp_int n pp_int 0 pp_int (len - 1)

  let number_range meta ~kind n =
    msgf meta "Number %a not in %a range"
      Fmt.code (Fmt.str "%a" Fmt.json_number n) Fmt.code kind

  let parse_string_number meta ~kind s =
    msgf meta "String %a does not parse to %a value"
      Fmt.json_string s pp_kind kind

  let integer_range meta ~kind n =
    msgf meta "Integer %a not in %a range" pp_int n pp_kind kind

  (* Maps *)

  let no_decoder meta ~kind = msgf meta "No decoder for %a" pp_kind kind
  let no_encoder meta ~kind = msgf meta "No encoder for %a" pp_kind kind
  let decode_todo meta ~kind_opt:k = msgf meta "TODO: decode%a" pp_kind_opt k
  let encode_todo meta ~kind_opt:k = msgf meta "TODO: encode%a" pp_kind_opt k
  let for' meta ~kind e = msgf meta "%a: %s" pp_kind kind e
end

(* Types *)

module Repr = struct (* See the .mli for documentation *)
  module String_map = Map.Make (String)
  module Type = Jsont_base.Type

  type ('ret, 'f) dec_fun =
  | Dec_fun : 'f -> ('ret, 'f) dec_fun
  | Dec_app : ('ret, 'a -> 'b) dec_fun * 'a Type.Id.t -> ('ret, 'b) dec_fun

  type ('a, 'b) base_map =
  { kind : string;
    doc : string;
    dec : Meta.t -> 'a -> 'b;
    enc : 'b -> 'a;
    enc_meta : 'b -> Meta.t; }

  type 'a t =
  | Null : (unit, 'a) base_map -> 'a t
  | Bool : (bool, 'a) base_map -> 'a t
  | Number : (float, 'a) base_map -> 'a t
  | String : (string, 'a) base_map -> 'a t
  | Array : ('a, 'elt, 'builder) array_map -> 'a t
  | Object : ('o, 'o) object_map -> 'o t
  | Any : 'a any_map -> 'a t
  | Map : ('a, 'b) map -> 'b t
  | Rec : 'a t Lazy.t -> 'a t

  and ('array, 'elt, 'builder) array_map =
  { kind : string;
    doc : string;
    elt : 'elt t;
    dec_empty : unit -> 'builder;
    dec_skip : int -> 'builder -> bool;
    dec_add : int -> 'elt -> 'builder -> 'builder;
    dec_finish : Meta.t -> int -> 'builder -> 'array;
    enc : 'acc. ('acc -> int -> 'elt -> 'acc) -> 'acc -> 'array -> 'acc;
    enc_meta : 'array -> Meta.t; }

  and ('o, 'dec) object_map =
  { kind : string;
    doc : string;
    dec : ('o, 'dec) dec_fun;
    mem_decs : mem_dec String_map.t;
    mem_encs : 'o mem_enc list;
    enc_meta : 'o -> Meta.t;
    shape : 'o object_shape; }

  and mem_dec = Mem_dec : ('o, 'a) mem_map -> mem_dec
  and 'o mem_enc = Mem_enc : ('o, 'a) mem_map -> 'o mem_enc
  and ('o, 'a) mem_map =
  { name : string;
    doc : string;
    type' : 'a t;
    id : 'a Type.Id.t;
    dec_absent : 'a option;
    enc : 'o -> 'a;
    (* enc_name_meta : 'a -> Meta.t; See comment in .mli *)
    enc_omit : 'a -> bool; }

  and 'o object_shape =
  | Object_basic : ('o, 'mems, 'builder) unknown_mems -> 'o object_shape
  | Object_cases :
      ('o, 'mems, 'builder) unknown_mems option *
      ('o, 'cases, 'tag) object_cases -> 'o object_shape

  and ('o, 'mems, 'builder) unknown_mems =
  | Unknown_skip : ('o, unit, unit) unknown_mems
  | Unknown_error : ('o, unit, unit) unknown_mems
  | Unknown_keep :
      ('mems, 'a, 'builder) mems_map * ('o -> 'mems) ->
      ('o, 'mems, 'builder) unknown_mems

  and ('mems, 'a, 'builder) mems_map =
  { kind : string;
    doc : string;
    mems_type : 'a t;
    id : 'mems Type.Id.t;
    dec_empty : unit -> 'builder;
    dec_add : Meta.t -> string -> 'a -> 'builder -> 'builder;
    dec_finish : Meta.t -> 'builder -> 'mems;
    enc :
      'acc. (Meta.t -> string -> 'a -> 'acc -> 'acc) -> 'mems -> 'acc -> 'acc }

  and ('o, 'cases, 'tag) object_cases =
  { tag : ('tag, 'tag) mem_map;
    tag_compare : 'tag -> 'tag -> int;
    tag_to_string : ('tag -> string) option;
    id : 'cases Type.Id.t;
    cases : ('cases, 'tag) case list;
    enc : 'o -> 'cases;
    enc_case : 'cases -> ('cases, 'tag) case_value; }

  and ('cases, 'case, 'tag) case_map =
  { tag : 'tag;
    object_map : ('case, 'case) object_map;
    dec : 'case -> 'cases; }

  and ('cases, 'tag) case_value =
  | Case_value :
      ('cases, 'case, 'tag) case_map * 'case -> ('cases, 'tag) case_value

  and ('cases, 'tag) case =
  | Case : ('cases, 'case, 'tag) case_map -> ('cases, 'tag) case

  and 'a any_map =
  { kind : string;
    doc : string;
    dec_null : 'a t option;
    dec_bool : 'a t option;
    dec_number : 'a t option;
    dec_string : 'a t option;
    dec_array : 'a t option;
    dec_object : 'a t option;
    enc : 'a -> 'a t; }

  and ('a, 'b) map =
  { kind : string;
    doc : string;
    dom : 'a t;
    dec : 'a -> 'b;
    enc : 'b -> 'a; }

  (* Convert *)

  let of_t = Fun.id
  let unsafe_to_t = Fun.id

  (* Kinds and doc *)

  let object_map_kinded_sort (map : ('o, 'dec) object_map) =
    Sort.kinded ~kind:map.kind Object

  let rec kinded_sort : type a. a t -> string = function
  | Null map -> Sort.kinded ~kind:map.kind Null
  | Bool map -> Sort.kinded ~kind:map.kind Bool
  | Number map -> Sort.kinded ~kind:map.kind Number
  | String map -> Sort.kinded ~kind:map.kind String
  | Array map -> array_map_kinded_sort map
  | Object map -> object_map_kinded_sort map
  | Any map -> if map.kind = "" then any_map_kinded_sort map else map.kind
  | Map map -> if map.kind = "" then kinded_sort map.dom else map.kind
  | Rec l -> kinded_sort (Lazy.force l)

  and array_map_kinded_sort : type a e b. (a, e, b) array_map -> string =
  fun map ->
    if map.kind <> "" then Sort.kinded ~kind:map.kind Array else
    let elt = kinded_sort map.elt in
    String.concat "" ["array<"; elt; ">"]

  and any_map_kinded_sort : type a. a any_map -> string = fun map ->
    let add_case ks sort = function
    | None -> ks
    | Some k ->
        (if map.kind <> "" then kinded_sort k else
         Sort.kinded ~kind:map.kind sort)
        :: ks
    in
    let ks = add_case [] Object map.dec_object in
    let ks = add_case ks Array map.dec_array in
    let ks = add_case ks String map.dec_string in
    let ks = add_case ks Number map.dec_number in
    let ks = add_case ks Bool map.dec_bool in
    let ks = add_case ks Null map.dec_null in
    "one of " ^ String.concat ", " ks

  let rec kind : type a. a t -> string = function
  | Null map -> Sort.or_kind ~kind:map.kind Null
  | Bool map -> Sort.or_kind ~kind:map.kind Bool
  | Number map -> Sort.or_kind ~kind:map.kind Number
  | String map -> Sort.or_kind ~kind:map.kind String
  | Array map -> Sort.or_kind ~kind:map.kind Array
  | Object map -> Sort.or_kind ~kind:map.kind Object
  | Any map -> if map.kind <> "" then map.kind else "any"
  | Map map -> if map.kind <> "" then map.kind else kind map.dom
  | Rec l -> kind (Lazy.force l)

  let rec doc : type a. a t -> string = function
  | Null map -> map.doc | Bool map -> map.doc | Number map -> map.doc
  | String map -> map.doc | Array map -> map.doc | Object map -> map.doc
  | Any map -> map.doc | Map map -> map.doc | Rec l -> doc (Lazy.force l)

  (* Errors *)

  let pp_code = Fmt.code
  let pp_kind = pp_kind

  let error_push_object meta map name e =
    Error.push_object ((object_map_kinded_sort map), meta) name e

  let error_push_array meta map i e =
    Error.push_array ((array_map_kinded_sort map), meta) i e

  let type_error meta t ~fnd =
    Error.kinded_sort meta ~exp:(kinded_sort t) ~fnd

  let missing_mems_error meta (object_map : ('o, 'o) object_map) ~exp ~fnd =
    let kinded_sort = object_map_kinded_sort object_map in
    let exp =
      let add n (Mem_dec m) acc = match m.dec_absent with
      | None -> n :: acc | Some _ -> acc
      in
      List.rev (String_map.fold add exp [])
    in
    Error.missing_mems meta ~kinded_sort ~exp ~fnd

  let unexpected_mems_error meta (object_map : ('o, 'o) object_map) ~fnd =
    let kinded_sort = object_map_kinded_sort object_map in
    let exp = List.map (fun (Mem_enc m) -> m.name) object_map.mem_encs in
    Error.unexpected_mems meta ~kinded_sort ~exp ~fnd

  let unexpected_case_tag_error meta object_map object_cases tag =
    let kinded_sort = object_map_kinded_sort object_map in
    let case_to_string (Case c) = match object_cases.tag_to_string with
    | None -> None | Some str -> Some (str c.tag)
    in
    let exp = List.filter_map case_to_string object_cases.cases in
    let fnd = match object_cases.tag_to_string with
    | None -> "<tag>" (* XXX not good *) | Some str -> str tag
    in
    let mem_name = object_cases.tag.name in
    Error.unexpected_case_tag meta ~kinded_sort ~mem_name ~exp ~fnd

  (* Processor toolbox *)

  let object_meta_arg : Meta.t Type.Id.t = Type.Id.make ()

  module Dict = struct
    module M = Map.Make (Int)
    type binding = B : 'a Type.Id.t * 'a -> binding
    type t = binding M.t
    let empty = M.empty
    let mem k m = M.mem (Type.Id.uid k) m
    let add k v m = M.add (Type.Id.uid k) (B (k, v)) m
    let remove k m = M.remove (Type.Id.uid k) m
    let find : type a. a Type.Id.t -> t -> a option =
    fun k m -> match M.find_opt (Type.Id.uid k) m with
    | None -> None
    | Some B (k', v) ->
        match Type.Id.provably_equal k k' with
        | Some Type.Equal -> Some v | None -> assert false
  end

  let rec apply_dict : type ret f. (ret, f) dec_fun -> Dict.t -> f =
  fun dec dict -> match dec with
  | Dec_fun f -> f
  | Dec_app (f, arg) -> (apply_dict f dict) (Option.get (Dict.find arg dict))

  type unknown_mems_option =
  | Unknown_mems :
      ('o, 'mems, 'builder) unknown_mems option -> unknown_mems_option

  let override_unknown_mems ~by umems dict = match by with
  | Unknown_mems None -> umems, dict
  | Unknown_mems _ as by ->
      match umems with
      | Unknown_mems (Some (Unknown_keep (umap, _))) ->
          (* A decoding function still expect [umap.id] argument in
             an Dec_app, we simply stub it with the empty map. *)
          let empty = umap.dec_finish Meta.none (umap.dec_empty ()) in
          let dict = Dict.add umap.id empty dict in
          by, dict
      | _ -> by, dict

  let finish_object_decode : type o p m mems builder.
    (o, o) object_map -> Meta.t -> (p, mems, builder) unknown_mems -> builder ->
    mem_dec String_map.t -> Dict.t -> Dict.t
    =
    fun map meta umems umap mem_decs dict ->
    let dict = Dict.add object_meta_arg meta dict in
    let dict = match umems with
    | Unknown_skip | Unknown_error -> dict
    | Unknown_keep (map, _) -> Dict.add map.id (map.dec_finish meta umap) dict
    in
    let add_default _ (Mem_dec mem_map) dict = match mem_map.dec_absent with
    | Some v -> Dict.add mem_map.id v dict
    | None -> raise Exit
    in
    (try String_map.fold add_default mem_decs dict with
    | Exit ->
        let no_default _ (Mem_dec mm) = Option.is_none mm.dec_absent in
        let exp = String_map.filter no_default mem_decs in
        missing_mems_error meta map ~exp ~fnd:[])
end

(* Types *)

type 'a t = 'a Repr.t
let kinded_sort = Repr.kinded_sort
let kind = Repr.kind
let doc = Repr.doc

(* Base types *)

let enc_meta_none _v = Meta.none

module Base = struct
  type ('a, 'b) map = ('a, 'b) Repr.base_map

  let base_map_sort = "base map"

  let map ?(kind = "") ?(doc = "") ?dec ?enc ?(enc_meta = enc_meta_none) () =
    let dec = match dec with
    | Some dec -> dec
    | None ->
        let kind = Sort.kinded' ~kind base_map_sort in
        fun meta _v -> Error.no_decoder meta ~kind
    in
    let enc = match enc with
    | Some enc -> enc
    | None ->
        let kind = Sort.kinded' ~kind base_map_sort in
        fun _v -> Error.no_encoder Meta.none ~kind
    in
    { Repr.kind; doc; dec; enc; enc_meta }

  let id =
    let dec _meta v = v and enc = Fun.id in
    { Repr.kind = ""; doc = ""; dec; enc; enc_meta = enc_meta_none }

  let ignore =
    let kind = "ignore" in
    let dec _meta _v = () in
    let enc _v =
      let kind = Sort.kinded' ~kind base_map_sort in
      Error.no_encoder Meta.none ~kind
    in
    { Repr.kind; doc = ""; dec; enc; enc_meta = enc_meta_none }

  let null map = Repr.Null map
  let bool map = Repr.Bool map
  let number map = Repr.Number map
  let string map = Repr.String map

  let dec dec = fun _meta v -> dec v
  let dec_result ?(kind = "") dec =
    let kind = Sort.kinded' ~kind base_map_sort in
    fun meta v -> match dec v with
    | Ok v -> v | Error e -> Error.for' meta ~kind e

  let dec_failure ?(kind = "") dec =
    let kind = Sort.kinded' ~kind base_map_sort in
    fun meta v -> try dec v with Failure e -> Error.for' meta ~kind e

  let enc = Fun.id
  let enc_result ?(kind = "") enc =
    let kind = Sort.kinded' ~kind base_map_sort in
    fun v -> match enc v with
    | Ok v -> v | Error e -> Error.for' Meta.none ~kind e

  let enc_failure ?(kind = "") enc =
    let kind = Sort.kinded' ~kind base_map_sort in
    fun v -> try enc v with Failure e -> Error.for' Meta.none ~kind e
end

(* Any *)

let any
    ?(kind = "") ?(doc = "") ?dec_null ?dec_bool ?dec_number ?dec_string
    ?dec_array ?dec_object ?enc ()
  =
  let enc = match enc with
  | Some enc -> enc
  | None ->
      let kind = Sort.kinded' ~kind "any" in
      fun _v -> Error.no_encoder Meta.none ~kind
  in
  Repr.Any { kind; doc; dec_null; dec_bool; dec_number; dec_string; dec_array;
             dec_object; enc }

(* Maps and recursion *)

let map ?(kind = "") ?(doc = "") ?dec ?enc dom =
  let map_sort = "map" in
  let dec = match dec with
  | Some dec -> dec
  | None ->
      let kind = Sort.kinded' ~kind map_sort in
      fun _v -> Error.no_decoder Meta.none ~kind
  in
  let enc = match enc with
  | Some enc -> enc
  | None ->
      let kind = Sort.kinded' ~kind map_sort in
      fun _v -> Error.no_encoder Meta.none ~kind
  in
  Repr.Map { kind; doc; dom; dec; enc }

let iter ?(kind = "") ?(doc = "") ?dec ?enc dom =
  let dec = match dec with
  | None -> Fun.id | Some dec -> fun v -> dec v; v
  in
  let enc = match enc with
  | None -> Fun.id | Some enc -> fun v -> enc v; v
  in
  Repr.Map { kind; doc; dom; dec; enc }

let rec' t = Repr.Rec t

(* Nulls and options *)

let null ?kind ?doc v =
  let dec _meta () = v and enc _meta = () in
  Repr.Null (Base.map ?doc ?kind ~dec ~enc ())

let none =
  let none = (* Can't use [Base.map] because of the value restriction. *)
    let dec _meta _v = None and enc _ = () in
    { Repr.kind = ""; doc = ""; dec; enc; enc_meta = enc_meta_none }
  in
  Repr.Null none

let some t = map ~dec:Option.some ~enc:Option.get t

let option ?kind ?doc t =
  let some = some t in
  let enc = function None -> none | Some _ -> some in
  match t with
  | Null _ -> any ?doc ?kind ~dec_null:none ~enc ()
  | Bool _ -> any ?doc ?kind ~dec_null:none ~dec_bool:some ~enc ()
  | Number _ -> any ?doc ?kind ~dec_null:none ~dec_number:some ~enc ()
  | String _ -> any ?doc ?kind ~dec_null:none ~dec_string:some ~enc ()
  | Array _ -> any ?doc ?kind ~dec_null:none ~dec_array:some ~enc ()
  | Object _ -> any ?doc ?kind ~dec_null:none ~dec_object:some ~enc ()
  | (Any _ | Map _ | Rec _) ->
      any ?doc ?kind ~dec_null:none ~dec_bool:some ~dec_number:some
        ~dec_string:some ~dec_array:some ~dec_object:some ~enc ()

(* Booleans *)

let bool = Repr.Bool Base.id

(* Numbers *)

let[@inline] check_finite_number meta ~kind v =
  if Float.is_finite v then () else
  Error.kinded_sort meta ~exp:(Sort.kinded ~kind Number) ~fnd:Sort.Null

let number = Repr.Number Base.id

let any_float =
  let kind = "float" in
  let finite = number in
  let non_finite =
    let dec m v = match Float.of_string_opt v with
    | Some v -> v | None -> Error.parse_string_number m ~kind v
    in
    Base.string (Base.map ~kind ~dec ~enc:Float.to_string ())
  in
  let enc v = if Float.is_finite v then finite else non_finite in
  any ~kind ~dec_null:finite ~dec_number:finite ~dec_string:non_finite ~enc ()

let float_as_hex_string =
  let kind = "float" in
  let dec meta v = match Float.of_string_opt v with
  | Some v -> v | None -> Error.parse_string_number meta ~kind v
  in
  let enc v = Printf.sprintf "%h" v in
  Base.string (Base.map ~kind ~dec ~enc ())

let uint8 =
  let kind = "uint8" in
  let dec meta v =
    check_finite_number meta ~kind v;
    if Jsont_base.Number.in_exact_uint8_range v then Int.of_float v else
    Error.number_range meta ~kind v
  in
  let enc v =
    if Jsont_base.Number.int_is_uint8 v then Int.to_float v else
    Error.integer_range Meta.none ~kind v
  in
  Base.number (Base.map ~kind ~dec ~enc ())

let uint16 =
  let kind = "uint16" in
  let dec meta v =
    check_finite_number meta ~kind v;
    if Jsont_base.Number.in_exact_uint16_range v then Int.of_float v else
    Error.number_range meta ~kind v
  in
  let enc v =
    if Jsont_base.Number.int_is_uint16 v then Int.to_float v else
    Error.integer_range Meta.none ~kind v
  in
  Base.number (Base.map ~kind ~dec ~enc ())

let int8 =
  let kind = "int8" in
  let dec meta v =
    check_finite_number meta ~kind v;
    if Jsont_base.Number.in_exact_int8_range v then Int.of_float v else
    Error.number_range meta ~kind v
  in
  let enc v =
    if Jsont_base.Number.int_is_int8 v then Int.to_float v else
    Error.integer_range Meta.none ~kind v
  in
  Base.number (Base.map ~kind ~dec ~enc ())

let int16 =
  let kind = "int16" in
  let dec meta v =
    check_finite_number meta ~kind v;
    if Jsont_base.Number.in_exact_int16_range v then Int.of_float v else
    Error.number_range meta ~kind v
  in
  let enc v =
    if Jsont_base.Number.int_is_int16 v then Int.to_float v else
    Error.integer_range Meta.none ~kind v
  in
  Base.number (Base.map ~kind ~dec ~enc ())

let int32 =
  let kind = "int32" in
  let dec meta v =
    check_finite_number meta ~kind v;
    if Jsont_base.Number.in_exact_int32_range v then Int32.of_float v else
    Error.number_range meta ~kind v
  in
  let enc = Int32.to_float (* Everything always fits *)  in
  Base.number (Base.map ~kind ~dec ~enc ())

let int64_as_string =
  let kind = "int64" in
  let dec meta v = match Int64.of_string_opt v with
  | Some v -> v | None -> Error.parse_string_number meta ~kind v
  in
  Base.string (Base.map ~kind ~dec ~enc:Int64.to_string ())

let int64_number =
  (* Usage by [int64] entails there's no need to test for nan or check
     range on encoding. *)
  let kind = "int64" in
  let dec meta v =
    if Jsont_base.Number.in_exact_int64_range v then Int64.of_float v else
    Error.number_range meta ~kind v
  in
  Base.number (Base.map ~kind ~dec ~enc:Int64.to_float ())

let int64 =
  let dec_number = int64_number and dec_string = int64_as_string in
  let enc v =
    if Jsont_base.Number.can_store_exact_int64 v then int64_number else
    int64_as_string
  in
  any ~kind:"int64" ~dec_number ~dec_string ~enc ()

let int_as_string =
  let kind = "OCaml int" in
  let dec meta v = match int_of_string_opt v with
  | Some v -> v | None -> Error.parse_string_number meta ~kind v
  in
  Base.string (Base.map ~kind ~dec ~enc:Int.to_string ())

let int_number =
  (* Usage by [int] entails there's no need to test for nan or check range on
     encoding. *)
  let kind = "OCaml int" in
  let dec meta v =
    if Jsont_base.Number.in_exact_int_range v then Int.of_float v else
    Error.number_range meta ~kind v
  in
  Base.number (Base.map ~kind ~dec ~enc:Int.to_float ())

let int =
  let enc v =
    if Jsont_base.Number.can_store_exact_int v then int_number else
    int_as_string
  in
  let dec_number = int_number and dec_string = int_as_string in
  any ~kind:"OCaml int" ~dec_number ~dec_string ~enc ()

(* String and enums *)

let string = Repr.String Base.id

let of_of_string ?kind ?doc ?enc of_string =
  let dec = Base.dec_result ?kind of_string in
  let enc = match enc with None -> None | Some enc -> Some (Base.enc enc) in
  Base.string (Base.map ?kind ?doc ?enc ~dec ())

let enum (type a) ?(cmp = Stdlib.compare) ?(kind = "") ?doc assoc =
  let kind = Sort.kinded' ~kind "enum" in
  let dec_map =
    let add m (k, v) = Repr.String_map.add k v m in
    let m = List.fold_left add Repr.String_map.empty assoc in
    fun k -> Repr.String_map.find_opt k m
  in
  let enc_map =
    let module M = Map.Make (struct type t = a let compare = cmp end) in
    let add m (k, v) = M.add v k m in
    let m = List.fold_left add M.empty assoc in
    fun v -> M.find_opt v m
  in
  let dec meta s = match dec_map s with
  | Some v -> v
  | None ->
      let kind = Sort.kinded ~kind String in
      let pp_kind ppf () = Fmt.pf ppf "%a value" Repr.pp_kind kind in
      Error.msgf meta "%a" (Fmt.out_of_dom ~pp_kind ()) (s, List.map fst assoc)
  in
  let enc v = match enc_map v with
  | Some s -> s
  | None ->
      Error.msgf Meta.none "Encode %a: unknown enum value" Repr.pp_kind kind
  in
  Base.string (Base.map ~kind ?doc ~dec ~enc ())

let binary_string =
  let kind = "hex" in
  let kind' = Sort.kinded ~kind String in
  let dec = Base.dec_result ~kind:kind' Jsont_base.binary_string_of_hex in
  let enc = Base.enc Jsont_base.binary_string_to_hex in
  Base.string (Base.map ~kind ~dec ~enc ())

(* Arrays and tuples *)

module Array = struct
  type ('array, 'elt, 'builder) map = ('array, 'elt, 'builder) Repr.array_map
  type ('array, 'elt) enc =
    { enc : 'acc. ('acc -> int -> 'elt -> 'acc) -> 'acc -> 'array -> 'acc }

  let array_kind kind = Sort.kinded ~kind Sort.Array
  let default_skip _i _builder = false
  let map
      ?(kind = "") ?(doc = "") ?dec_empty ?dec_skip ?dec_add ?dec_finish
      ?enc ?(enc_meta = enc_meta_none) elt
    =
    let dec_empty = match dec_empty with
    | Some dec_empty -> dec_empty
    | None -> fun () -> Error.no_decoder Meta.none ~kind:(array_kind kind)
    in
    let dec_skip = Option.value ~default:default_skip dec_skip in
    let dec_add = match dec_add with
    | Some dec_add -> dec_add
    | None -> fun _ _ _ -> Error.no_decoder Meta.none ~kind:(array_kind kind)
    in
    let dec_finish = match dec_finish with
    | Some dec_finish -> dec_finish
    | None -> fun _ _ _ -> Error.no_decoder Meta.none ~kind:(array_kind kind)
    in
    let enc = match enc with
    | Some { enc } -> enc
    | None -> fun _ _ _ -> Error.no_encoder Meta.none ~kind:(array_kind kind)
    in
    { Repr.kind; doc; elt; dec_empty; dec_add; dec_skip; dec_finish; enc;
      enc_meta; }

  let list_enc f acc l =
    let rec loop f acc i = function
    | [] -> acc | v :: l -> loop f (f acc i v) (i + 1) l
    in
    loop f acc 0 l

  let list_map ?kind ?doc ?dec_skip elt =
    let dec_empty () = [] in
    let dec_add _i v l = v :: l in
    let dec_finish _meta _len l = List.rev l in
    let enc = { enc = list_enc } in
    map ?kind ?doc ~dec_empty ?dec_skip ~dec_add ~dec_finish ~enc elt

  type 'a array_builder = 'a Jsont_base.Rarray.t

  let array_enc f acc a =
    let acc = ref acc in
    for i = 0 to Array.length a - 1
    do acc := f !acc i (Array.unsafe_get a i) done;
    !acc

  let array_map ?kind ?doc ?dec_skip elt =
    let dec_empty () = Jsont_base.Rarray.empty () in
    let dec_add _i v a = Jsont_base.Rarray.add_last v a in
    let dec_finish _meta _len a = Jsont_base.Rarray.to_array a in
    let enc = { enc = array_enc } in
    map ?kind ?doc ~dec_empty ?dec_skip ~dec_add ~dec_finish ~enc elt

  type ('a, 'b, 'c) bigarray_builder = ('a, 'b, 'c) Jsont_base.Rbigarray1.t

  let bigarray_map ?kind ?doc ?dec_skip k l elt =
    let dec_empty _meta = Jsont_base.Rbigarray1.empty k l in
    let dec_add _i v a = Jsont_base.Rbigarray1.add_last v a in
    let dec_finish _meta _len a = Jsont_base.Rbigarray1.to_bigarray a in
    let enc f acc a =
      let acc = ref acc in
      for i = 0 to Bigarray.Array1.dim a - 1
      do acc := f !acc i (Bigarray.Array1.unsafe_get a i) done;
      !acc
    in
    let enc = { enc } in
    map ?kind ?doc ~dec_empty ?dec_skip ~dec_add ~dec_finish ~enc elt

  let array map = Repr.Array map

  let stub_elt =
    Repr.Map { kind = ""; doc = ""; dom = Base.(null id);
               enc = (fun _ -> assert false);
               dec = (fun _ -> assert false); }

  let ignore =
    let kind = "ignore" in
    let kind' = Sort.kinded ~kind Array in
    let dec_empty () = () and dec_add _i _v () = () in
    let dec_skip _i () = true and dec_finish _meta _len () = () in
    let enc = { enc = fun _ _ () -> Error.no_encoder Meta.none ~kind:kind' } in
    array (map ~kind ~dec_empty ~dec_skip ~dec_add ~dec_finish ~enc stub_elt)

  let zero =
    let dec_empty () = () and dec_add _i _v () = () in
    let dec_skip _i () = true and dec_finish _meta _len () = () in
    let enc = { enc = fun _ acc () -> acc } in
    let kind = "zero" in
    array (map ~kind ~dec_empty ~dec_skip ~dec_add ~dec_finish ~enc stub_elt)
end

let list ?kind ?doc t = Repr.Array (Array.list_map ?kind ?doc t)
let array ?kind ?doc t = Repr.Array (Array.array_map ?kind ?doc t)
let array_as_string_map ?kind ?doc ~key t =
  let dec_empty () = Repr.String_map.empty in
  let dec_add _i elt acc = Repr.String_map.add (key elt) elt acc in
  let dec_finish _meta _len acc = acc in
  let enc f acc m =
    let i = ref (-1) in
    Repr.String_map.fold (fun _ elt acc -> incr i; f acc !i elt) m acc
  in
  let enc = Array.{enc} in
  let map = Array.map ?kind ?doc ~dec_empty ~dec_add ~dec_finish ~enc t in
  Repr.Array map

let bigarray ?kind ?doc k t =
  Repr.Array (Array.bigarray_map ?kind ?doc k Bigarray.c_layout t)

let tuple_no_decoder ~kind meta =
  Error.no_decoder meta ~kind:(Sort.kinded' ~kind "tuple")

let tuple_no_encoder ~kind =
  Error.no_encoder Meta.none ~kind:(Sort.kinded' ~kind "tuple")

let error_tuple_size meta kind ~exp fnd =
  Error.msgf meta "Expected %a elements in %a but found %a"
    pp_int exp pp_kind (Sort.kinded' ~kind "tuple") pp_int fnd

let t2 ?(kind = "") ?doc ?dec ?enc t =
  let size = 2 in
  let dec = match dec with
  | None -> fun meta _v0 _v1 -> tuple_no_decoder ~kind meta
  | Some dec -> fun _meta v0 v1 -> dec v0 v1
  in
  let dec_empty () = [] in
  let dec_add _i v acc = v :: acc in
  let dec_finish meta _len = function
  | [v1; v0] -> dec meta v0 v1
  | l -> error_tuple_size meta kind ~exp:size (List.length l)
  in
  let enc = match enc with
  | None -> fun _f _acc _v -> tuple_no_encoder ~kind
  | Some enc -> fun f acc v -> f (f acc 0 (enc v 0)) 1 (enc v 1)
  in
  let enc = { Array.enc } in
  Repr.Array (Array.map ~kind ?doc ~dec_empty ~dec_add ~dec_finish ~enc t)

let t3 ?(kind = "") ?doc ?dec ?enc t =
  let size = 3 in
  let dec = match dec with
  | None -> fun meta _v0 _v1 _v2 -> tuple_no_decoder ~kind meta
  | Some dec -> fun _meta v0 v1 v2 -> dec v0 v1 v2
  in
  let dec_empty () = [] in
  let dec_add _i v acc = v :: acc in
  let dec_finish meta _len = function
  | [v2; v1; v0] -> dec meta v0 v1 v2
  | l -> error_tuple_size meta kind ~exp:size (List.length l)
  in
  let enc = match enc with
  | None -> fun _f _acc _v -> tuple_no_encoder ~kind
  | Some enc ->
      fun f acc v -> f (f (f acc 0 (enc v 0)) 1 (enc v 1)) 2 (enc v 2)
  in
  let enc = { Array.enc } in
  Repr.Array (Array.map ~kind ?doc ~dec_empty ~dec_add ~dec_finish ~enc t)

let t4 ?(kind = "") ?doc ?dec ?enc t =
  let size = 4 in
  let dec = match dec with
  | None -> fun meta _v0 _v1 _v2 _v3 -> tuple_no_decoder ~kind meta
  | Some dec -> fun _meta v0 v1 v2 v3 -> dec v0 v1 v2 v3
  in
  let dec_empty () = [] in
  let dec_add _i v acc = v :: acc in
  let dec_finish meta _len = function
  | [v3; v2; v1; v0] -> dec meta v0 v1 v2 v3
  | l -> error_tuple_size meta kind ~exp:size (List.length l)
  in
  let enc = match enc with
  | None -> fun _f _acc _v -> tuple_no_encoder ~kind
  | Some enc ->
      fun f acc v ->
        f (f (f (f acc 0 (enc v 0)) 1 (enc v 1)) 2 (enc v 2)) 3 (enc v 3)
  in
  let enc = { Array.enc } in
  Repr.Array (Array.map ~kind ?doc ~dec_empty ~dec_add ~dec_finish ~enc t)

let tn ?(kind = "") ?doc ~n elt =
  let dec_empty () = Jsont_base.Rarray.empty () in
  let dec_add _i v a = Jsont_base.Rarray.add_last v a in
  let dec_finish meta _len a =
    let len = Jsont_base.Rarray.length a in
    if len <> n then error_tuple_size meta kind ~exp:n len else
    Jsont_base.Rarray.to_array a
  in
  let enc = { Array.enc = Array.array_enc } in
  Repr.Array (Array.map ~kind ?doc ~dec_empty ~dec_add ~dec_finish ~enc elt)

(* Objects *)

module Object = struct
  open Repr

  (* Maps *)

  type ('o, 'dec) map = ('o, 'dec) object_map

  let default_shape = Object_basic Unknown_skip

  let _map ?(kind = "") ?(doc = "") ?(enc_meta = enc_meta_none) dec =
    { kind; doc; dec; mem_decs = String_map.empty; mem_encs = [];
      enc_meta; shape = default_shape }

  let map ?kind ?doc dec = _map ?kind ?doc (Dec_fun dec)
  let map' ?kind ?doc ?enc_meta dec =
    _map ?kind ?doc ?enc_meta (Dec_app (Dec_fun dec, object_meta_arg))

  let enc_only ?(kind = "") ?doc ?enc_meta () =
    let dec meta = Error.no_decoder meta ~kind:(Sort.kinded ~kind Object) in
    map' ~kind ?doc ?enc_meta dec

  let check_name_unicity m =
    let add n kind = function
    | None -> Some kind
    | Some kind' ->
        let ks k = Sort.or_kind ~kind Object in
        invalid_arg @@
        Fmt.str "member %s defined both in %s and %s" n (ks kind) (ks kind')
    in
    let rec loop :
      type o dec. string String_map.t -> (o, dec) object_map -> unit
    =
    fun names m ->
      let add_name names n = String_map.update n (add n m.kind) names in
      let add_mem_enc names (Mem_enc m) = add_name names m.name in
      let names = List.fold_left add_mem_enc names m.mem_encs in
      match m.shape with
      | Object_basic _ -> ()
      | Object_cases (u, cases) ->
          let names = add_name names cases.tag.name in
          let check_case (Case c) = loop names c.object_map in
          List.iter check_case cases.cases
    in
    loop String_map.empty m

  let finish mems =
    let () = check_name_unicity mems in
    Object { mems with mem_encs = List.rev mems.mem_encs }

  let get_object_map = function
  | Object map -> map | _ -> invalid_arg "Not an object"

  (* Members *)

  module Mem = struct
    type ('o, 'a) map = ('o, 'a) Repr.mem_map

    let no_enc name = fun _v ->
      Error.msgf Meta.none "No encoder for member %a" pp_code name

    let map ?(doc = "") ?dec_absent ?enc ?enc_omit name type' =
      let id = Type.Id.make () in
      let enc = match enc with None -> no_enc name | Some enc -> enc in
      let enc_omit = match enc_omit with
      | None -> Fun.const false | Some omit -> omit
      in
      { name; doc; type'; id; dec_absent; enc; enc_omit }

    let app object_map mm =
      let mem_decs = String_map.add mm.name (Mem_dec mm) object_map.mem_decs in
      let mem_encs = Mem_enc mm :: object_map.mem_encs in
      let dec = Dec_app (object_map.dec, mm.id) in
      { object_map with dec; mem_decs; mem_encs }
  end

  let mem ?(doc = "") ?dec_absent ?enc ?enc_omit name type' map =
    let mmap =  Mem.map ~doc ?dec_absent ?enc ?enc_omit name type' in
    let mem_decs = String_map.add name (Mem_dec mmap) map.mem_decs in
    let mem_encs = Mem_enc mmap :: map.mem_encs in
    let dec = Dec_app (map.dec, mmap.id) in
    { map with dec; mem_decs; mem_encs }

  let opt_mem ?doc ?enc:e name dom map =
    let dec = Option.some and enc = Option.get in
    let some = Map { kind = ""; doc = ""; dom; dec; enc} in
    mem ?doc ~dec_absent:None ?enc:e ~enc_omit:Option.is_none name some map

  (* Case objects *)

  module Case = struct
    type ('cases, 'case, 'tag) map = ('cases, 'case, 'tag) case_map
    type ('cases, 'tag) t = ('cases, 'tag) case
    type ('cases, 'tag) value = ('cases, 'tag) case_value

    let no_dec _ = Error.msgf Meta.none "No decoder for case"
    let map ?(dec = no_dec) tag obj =
      { tag; object_map = get_object_map obj; dec; }

    let map_tag c = c.tag
    let make c = Case c
    let value c v = Case_value (c, v)
  end

  let check_case_mem map cases ~dec_absent ~tag_compare ~tag_to_string =
    match map.shape with
    | Object_cases _ -> invalid_arg "Multiple calls to Jsont.Object.case_mem"
    | _ ->
        match dec_absent with
        | None -> ()
        | Some tag ->
            (* Check that we have a case definition for it *)
            let equal_t (Case case) = tag_compare case.tag tag = 0 in
            if not (List.exists equal_t cases) then
              let tag = match tag_to_string with
              | None -> "" | Some tag_to_string -> " " ^ tag_to_string tag
              in
              invalid_arg ("No case for dec_absent case member value" ^ tag)

  let case_tag_mem ?(doc = "") name type' ~dec_absent ~enc_omit =
    let id = Type.Id.make () in
    let enc t = t (* N.B. this fact may be used by encoders. *) in
    let enc_omit = match enc_omit with
    | None -> Fun.const false | Some omit -> omit
    in
    { name; doc; type'; id; dec_absent; enc; enc_omit }

  let case_mem
      ?doc ?(tag_compare = Stdlib.compare) ?tag_to_string ?dec_absent
      ?enc ?enc_omit ?enc_case name type' cases map
    =
    let () = check_case_mem map cases ~dec_absent ~tag_compare ~tag_to_string in
    let tag = case_tag_mem ?doc name type' ~dec_absent ~enc_omit in
    let enc = match enc with None -> Mem.no_enc name | Some e -> e in
    let enc_case = match enc_case with
    | Some enc_case -> enc_case
    | None ->
        fun _case ->
          Error.msgf Meta.none "No case encoder for member %a" pp_code name
    in
    let id = Type.Id.make () in
    let cases = {tag; tag_compare; tag_to_string; id; cases; enc; enc_case} in
    let dec = Dec_app (map.dec, id) in
    { map with dec; shape = Object_cases (None, cases) }

  (* Unknown members *)

  module Mems = struct
    type ('mems, 'a) enc =
      { enc :
          'acc. (Meta.t -> string -> 'a -> 'acc -> 'acc) -> 'mems -> 'acc ->
          'acc }

    type ('mems, 'a, 'builder) map = ('mems, 'a, 'builder) mems_map

    let mems_kind kind = Sort.kinded' ~kind "members map"
    let map
        ?(kind = "") ?(doc = "") ?dec_empty ?dec_add ?dec_finish
        ?enc mems_type
      =
      let dec_empty = match dec_empty with
      | Some dec_empty -> dec_empty
      | None -> fun () -> Error.no_decoder Meta.none ~kind:(mems_kind kind)
      in
      let dec_add = match dec_add with
      | Some dec_add -> dec_add
      | None -> fun _ _ _ _ -> Error.no_decoder Meta.none ~kind:(mems_kind kind)
      in
      let dec_finish = match dec_finish with
      | Some dec_finish -> dec_finish
      | None -> fun _ _ -> Error.no_decoder Meta.none ~kind:(mems_kind kind)
      in
      let enc = match enc with
      | Some { enc } -> enc
      | None -> fun _ _ _ -> Error.no_encoder Meta.none ~kind:(mems_kind kind)
      in
      let id = Type.Id.make () in
      { kind; doc; mems_type; id; dec_empty; dec_add; dec_finish; enc }

    let string_map ?kind ?doc type' =
      let dec_empty () = String_map.empty in
      let dec_add _meta n v mems = String_map.add n v mems in
      let dec_finish _meta mems = mems in
      let enc f mems acc =
        String_map.fold (fun n v acc -> f Meta.none n v acc) mems acc
      in
      map ?kind ?doc type' ~dec_empty ~dec_add ~dec_finish ~enc:{enc}
  end

  let set_shape_unknown_mems shape u = match shape with
  | Object_basic (Unknown_keep _) | Object_cases (Some (Unknown_keep _), _) ->
      invalid_arg "Jsont.Object.keep_unknown already called on object"
  | Object_basic _ -> Object_basic u
  | Object_cases (_, cases) -> Object_cases (Some u, cases)

  let skip_unknown map =
    { map with shape = set_shape_unknown_mems map.shape Unknown_skip }

  let error_unknown map =
    { map with shape = set_shape_unknown_mems map.shape Unknown_error }

  let mems_noenc (mems : (_, _, _) mems_map)  _o =
    let kind = Sort.kinded' ~kind:mems.kind "members" in
    Error.no_encoder Meta.none ~kind

  let keep_unknown ?enc mems (map : ('o, 'dec) object_map) =
    let enc = match enc with None -> mems_noenc mems | Some enc -> enc in
    let dec = Dec_app (map.dec, mems.id) in
    let unknown = Unknown_keep (mems, enc) in
    { map with dec; shape = set_shape_unknown_mems map.shape unknown }

  let zero = finish (map ~kind:"zero" ())

  let as_string_map ?kind ?doc t =
    map ?kind ?doc Fun.id
    |> keep_unknown (Mems.string_map t) ~enc:Fun.id
    |> finish
end

(* Ignoring *)

let ignore =
  let kind = "ignore" in
  let dec_null = Repr.Null Base.ignore and dec_bool = Repr.Bool Base.ignore in
  let dec_number = Repr.Number Base.ignore in
  let dec_string = Repr.String Base.ignore in
  let dec_array = Array.ignore and dec_object = Object.zero in
  let enc _v = Error.no_encoder Meta.none ~kind in
  any ~kind ~dec_null ~dec_bool ~dec_number ~dec_string ~dec_array ~dec_object
    ~enc ()

let zero =
  let kind = "zero" in
  let null = Repr.Null Base.ignore and dec_bool = Repr.Bool Base.ignore in
  let dec_number = Repr.Number Base.ignore in
  let dec_string = Repr.String Base.ignore in
  let dec_array = Array.ignore and dec_object = Object.zero in
  let enc () = null in
  any ~kind ~dec_null:null ~dec_bool ~dec_number ~dec_string ~dec_array
    ~dec_object ~enc ()

let todo ?(kind = "") ?doc ?dec_stub () =
  let dec = match dec_stub with
  | Some v -> Fun.const v
  | None -> fun _v -> Error.decode_todo Meta.none ~kind_opt:kind
  in
  let enc _v = Error.encode_todo Meta.none ~kind_opt:kind in
  map ~kind ?doc ~dec ~enc ignore

(* Generic JSON *)

type name = string node
type mem = name * json
and object' = mem list
and json =
| Null of unit node
| Bool of bool node
| Number of float node
| String of string node
| Array of json list node
| Object of object' node

let pp_null = Fmt.json_null
let pp_bool = Fmt.json_bool
let pp_string = Fmt.json_string
let pp_number = Fmt.json_number
let pp_number' = Fmt.json_number'
let pp_json' ?(number_format = Fmt.json_default_number_format) () ppf j =
  let pp_indent = 2 in
  let pp_sep ppf () =
    Format.pp_print_char ppf ',';
    Format.pp_print_break ppf 1 pp_indent
  in
  let rec pp_array ppf a =
    Format.pp_open_hovbox ppf 0;
    Format.pp_print_char ppf '[';
    Format.pp_print_break ppf 0 pp_indent;
    (Format.pp_print_list ~pp_sep pp_value) ppf a;
    Format.pp_print_break ppf 0 0;
    Format.pp_print_char ppf ']';
    Format.pp_close_box ppf ()
  and pp_mem ppf ((m, _), v) =
    Format.pp_open_hvbox ppf 0;
    pp_string ppf m; Format.pp_print_string ppf ": "; pp_value ppf v;
    Format.pp_close_box ppf ();
  and pp_obj ppf o =
    Format.pp_open_hvbox ppf 0;
    Format.pp_print_char ppf '{';
    Format.pp_print_break ppf 0 pp_indent;
    (Format.pp_print_list ~pp_sep pp_mem) ppf o;
    Format.pp_print_break ppf 0 0;
    Format.pp_print_char ppf '}';
    Format.pp_close_box ppf ();
  and pp_value ppf = function
  | Null _ -> pp_null ppf ()
  | Bool (b,_ ) -> pp_bool ppf b
  | Number (f, _) -> pp_number' number_format ppf f
  | String (s, _) -> pp_string ppf s
  | Array (a, _) -> pp_array ppf a
  | Object (o, _) -> pp_obj ppf o
  in
  pp_value ppf j

let pp_json ppf j = pp_json' () ppf j

(* Generic JSON *)

module Json = struct
  type 'a cons = ?meta:Meta.t -> 'a -> json
  type t = json

  let meta = function
  | Null (_, m) -> m | Bool (_, m) -> m | Number (_, m) -> m
  | String (_, m) -> m | Array (_, m) -> m | Object (_, m) -> m

  let set_meta m = function
  | Null (v, _) -> Null (v, m) | Bool (v, _) -> Bool (v, m)
  | Number (v, _) -> Number (v, m) | String (v, _) -> String (v, m)
  | Array (v, _) -> Array (v, m) | Object (v, _) -> Object (v, m)

  let get_meta = meta
  let copy_layout v ~dst =
    set_meta (Meta.copy_ws (meta v) ~dst:(meta dst)) dst

  let sort = function
  | Null _ -> Sort.Null | Bool _ -> Sort.Bool | Number _ -> Sort.Number
  | String _ -> Sort.String | Array _ -> Sort.Array | Object _ -> Sort.Object

  let rec compare (j0 : json) (j1 : json) = match j0, j1 with
  | Null ((), _), Null ((), _) -> 0
  | Null _, _ -> -1 | _, Null _ -> 1
  | Bool (b0, _), Bool (b1, _) -> Bool.compare b0 b1
  | Bool _, _ -> -1 | _, Bool _ -> 1
  | Number (f0, _), Number (f1, _) -> Float.compare f0 f1
  | Number _, _ -> -1 | _, Number _ -> 1
  | String (s0, _), String (s1, _) -> String.compare s0 s1
  | String _, _ -> -1 | _, String _ -> 1
  | Array (a0, _), (Array (a1, _)) -> List.compare compare a0 a1
  | Array _, _ -> -1 | _, Array _ -> 1
  | Object (o0, _), Object (o1, _) ->
      let order_mem ((n0, _), _) ((n1, _), _) = String.compare n0 n1 in
      let compare_mem ((n0, _), j0) ((n1, _), j1) =
        let c = String.compare n0 n1 in
        if c = 0 then compare j0 j1 else c
      in
      List.compare compare_mem (List.sort order_mem o0) (List.sort order_mem o1)

  let equal j0 j1 = compare j0 j1 = 0
  let pp = pp_json

  (* Nulls and options *)

  let null' = Null ((), Meta.none)
  let null ?(meta = Meta.none) () = Null ((), meta)
  let option c ?meta = function None -> null ?meta () | Some v -> c ?meta v

  (* Booleans *)

  let bool ?(meta = Meta.none) b = Bool (b, meta)

  (* Numbers *)

  let number ?(meta = Meta.none) n = Number (n, meta)
  let any_float ?(meta = Meta.none) v =
    if Float.is_finite v
    then Number (v, meta)
    else String (Float.to_string v, meta)

  let int32 ?(meta = Meta.none) v = Number (Int32.to_float v, meta)
  let int64_as_string ?(meta = Meta.none) v = String (Int64.to_string v, meta)
  let int64 ?(meta = Meta.none) v =
    if Jsont_base.Number.can_store_exact_int64 v
    then Number (Int64.to_float v, meta)
    else String (Int64.to_string v, meta)

  let int_as_string ?(meta = Meta.none) i = String (Int.to_string i, meta)
  let int ?(meta = Meta.none) v =
    if Jsont_base.Number.can_store_exact_int v
    then Number (Int.to_float v, meta)
    else String (Int.to_string v, meta)

  (* Strings *)

  let string ?(meta = Meta.none) s = String (s, meta)

  (* Arrays *)

  let list ?(meta = Meta.none) l = Array (l, meta)
  let array ?(meta = Meta.none) a = Array (Stdlib.Array.to_list a, meta)
  let empty_array = list []

  (* Objects *)

  let name ?(meta = Meta.none) n = n, meta
  let mem n v = n, v
  let object' ?(meta = Meta.none) mems = Object (mems, meta)
  let empty_object = object' []

  let rec find_mem n = function
  | [] -> None
  | ((n', _), _ as m) :: ms ->
      if String.equal n n' then Some m else find_mem n ms

  let find_mem' (n, _) ms = find_mem n ms
  let object_names mems = List.map (fun ((n, _), _) -> n) mems
  let object_names' mems = List.map fst mems

  (* Zero *)

  let zero ?meta j = match sort j with
  | Null -> null ?meta () | Bool -> bool ?meta false
  | Number -> number ?meta 0. | String -> string ?meta ""
  | Array -> list ?meta [] | Object -> object' ?meta []

  (* Converting *)

  open Repr

  let error_sort ~exp j = Error.sort (meta j) ~exp ~fnd:(sort j)
  let error_type t fnd =
    Error.kinded_sort (meta fnd) ~exp:(kinded_sort t) ~fnd:(sort fnd)

  let find_all_unexpected ~mem_decs mems =
    let unexpected ((n, _ as nm), _v) =
      match Repr.String_map.find_opt n mem_decs with
      | None -> Some nm | Some _ -> None
    in
    List.filter_map unexpected mems

  (* Decoding *)

  let rec decode : type a. a Repr.t -> json -> a =
  fun t j -> match t with
  | Null map ->
      (match j with Null (n, meta) -> map.dec meta n | j -> error_type t j)
  | Bool map ->
      (match j with Bool (b, meta) -> map.dec meta b | j -> error_type t j)
  | Number map ->
      (match j with
      | Number (n, meta) -> map.dec meta n
      | Null (_, meta) -> map.dec meta Float.nan
      | j -> error_type t j)
  | String map ->
      (match j with String (s, meta) -> map.dec meta s | j -> error_type t j)
  | Array map ->
      (match j with
      | Array (vs, meta) -> decode_array map meta vs
      | j -> error_type t j)
  | Object map ->
      (match j with
      | Object (mems, meta) -> decode_object map meta mems
      | j -> error_type t j)
  | Map map -> map.dec (decode map.dom j)
  | Any map -> decode_any t map j
  | Rec t -> decode (Lazy.force t) j

  and decode_array :
    type a elt b. (a, elt, b) array_map -> Meta.t -> json list -> a
  =
  fun map meta vs ->
    let rec next (map : (a, elt, b) array_map) meta b i = function
    | [] -> map.dec_finish meta i b
    | v :: vs ->
        let b =
          try
            if map.dec_skip i b then b else
            map.dec_add i (decode map.elt v) b
          with Error e -> Repr.error_push_array meta map (i, get_meta v) e
        in
        next map meta b (i + 1) vs
    in
    next map meta (map.dec_empty ()) 0 vs

  and decode_object : type o. (o, o) Object.map -> Meta.t -> object' -> o =
  fun map meta mems ->
    let dict = Dict.empty in
    let umems = Unknown_mems None in
    apply_dict map.dec @@
    decode_object_map map meta umems String_map.empty String_map.empty dict mems

  and decode_object_map : type o.
    (o, o) Object.map -> Meta.t -> unknown_mems_option ->
    mem_dec String_map.t -> mem_dec String_map.t -> Dict.t -> object' -> Dict.t
  =
  fun map meta umems mem_miss mem_decs dict mems ->
    let u _ _ _ = assert false in
    let mem_miss = String_map.union u mem_miss map.mem_decs in
    let mem_decs = String_map.union u mem_decs map.mem_decs in
    match map.shape with
    | Object_cases (umems', cases) ->
        let umems' = Unknown_mems umems' in
        let umems, dict = Repr.override_unknown_mems ~by:umems umems' dict in
        decode_object_cases map meta umems cases mem_miss mem_decs dict [] mems
    | Object_basic umems' ->
        let umems' = Unknown_mems (Some umems') in
        let umems, dict = Repr.override_unknown_mems ~by:umems umems' dict in
        match umems with
        | Unknown_mems (Some Unknown_skip | None) ->
            let umems = Unknown_skip in
            decode_object_basic map meta umems () mem_miss mem_decs dict mems
        | Unknown_mems (Some (Unknown_error as umems)) ->
            decode_object_basic map meta umems () mem_miss mem_decs dict mems
        | Unknown_mems (Some (Unknown_keep (umap, _) as umems)) ->
            let umap = umap.dec_empty () in
            decode_object_basic map meta umems umap mem_miss mem_decs dict mems

  and decode_object_basic : type o p m b.
    (o, o) object_map -> Meta.t -> (p, m, b) unknown_mems -> b ->
    mem_dec String_map.t -> mem_dec String_map.t -> Dict.t -> object' -> Dict.t
  =
  fun map meta umems umap mem_miss mem_decs dict -> function
  | [] -> Repr.finish_object_decode map meta umems umap mem_miss dict
  | ((n, nmeta as nm), v) :: mems ->
      match String_map.find_opt n mem_decs with
      | Some (Mem_dec m) ->
          let dict = try Dict.add m.id (decode m.type' v) dict with
          | Error e -> Repr.error_push_object meta map nm e
          in
          let mem_miss = String_map.remove n mem_miss in
          decode_object_basic map meta umems umap mem_miss mem_decs dict mems
      | None ->
          match umems with
          | Unknown_skip ->
              decode_object_basic
                map meta umems umap mem_miss mem_decs dict mems
          | Unknown_error ->
              let fnd = nm :: find_all_unexpected ~mem_decs mems in
              Repr.unexpected_mems_error meta map ~fnd
          | Unknown_keep (umap', _) ->
              let umap =
                try umap'.dec_add nmeta n (decode umap'.mems_type v) umap with
                | Error e -> Repr.error_push_object meta map nm e
              in
              decode_object_basic
                map meta umems umap mem_miss mem_decs dict mems

  and decode_object_cases : type o cs t.
    (o, o) object_map -> Meta.t -> unknown_mems_option ->
    (o, cs, t) object_cases -> mem_dec String_map.t -> mem_dec String_map.t ->
    Dict.t -> object' -> object' -> Dict.t
  =
  fun map meta umems cases mem_miss mem_decs dict delay mems ->
    let decode_case_tag map meta tag delay mems =
      let eq_tag (Case c) = cases.tag_compare c.tag tag = 0 in
      match List.find_opt eq_tag cases.cases with
      | None -> Repr.unexpected_case_tag_error meta map cases tag
      | Some (Case case) ->
          let mems = List.rev_append delay mems in
          let dict =
            decode_object_map
              case.object_map meta umems mem_miss mem_decs dict mems
          in
          Dict.add
            cases.id (case.dec (apply_dict case.object_map.dec dict)) dict
    in
    match mems with
    | [] ->
        (match cases.tag.dec_absent with
        | Some tag -> decode_case_tag map meta tag delay []
        | None ->
            let kinded_sort = Repr.object_map_kinded_sort map in
            Error.missing_mems meta ~kinded_sort
              ~exp:[cases.tag.name]
              ~fnd:(List.map (fun ((n, _), _) -> n) delay))
    | ((n, meta as nm), v as mem) :: mems ->
        if n = cases.tag.name then
          let tag = try decode cases.tag.type' v with
          | Error e -> Repr.error_push_object meta map nm e
          in
          decode_case_tag map meta tag delay mems
        else
        match String_map.find_opt n mem_decs with
        | None ->
            let delay = mem :: delay in
            decode_object_cases
              map meta umems cases mem_miss mem_decs dict delay mems
        | Some (Mem_dec m) ->
            let dict = try Dict.add m.id (decode m.type' v) dict with
            | Error e -> Repr.error_push_object meta map nm e
            in
            let mem_miss = String_map.remove n mem_miss in
            decode_object_cases
              map meta umems cases mem_miss mem_decs dict delay mems

  and decode_any : type a. a Repr.t -> a any_map -> json -> a =
  fun t map j ->
    let dec t map j = match map with
    | Some t -> decode t j | None -> error_type t j
    in
    match j with
    | Null _ -> dec t map.dec_null j
    | Bool _ -> dec t map.dec_bool j
    | Number _ -> dec t map.dec_number j
    | String _ -> dec t map.dec_string j
    | Array _ -> dec t map.dec_array j
    | Object _ -> dec t map.dec_object j

  let dec = decode
  let decode' t j = try Ok (decode t j) with Error e -> Result.Error e
  let decode t j = Result.map_error Error.to_string (decode' t j)

  (* Encode *)

  let rec encode : type a. a Repr.t -> a -> json =
  fun t v -> match t with
  | Null map -> null ~meta:(map.enc_meta v) (map.enc v)
  | Bool map -> bool ~meta:(map.enc_meta v) (map.enc v)
  | Number map -> number ~meta:(map.enc_meta v) (map.enc v)
  | String map -> string ~meta:(map.enc_meta v) (map.enc v)
  | Array map ->
      let enc map acc i elt =
        try encode map.elt elt :: acc with
        | Error e -> Repr.error_push_array Meta.none map (i, Meta.none) e
      in
      list ~meta:(map.enc_meta v) (List.rev (map.enc (enc map) [] v))
  | Object map ->
      let mems = encode_object map ~do_unknown:true v [] in
      Object (List.rev mems, map.enc_meta v)
  | Any map -> encode (map.enc v) v
  | Map map -> encode map.dom (map.enc v)
  | Rec t -> encode (Lazy.force t) v

  and encode_object : type o dec.
    (o, o) object_map -> do_unknown:bool -> o -> object' -> object'
  =
  fun map ~do_unknown o obj ->
    let encode_mem map obj (Mem_enc mmap) =
      try
        let v = mmap.enc o in
        if mmap.enc_omit v then obj else
        ((mmap.name, Meta.none), encode mmap.type' v) :: obj
      with
      | Error e -> Repr.error_push_object Meta.none map (mmap.name, Meta.none) e
    in
    let obj = List.fold_left (encode_mem map) obj map.mem_encs in
    match map.shape with
    | Object_basic (Unknown_keep (umap, enc)) when do_unknown ->
        encode_unknown_mems map umap (enc o) obj
    | Object_basic _ -> obj
    | Object_cases (u, cases) ->
        let Case_value (case, c) = cases.enc_case (cases.enc o) in
        let obj =
          let n = cases.tag.name, Meta.none in
          try
            if cases.tag.enc_omit case.tag then obj else
            (n, encode cases.tag.type' case.tag) :: obj
          with
          | Error e -> Repr.error_push_object Meta.none map n e
        in
        match u with
        | Some (Unknown_keep (umap, enc)) ->
            (* Less T.R. but feels nicer to encode unknowns at the end *)
            let obj = encode_object case.object_map ~do_unknown:false c obj in
            encode_unknown_mems map umap (enc o) obj
        | _ -> encode_object case.object_map ~do_unknown c obj

   and encode_unknown_mems : type o dec mems a builder.
     (o, o) object_map -> (mems, a, builder) mems_map -> mems -> object' ->
     object'
   =
   fun map umap mems obj ->
     let encode_mem map meta name v obj =
       let n = (name, meta) in
       let v = try encode umap.mems_type v with
       | Error e -> Repr.error_push_object Meta.none map n e
       in
       (n, v) :: obj
     in
     (umap.enc (encode_mem map) mems obj)

   let enc = encode
   let encode' t v = try Ok (encode t v) with Error e -> Result.Error e
   let encode t v = Result.map_error Error.to_string (encode' t v)

   (* Recode *)

   let update t v = enc t (dec t v)
   let recode' t v = try Ok (update t v) with Error e -> Result.Error e
   let recode t v = Result.map_error Error.to_string (recode' t v)
end

let json_null =
  let dec meta () = Json.null ~meta () in
  let enc = function
  | Null ((), _) -> () | j -> Json.error_sort ~exp:Sort.Null j
  in
  Repr.Null (Base.map ~dec ~enc ~enc_meta:Json.meta ())

let json_bool =
  let dec meta b = Json.bool ~meta b in
  let enc = function
  | Bool (b, _) -> b | j -> Json.error_sort ~exp:Sort.Bool j
  in
  Repr.Bool (Base.map ~dec ~enc ~enc_meta:Json.meta ())

let json_number =
  let dec meta n = Json.number ~meta n in
  let enc = function
  | Number (n, _) -> n | j -> Json.error_sort ~exp:Sort.Number j
  in
  Repr.Number (Base.map ~dec ~enc ~enc_meta:Json.meta ())

let json_string =
  let dec meta s = Json.string ~meta s in
  let enc = function
  | String (s, _) -> s | j -> Json.error_sort ~exp:Sort.String j
  in
  Repr.String (Base.map ~dec ~enc ~enc_meta:Json.meta ())

let json, json_array, mem_list, json_object =
  let rec elt = Repr.Rec any
  and array_map = lazy begin
    let dec_empty () = [] in
    let dec_add _i v a = v :: a in
    let dec_finish meta _len a = Json.list ~meta (List.rev a) in
    let enc f acc = function
    | Array (a, _) -> Array.list_enc f acc a
    | j -> Json.error_sort ~exp:Sort.Array j
    in
    let enc = { Array.enc = enc } in
    Array.map ~dec_empty ~dec_add ~dec_finish ~enc ~enc_meta:Json.meta elt
  end

  and array = lazy (Array.array (Lazy.force array_map))
  and mems = lazy begin
    let dec_empty () = [] in
    let dec_add meta n v mems = ((n, meta), v) :: mems in
    let dec_finish _meta mems = List.rev mems in
    let enc f l a = List.fold_left (fun a ((n, m), v) -> f m n v a) a l in
    let enc = { Object.Mems.enc = enc } in
    Object.Mems.map ~dec_empty ~dec_add ~dec_finish ~enc elt
  end

  and object' = lazy begin
    let enc_meta = function
    | Object (_, meta) -> meta | j -> Json.error_sort ~exp:Sort.Object j
    in
    let enc = function
    | Object (mems, _) -> mems | j -> Json.error_sort ~exp:Sort.Object j
    in
    let dec meta mems = Object (mems, meta) in
    Object.map' dec ~enc_meta
    |> Object.keep_unknown (Lazy.force mems) ~enc
    |> Object.finish
  end

  and any = lazy begin
    let json_array = Lazy.force array in
    let json_object = Lazy.force object' in
    let enc = function
    | Null _ -> json_null | Bool _ -> json_bool
    | Number _ -> json_number | String _ -> json_string
    | Array _ -> json_array | Object _ -> json_object
    in
    Repr.Any { kind = "json"; doc = "";
               dec_null = Some json_null; dec_bool = Some json_bool;
               dec_number = Some json_number; dec_string = Some json_string;
               dec_array = Some json_array;
               dec_object = Some json_object; enc }
   end
  in
  Lazy.force any, Lazy.force array, Lazy.force mems, Lazy.force object'

let json_mems =
  let dec_empty () = [] in
  let dec_add meta name v mems = ((name, meta), v) :: mems in
  let dec_finish meta mems = Object (List.rev mems, meta) in
  let enc f j acc = match j with
  | Object (ms, _) -> List.fold_left (fun acc ((n, m), v) -> f m n v acc) acc ms
  | j -> Json.error_sort ~exp:Sort.Object j
  in
  let enc = { Object.Mems.enc = enc } in
  Object.Mems.map ~dec_empty ~dec_add ~dec_finish ~enc json

(* Queries and updates *)

(* val app : ('a -> 'b) t -> 'a t -> 'b t
   val product : 'a t -> 'b t -> ('a * 'b) t
   val bind : 'a t -> ('a -> 'b t) -> 'b t
   val map : ('a -> 'b) -> 'a t -> 'b t *)

let const t v =
  let const _ = v in
  let dec = map ~dec:const ignore in
  let enc = map ~enc:const t in
  let enc _v = enc in
  any ~dec_null:dec ~dec_bool:dec ~dec_number:dec ~dec_string:dec ~dec_array:dec
    ~dec_object:dec ~enc ()

let recode ~dec:dom f ~enc =
  let m = map ~dec:f dom in
  let enc _v = enc in
  any ~dec_null:m ~dec_bool:m ~dec_number:m ~dec_string:m ~dec_array:m
    ~dec_object:m ~enc ()

let update t =
  let dec v = Json.update t v in
  Repr.Map { kind = ""; doc = ""; dom = json; dec; enc = Fun.id }

(* Array queries *)

let rec list_repeat n v l = if n <= 0 then l else list_repeat (n - 1) v (v :: l)

let nth ?absent n t =
  let dec_empty () = None in
  let dec_skip i _v = i <> n in
  let dec_add _i v _acc = Some v in
  let dec_finish meta len v = match v with
  | Some v -> v
  | None ->
      match absent with
      | Some v -> v
      | None -> Error.index_out_of_range meta ~n ~len
  in
  let enc f acc v = f acc 0 v in
  let enc = { Array.enc } in
  Array.array (Array.map ~dec_empty ~dec_skip ~dec_add ~dec_finish ~enc t)

let update_nth ?stub ?absent n t =
  let update_elt n t v = Json.copy_layout v ~dst:(Json.update t v) in
  let rec update_array ~seen n t i acc = function
  | v :: vs when i = n ->
      let elt = update_elt (i, Json.meta v) t v in
      update_array ~seen:true n t (i + 1) (elt :: acc) vs
  | v :: vs -> update_array ~seen n t (i + 1) (v :: acc) vs
  | [] when seen -> Either.Right (List.rev acc)
  | [] -> Either.Left (acc, i)
  in
  let update ?stub ?absent n t j = match j with
  | Array (vs, meta) ->
      begin match update_array ~seen:false n t 0 [] vs with
      | Either.Right elts -> Array (elts, meta)
      | Either.Left (acc, len) ->
          match absent with
          | None -> Error.index_out_of_range meta ~n ~len
          | Some absent ->
              let elt = Json.enc t absent in
              let stub = match stub with
              | None -> Json.zero elt | Some j -> j
              in
              Array (List.rev (elt :: list_repeat (n - len) stub acc), meta)
      end
  | j -> Json.error_sort ~exp:Sort.Array j
  in
  let dec = update ?stub ?absent n t in
  let enc j = j in
  map ~dec ~enc json

let set_nth ?stub ?(allow_absent = false) t n v =
  let absent = if allow_absent then Some v else None in
  update_nth ?stub ?absent n (const t v)

let delete_nth ?(allow_absent = false) n =
  let dec_empty () = [] in
  let dec_add i v a = if i = n then a else (v :: a) in
  let dec_finish meta len a =
    if n < len || allow_absent then Json.list ~meta (List.rev a) else
    Error.index_out_of_range meta ~n ~len
  in
  let enc f acc = function
  | Array (a, _) -> Array.list_enc f acc a
  | j -> Json.error_sort ~exp:Sort.Array j
  in
  let enc_meta j = Json.meta j in
  let enc = { Array.enc = enc } in
  Array.array (Array.map ~dec_empty ~dec_add ~dec_finish ~enc ~enc_meta json)

let filter_map_array a b f =
  let dec_empty () = [] in
  let dec_add i v acc = match f i (Json.dec a v) with
  | None -> acc | Some v' -> (Json.enc b v') :: acc
  in
  let dec_finish meta _len acc = Json.list ~meta (List.rev acc) in
  let enc  f acc = function
  | Array (a, _) -> Array.list_enc f acc a
  | j -> Json.error_sort ~exp:Sort.Array j
  in
  let enc = { Array.enc = enc } in
  let enc_meta j = Json.meta j in
  Array.array (Array.map ~dec_empty ~dec_add ~dec_finish ~enc ~enc_meta json)

let fold_array t f acc =
  let dec_empty () = acc in
  let dec_add = f in
  let dec_finish _meta _len acc = acc in
  let enc _f acc _a = acc in
  let enc = { Array.enc = enc } in
  Array.array (Array.map ~dec_empty ~dec_add ~dec_finish ~enc t)

(* Object queries *)

let mem ?absent name t =
  Object.map Fun.id
  |> Object.mem name t ~enc:Fun.id ?dec_absent:absent
  |> Object.finish

let update_mem ?absent name t =
  let update_mem n t v = n, Json.copy_layout v ~dst:(Json.update t v) in
  let rec update_object ~seen name t acc = function
  | ((name', _ as n), v) :: mems when String.equal name name' ->
      update_object ~seen:true name t (update_mem n t v :: acc) mems
  | mem :: mems -> update_object ~seen name t (mem :: acc) mems
  | [] when seen -> Either.Right (List.rev acc)
  | [] -> Either.Left acc
  in
  let update ?absent name t = function
  | Object (mems, meta) ->
      let mems = match update_object ~seen:false name t [] mems with
      | Either.Right mems -> mems
      | Either.Left acc ->
          match absent with
          | None ->
              let fnd = Json.object_names mems in
              Error.missing_mems meta ~kinded_sort:"" ~exp:[name] ~fnd
          | Some absent ->
              let m = (name, Meta.none), Json.enc t absent in
              List.rev (m :: acc)
      in
      Object (mems, meta)
  | j -> Json.error_sort ~exp:Sort.Object j
  in
  let update = update ?absent name t in
  let enc j = j in
  map ~dec:update ~enc json

let set_mem ?(allow_absent = false) t name v =
  let absent = if allow_absent then Some v else None in
  update_mem ?absent name (const t v)

let update_json_object ~name ~dec_add ~dec_finish =
  let mems =
    let dec_empty () = false, [] in
    let enc f (_, l) a = List.fold_left (fun a ((n, m), v) -> f m n v a) a l in
    let enc = { Object.Mems.enc = enc } in
    Object.Mems.map ~dec_empty ~dec_add ~dec_finish ~enc json
  in
  let enc_meta = function
  | Object (_, meta) -> meta | j -> Json.error_sort ~exp:Sort.Object j
  in
  let enc = function
  | Object (mems, _) -> false, mems | j -> Json.error_sort ~exp:Sort.Object j
  in
  let dec meta (ok, mems) =
    let fnd = Json.object_names mems in
    if not ok
    then Error.missing_mems meta ~kinded_sort:"" ~exp:[name] ~fnd else
    Object (List.rev mems, meta)
  in
  Object.map' dec ~enc_meta
  |> Object.keep_unknown mems ~enc
  |> Object.finish

let delete_mem ?(allow_absent = false) name =
  let dec_add meta n v (ok, mems) =
    if n = name then true, mems else ok, ((n, meta), v) :: mems
  in
  let dec_finish _meta (ok, ms as a) = if allow_absent then (true, ms) else a in
  update_json_object ~name ~dec_add ~dec_finish

let fold_object t f acc =
  let mems =
    let dec_empty () = acc and dec_add = f and dec_finish _meta acc = acc in
    let enc f _ acc = acc in
    Object.Mems.map t ~dec_empty ~dec_add ~dec_finish ~enc:{ Object.Mems.enc }
  in
  Object.map Fun.id
  |> Object.keep_unknown mems ~enc:Fun.id
  |> Object.finish

let filter_map_object a b f =
  let dec_add meta n v (_, mems) =
    match f meta n (Json.dec a v) with
    | None -> (true, mems)
    | Some (n', v') -> (true, (n', (Json.enc b v')) :: mems)
  in
  let dec_finish _meta acc = acc in
  update_json_object ~name:"" (* irrelevant *) ~dec_add ~dec_finish

(* Index queries *)

let index ?absent i t = match i with
| Path.Nth (n, _) -> nth ?absent n t
| Path.Mem (n, _) -> mem ?absent n t

let set_index ?allow_absent t i v = match i with
| Path.Nth (n, _) -> set_nth ?allow_absent t n v
| Path.Mem (n, _) -> set_mem ?allow_absent t n v

let update_index ?stub ?absent i t = match i with
| Path.Nth (n, _) -> update_nth ?stub ?absent n t
| Path.Mem (n, _) -> update_mem ?absent n t

let delete_index ?allow_absent = function
| Path.Nth (n, _) -> delete_nth ?allow_absent n
| Path.Mem (n, _) -> delete_mem ?allow_absent n

(* Path queries *)

let path ?absent p q =
  List.fold_left (fun q i -> index ?absent i q) q (Path.rev_indices p)

let update_path ?stub ?absent p t = match Path.rev_indices p with
| [] -> update t
| i :: is ->
    match absent with
    | None ->
        let update t i = update_index i t in
        List.fold_left update (update_index i t) is
    | Some absent ->
        let rec loop absent t = function
        | Path.Nth (n, _) :: is ->
            loop Json.empty_array (update_nth ~absent n t) is
        | Path.Mem (n, _) :: is ->
            loop Json.empty_object (update_mem ~absent n t) is
        | [] -> t
        in
        match i with
        | Path.Nth (n, _) ->
            loop Json.empty_array (update_nth ?stub ~absent n t) is
        | Path.Mem (n, _) ->
            loop Json.empty_object (update_mem ~absent n t) is

let delete_path ?allow_absent p = match Path.rev_indices p with
| [] -> recode ~dec:ignore (fun () -> Json.null') ~enc:json
| i :: is ->
    let upd del i = update_index i del in
    List.fold_left upd (delete_index ?allow_absent i) is

let set_path ?stub ?(allow_absent = false) t p v = match Path.rev_indices p with
| [] -> recode ~dec:ignore (fun () -> Json.enc t v) ~enc:json
| i :: is ->
    let absent = if allow_absent then Some v else None in
    update_path ?stub ?absent p (const t v)

(* Formatting *)

type format = Minify | Indent | Layout
type number_format = Fmt.json_number_format
let default_number_format = Fmt.json_default_number_format
let pp_value ?number_format t () = fun ppf v -> match Json.encode t v with
| Ok j ->  pp_json' ?number_format () ppf j
| Error e -> pp_string ppf e
