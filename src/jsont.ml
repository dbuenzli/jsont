(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module String_set = Set.Make (String)

type 'a fmt = 'a Jsont_base.Fmt.t
module Fmt = Jsont_base.Fmt

module Textloc = Jsont_base.Textloc
module Meta = Jsont_base.Meta
type 'a node = 'a * Meta.t

module Path = struct
  type index = Mem of string node | Nth of int node
  let pp_name ppf (n, _) = Fmt.code ppf n
  let pp_index ppf = function
  | Mem n -> pp_name ppf n | Nth (n, _) -> Fmt.pf ppf "[%d]" n

  let pp_index_trace ppf = function
  | Mem (_, meta as n) ->
      Fmt.pf ppf "%a: in member %a" Textloc.pp (Meta.textloc meta) pp_name n
  | Nth (n, meta) ->
      Fmt.pf ppf "%a: at index %d" Textloc.pp (Meta.textloc meta) n

  let pp_bracketed_index ppf = function
  | Mem n -> Fmt.pf ppf "[%a]" pp_name n
  | Nth (n, _) -> Fmt.pf ppf "[%d]" n

  type t = index list
  let root = []
  let is_root = function [] -> true | _ -> false
  let nth ?(meta = Meta.none) n p = Nth (n, meta) :: p
  let mem ?(meta = Meta.none) n p = Mem (n, meta) :: p
  let rev_indices p = p

  let pp ppf is =
    let pp_sep ppf () = Fmt.char ppf '.' in
    Fmt.list ~pp_sep pp_index ppf (List.rev is)

  let pp_trace ppf is =
    if is <> [] then Fmt.pf ppf "@,@[<v>%a@]" (Fmt.list pp_index_trace) is

  let none = []
  let err i fmt = Format.kasprintf failwith ("%d: " ^^ fmt) i
  let err_unexp_eoi i = err i "Unexpected end of input"
  let err_unexp_char i s = err i "Unexpected character: %C" s.[i]
  let err_illegal_char i s = err i "Illegal character here: %C" s.[i]
  let err_unexp i s =
    err i "Unexpected input: %S" (Jsont_base.string_subrange ~first:i s)

  let parse_eoi s i max = if i > max then () else err_unexp i s
  let parse_index p s i max =
    let first, stop = match s.[i] with '[' -> i + 1, ']' | _ -> i, '.' in
    let last, next =
      let rec loop stop s i max = match i > max with
      | true -> if stop = ']' then err_unexp_eoi i else (i - 1), i
      | false ->
          let illegal = s.[i] = '[' || (s.[i] = ']' && stop = '.') in
          if illegal then err_illegal_char i s else
          if s.[i] <> stop then loop stop s (i + 1) max else
          (i - 1), if stop = ']' then i + 1 else i
      in
      loop stop s first max
    in
    let idx = Jsont_base.string_subrange ~first ~last s in
    if idx = "" then err first "illegal empty index" else
    match int_of_string idx with
    | exception Failure _ -> next, (Mem (idx, Meta.none)) :: p
    | idx -> next, (Nth (idx, Meta.none)) :: p

  let of_string s =
    let rec loop p s i max =
      if i > max then p else
      let next, p = parse_index p s i max in
      if next > max then p else
      if s.[next] <> '.' then err_unexp_char next s else
      if next + 1 <= max then loop p s (next + 1) max else
      err_unexp_eoi next
    in
    try
      if s = "" then Ok [] else
      let start = if s.[0] = '.' then 1 else 0 in
      Ok (loop [] s start (String.length s - 1))
    with Failure e -> Error e
end

module Context = struct
  (* Note we could make this more subtle and push array and object
     type kinds to show up in error traces. *)
  type t = Path.t
  let root = []
  let make = Fun.id
  let path = Fun.id
  let is_root = Path.is_root
  let push_nth nth ctx = (Path.Nth nth) :: ctx
  let push_mem mem ctx = (Path.Mem mem) :: ctx
end

module Sort = struct
  type t = Null | Bool | Number | String | Array | Obj
  let to_string = function
  | Null -> "null" | Bool -> "boolean" | Number -> "number"
  | String  -> "string" | Array  -> "array" | Obj -> "object"

  let pp ppf s = Fmt.code ppf (to_string s)
end

type error_kind = string
type error = Context.t * Meta.t * error_kind
exception Error of error

module Error = struct
  type kind = error_kind
  type t = error
  let make_msg ctx meta msg = ctx, meta, msg
  let msg ctx meta msg = raise (Error (ctx, meta, msg))
  let msgf ctx meta fmt = Format.kasprintf (fun m -> msg ctx meta m) fmt

  let pp ppf (ctx, m, msg) =
    let pp_meta ppf m =
      if Meta.is_none m then () else
      Fmt.pf ppf "@,%a" Textloc.pp (Meta.textloc m)
    in
    Fmt.pf ppf "@[<v>%a%a%a@]"
      (Fmt.list Fmt.string) (String.split_on_char '\n' msg)
      pp_meta m Path.pp_trace ctx

  let to_string e = Format.asprintf "@[%a@]" pp e

  let sort ctx meta ~exp ~fnd =
    msgf ctx meta "Expected %a but found %a" Sort.pp exp Sort.pp fnd

  let kind ctx meta ~exp ~fnd =
    msgf ctx meta "Expected %a but found %a" Fmt.code exp Sort.pp fnd

  let missing_mems ctx meta ~obj_kind ~exp ~fnd =
    let pp_miss ppf m =
      Fmt.pf ppf "@[%a%a@]" Fmt.code m Fmt.similar_mems (m, fnd)
    in
    match exp with
    | [n] ->
        msgf ctx meta "@[<v>Missing member %a in %a%a@]"
          Fmt.code n Fmt.code obj_kind Fmt.similar_mems (n, fnd)
    | exp ->
        msgf ctx meta "@[<v1>Missing members in %a:@,%a@]"
          Fmt.code obj_kind (Fmt.list pp_miss) exp

  let unexpected_mems ctx meta ~obj_kind ~exp ~fnd =
    let pp_unexp ppf m =
      Fmt.pf ppf " @[%a%a@]" Fmt.code m Fmt.should_it_be_mem (m, exp)
    in
    match fnd with
    | [(u, _)] -> (* TODO use the name metas *)
        msgf ctx meta "@[<v>Unexpected member %a for %a%a@]"
          Fmt.code u Fmt.code obj_kind Fmt.should_it_be_mem (u, exp)
    | us ->
        msgf ctx meta "@[<v1>Unexpected members for %a:@,%a@]"
          Fmt.code obj_kind (Fmt.list pp_unexp) (List.map fst us)

  let unexpected_case_tag ctx meta ~obj_kind ~mem_name ~exp ~fnd =
    let pp_kind ppf () =
      Fmt.pf ppf "member %a value in %a" Fmt.code mem_name Fmt.code obj_kind
    in
    msgf ctx meta "@[%a@]" (Fmt.out_of_dom ~pp_kind ()) (fnd, exp)

  let out_of_range ctx meta ~n ~len =
    let s = Int.to_string in
    msgf ctx meta "Index %a out of range [%a;%a]"
      Fmt.code (s n) Fmt.code (s 0) Fmt.code (s (len - 1))
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
    dec : Context.t -> Meta.t -> 'a -> 'b;
    enc : Context.t -> 'b -> 'a;
    enc_meta : Context.t -> 'b -> Meta.t; }

  type 'a t =
  | Null : (unit, 'a) base_map -> 'a t
  | Bool : (bool, 'a) base_map -> 'a t
  | Number : (float, 'a) base_map -> 'a t
  | String : (string, 'a) base_map -> 'a t
  | Array : ('a, 'elt, 'builder) array_map -> 'a t
  | Obj : ('o, 'o) obj_map -> 'o t
  | Any : 'a any_map -> 'a t
  | Map : ('a, 'b) map -> 'b t
  | Rec : 'a t Lazy.t -> 'a t

  and ('array, 'elt, 'builder) array_map =
  { kind : string;
    doc : string;
    elt : 'elt t;
    dec_empty : Context.t -> 'builder;
    dec_skip : Context.t -> int -> 'builder -> bool;
    dec_add : Context.t -> int -> 'elt -> 'builder -> 'builder;
    dec_finish : Context.t -> Meta.t -> int -> 'builder -> 'array;
    enc :
      'acc. Context.t -> ('acc -> int -> 'elt -> 'acc) -> 'acc -> 'array ->
      'acc;
    enc_meta : Context.t -> 'array -> Meta.t; }

  and ('o, 'dec) obj_map =
  { kind : string;
    doc : string;
    dec : ('o, 'dec) dec_fun;
    mem_decs : mem_dec String_map.t;
    mem_encs : 'o mem_enc list;
    enc_meta : Context.t -> 'o -> Meta.t;
    shape : 'o obj_shape; }

  and mem_dec = Mem_dec : ('o, 'a) mem_map -> mem_dec
  and 'o mem_enc = Mem_enc : ('o, 'a) mem_map -> 'o mem_enc
  and ('o, 'a) mem_map =
  { name : string;
    doc : string;
    type' : 'a t;
    id : 'a Type.Id.t;
    dec_absent : 'a option;
    enc : Context.t -> 'o -> 'a;
    enc_meta : Context.t -> 'a -> Meta.t;
    enc_omit : Context.t -> 'a -> bool; }

  and 'o obj_shape =
  | Obj_basic : ('o, 'mems, 'builder) unknown_mems -> 'o obj_shape
  | Obj_cases :
      ('o, 'mems, 'builder) unknown_mems option *
      ('o, 'cases, 'tag) obj_cases -> 'o obj_shape

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
    dec_empty : Context.t -> 'builder;
    dec_add : Context.t -> Meta.t -> string -> 'a -> 'builder -> 'builder;
    dec_finish : Context.t -> 'builder -> 'mems;
    enc :
      'acc. Context.t ->
      (Meta.t -> string -> 'a -> 'acc -> 'acc) -> 'mems -> 'acc -> 'acc }

  and ('o, 'cases, 'tag) obj_cases =
  { tag : ('tag, 'tag) mem_map;
    tag_compare : 'tag -> 'tag -> int;
    tag_to_string : ('tag -> string) option;
    id : 'cases Type.Id.t;
    cases : ('cases, 'tag) case list;
    enc : 'o -> 'cases;
    enc_case : Context.t -> 'cases -> ('cases, 'tag) case_value; }

  and ('cases, 'case, 'tag) case_map =
  { tag : 'tag;
    obj_map : ('case, 'case) obj_map;
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
    dec_obj : 'a t option;
    enc : Context.t -> 'a -> 'a t; }

  and ('a, 'b) map =
  { kind : string;
    doc : string;
    dom : 'a t;
    dec : Context.t -> 'a -> 'b;
    enc : Context.t -> 'b -> 'a; }

  let of_t = Fun.id
  let unsafe_to_t = Fun.id

  let type_kind ~kind ~type' = Printf.sprintf "%s<%s>" type' kind
  let sort_kind ~kind ~sort =
    if kind = "" then sort else Printf.sprintf "%s %s" kind sort

  let null_kind ~kind = sort_kind ~kind ~sort:"null"
  let bool_kind ~kind = sort_kind ~kind ~sort:"bool"
  let string_kind ~kind = sort_kind ~kind ~sort:"string"
  let number_kind ~kind = sort_kind ~kind ~sort:"number"
  let obj_kind ~kind = sort_kind ~kind ~sort:"object"

  let mems_map_kind (map : (_, _, _) mems_map) =
    sort_kind ~kind:map.kind ~sort:"members"

  let obj_map_kind (map : (_, _) obj_map) = obj_kind ~kind:map.kind

  let rec value_kind : type a. a t -> string = function
  | Null map -> null_kind ~kind:map.kind
  | Bool map -> bool_kind ~kind:map.kind
  | Number map -> number_kind ~kind:map.kind
  | String map -> string_kind ~kind:map.kind
  | Array map -> array_kind ~kind:map.kind map.elt
  | Obj map -> obj_map_kind map
  | Any map -> if map.kind = "" then any_map_kind map else map.kind
  | Map map -> if map.kind = "" then value_kind map.dom else map.kind
  | Rec l -> value_kind (Lazy.force l)

  and array_kind : type a. kind:string -> a t -> string = fun ~kind:k t ->
    sort_kind ~kind:k ~sort:(type_kind ~kind:(value_kind t) ~type':"array")

  and any_map_kind : type a. a any_map -> string = fun map ->
    let add_case ks sort = function
    | None -> ks
    | Some k ->
        (if map.kind <> ""
         then value_kind k else sort_kind ~kind:map.kind ~sort)
        :: ks
    in
    let ks = add_case [] "object" map.dec_obj in
    let ks = add_case ks "array" map.dec_array in
    let ks = add_case ks "string" map.dec_string in
    let ks = add_case ks "number" map.dec_number in
    let ks = add_case ks "boolean" map.dec_bool in
    let ks = add_case ks "null" map.dec_null in
    "one of " ^ String.concat ", " ks

  let rec kind : type a. a t -> string = function
  | Null map -> if map.kind = "" then "null" else map.kind
  | Bool map -> if map.kind = "" then "bool" else map.kind
  | Number map -> if map.kind = "" then "number" else map.kind
  | String map -> if map.kind = "" then "string" else map.kind
  | Array map -> if map.kind = "" then "array" else map.kind
  | Obj map -> if map.kind = "" then "object" else map.kind
  | Any map -> if map.kind = "" then "any" else map.kind
  | Map map -> if map.kind = "" then kind map.dom else map.kind
  | Rec l -> kind (Lazy.force l)

  let rec doc : type a. a t -> string = function
  | Null map -> map.doc | Bool map -> map.doc | Number map -> map.doc
  | String map -> map.doc | Array map -> map.doc | Obj map -> map.doc
  | Any map -> map.doc | Map map -> map.doc | Rec l -> doc (Lazy.force l)

  let type_error ctx meta t ~fnd = Error.kind ctx meta ~exp:(value_kind t) ~fnd

  let missing_mems_error ctx meta (obj_map : ('o, 'o) obj_map) ~exp ~fnd =
    let obj_kind = obj_map_kind obj_map in
    let exp =
      let add n (Mem_dec m) acc = match m.dec_absent with
      | None -> n :: acc | Some _ -> acc
      in
      List.rev (String_map.fold add exp [])
    in
    Error.missing_mems ctx meta ~obj_kind ~exp ~fnd

  let unexpected_mems_error ctx meta (obj_map : ('o, 'o) obj_map) ~fnd =
    let obj_kind = obj_map_kind obj_map in
    let exp = List.map (fun (Mem_enc m) -> m.name) obj_map.mem_encs in
    Error.unexpected_mems ctx meta ~obj_kind ~exp ~fnd

  let unexpected_case_tag_error ctx meta obj_map obj_cases tag =
    let obj_kind = obj_map_kind obj_map in
    let case_to_string (Case c) = match obj_cases.tag_to_string with
    | None -> None | Some str -> Some (str c.tag)
    in
    let exp = List.filter_map case_to_string obj_cases.cases in
    let fnd = match obj_cases.tag_to_string with
    | None -> "<value>" (* FIXME not good *) | Some str -> str tag
    in
    let mem_name = obj_cases.tag.name in
    Error.unexpected_case_tag ctx meta ~obj_kind ~mem_name ~exp ~fnd

  let push_mem (m : ('o, 'dec) obj_map) n meta ctx =
    Context.push_mem (n, meta) ctx

  let push_mem' (m : ('o, 'dec) obj_map) n ctx =
    Context.push_mem n ctx

  let push_nth (m : ('array, 'elt, 'builder) array_map) i meta ctx =
    Context.push_nth (i, meta) ctx

  let pp_code = Fmt.code
  let pp_kind = Fmt.code

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

  let obj_context_arg : Context.t Type.Id.t = Type.Id.make ()
  let obj_meta_arg : Meta.t Type.Id.t = Type.Id.make ()

  type unknown_mems_option =
  | Unknown_mems :
      ('o, 'mems, 'builder) unknown_mems option -> unknown_mems_option


  let override_unknown_mems ctx ~by umems dict = match by with
  | Unknown_mems None -> umems, dict
  | Unknown_mems _ as by ->
      match umems with
      | Unknown_mems (Some (Unknown_keep (umap, _))) ->
          (* A decoding function still expect [umap.id] argument in
             an Dec_app, we simply stub it with the empty map. *)
          let empty = umap.dec_finish ctx (umap.dec_empty ctx) in
          let dict =  Dict.add umap.id empty dict in
          by, dict
      | _ -> by, dict
end

include Repr

let pp_kind = Fmt.code
let for_kind ppf = function "" -> () | k -> Fmt.pf ppf " for %a" pp_kind k
let dec_none k ctx m _ = Error.msgf ctx m "No decoder%a" for_kind k
let enc_none k ctx _ = Error.msgf ctx Meta.none "No encoder%a" for_kind k
let enc_meta_none ctx _ = Meta.none
let enc_ignore ctx _ = Error.msg ctx Meta.none "Decoding ignored, cannot encode"

module Base = struct
  type ('a, 'b) map = ('a, 'b) base_map

  let map
      ?(kind = "") ?(doc = "") ?(dec = dec_none kind) ?(enc = enc_none kind)
      ?(enc_meta = enc_meta_none) ()
    =
    { kind; doc; dec; enc; enc_meta }

  let id =
    let dec _ _ v = v and enc _ v = v in
    { kind = ""; doc = ""; dec; enc; enc_meta = enc_meta_none }

  let ignore =
    let dec _ _ _ = () and enc = enc_ignore in
    { kind = "ignored"; doc = ""; dec; enc; enc_meta = enc_meta_none }

  let null map = Null map
  let bool map = Bool map
  let number map = Number map
  let string map = String map

  let error ctx meta kind e =
    let pp_kind_header ppf k =
      if k = "" then () else Fmt.pf ppf "%a: " Repr.pp_kind k
    in
    Error.msgf ctx meta "%a%s" pp_kind_header kind e

  let dec dec _ctx _meta v = dec v
  let dec_result ?(kind = "") dec ctx meta v = match dec v with
  | Ok v -> v | Error e -> error ctx meta kind e

  let dec_failure ?(kind = "") dec ctx meta v = try dec v with
  | Failure e -> error ctx meta kind e

  let enc enc _ctx v = enc v
  let enc_failure ?(kind = "") enc ctx v = try enc v with
  | Failure e -> error ctx Meta.none kind e

  let enc_result ?(kind = "") enc ctx v = match enc v with
  | Ok v -> v | Error e -> error ctx Meta.none kind e
end

module Array = struct
  type ('array, 'elt, 'builder) map = ('array, 'elt, 'builder) array_map
  type ('array, 'elt) enc =
    { enc :
        'acc. Context.t -> ('acc -> int -> 'elt -> 'acc) -> 'acc -> 'array ->
        'acc }

  let default_skip _ _ _ = false
  let map
      ?(kind = "") ?(doc = "") ~dec_empty ?dec_skip ~dec_add ~dec_finish
      ~enc:{enc} ?(enc_meta = enc_meta_none) elt
    =
    let dec_skip = Option.value ~default:default_skip dec_skip in
    { kind; doc; elt; dec_empty; dec_add; dec_skip; dec_finish; enc; enc_meta; }

  let list_enc _ctx f acc l =
    let rec loop f acc i = function
    | [] -> acc | v :: l -> loop f (f acc i v) (i + 1) l
    in
    loop f acc 0 l

  let list_map ?kind ?doc ?dec_skip elt =
    let dec_empty _ctx = [] in
    let dec_add _ctx _i v l = v :: l in
    let dec_finish _ctx _meta _len l = List.rev l in
    let enc = { enc = list_enc } in
    map ?kind ?doc ~dec_empty ?dec_skip ~dec_add ~dec_finish ~enc elt

  type 'a array_builder = 'a Jsont_base.Rarray.t

  let array_enc _ctx f acc a =
    let acc = ref acc in
    for i = 0 to Array.length a - 1
    do acc := f !acc i (Array.unsafe_get a i) done;
    !acc

  let array_map ?kind ?doc ?dec_skip elt =
    let dec_empty _ctx = Jsont_base.Rarray.empty () in
    let dec_add _ctx _i v a = Jsont_base.Rarray.add_last v a in
    let dec_finish _ctx _meta _len a = Jsont_base.Rarray.to_array a in
    let enc = { enc = array_enc } in
    map ?kind ?doc ~dec_empty ?dec_skip ~dec_add ~dec_finish ~enc elt

  type ('a, 'b, 'c) bigarray_builder = ('a, 'b, 'c) Jsont_base.Rbigarray1.t

  let bigarray_map ?kind ?doc ?dec_skip k l elt =
    let dec_empty _meta = Jsont_base.Rbigarray1.empty k l in
    let dec_add _ctx _i v a = Jsont_base.Rbigarray1.add_last v a in
    let dec_finish _ctx _meta _len a = Jsont_base.Rbigarray1.to_bigarray a in
    let enc _ctx f acc a =
      let acc = ref acc in
      for i = 0 to Bigarray.Array1.dim a - 1
      do acc := f !acc i (Bigarray.Array1.unsafe_get a i) done;
      !acc
    in
    let enc = { enc } in
    map ?kind ?doc ~dec_empty ?dec_skip ~dec_add ~dec_finish ~enc elt

  let array map = Array map

  let ignore =
    let dec_empty _ctx = () in
    let dec_add _ctx _i _v () = () in
    let dec_skip _ctx _i () = true in
    let dec_finish _ctx _meta _len () = () in
    let stub =
      Map { kind = ""; doc = "";
            dom = Base.(null id);
            enc = (fun _ -> assert false);
            dec = (fun _ -> assert false); }
    in
    let enc ctx _f _acc () = Error.msg ctx Meta.none "no encoder specified" in
    let enc = { enc } in
    let kind = "ignored" in
    array (map ~kind ~dec_empty ~dec_skip ~dec_add ~dec_finish ~enc stub)
end

module Obj = struct
  module Mem = struct
    type nonrec ('o, 'dec) obj_map = ('o, 'dec) obj_map
    type ('o, 'a) map = ('o, 'a) mem_map

    let noenc name = fun ctx _v ->
      Error.msgf ctx Meta.none "No encoder for member %a" Fmt.code name

    let map ?(doc = "") ?dec_absent ?enc ?enc_meta ?enc_omit name type' =
      let id = Type.Id.make () in
      let enc = match enc with
      | None -> noenc name | Some enc -> fun _ctx o -> enc o
      in
      let enc_omit = match enc_omit with
      | None -> fun _ctx _v -> false | Some omit -> fun _ctx v -> omit v
      in
      let enc_meta = match enc_meta with
      | None -> enc_meta_none | Some enc_meta -> fun _ctx v -> enc_meta v
      in
      { name; doc; type'; id; dec_absent; enc; enc_omit; enc_meta }

    let app obj_map mm =
      let mem_decs = String_map.add mm.name (Mem_dec mm) obj_map.mem_decs in
      let mem_encs = Mem_enc mm :: obj_map.mem_encs in
      let dec = Dec_app (obj_map.dec, mm.id) in
      { obj_map with dec; mem_decs; mem_encs }
  end

  type ('o, 'dec) map = ('o, 'dec) obj_map

  let kind = Repr.obj_map_kind
  let map ?(kind = "") ?(doc = "") dec =
    { kind; doc; dec = Dec_fun dec; mem_decs = String_map.empty;
      mem_encs = []; enc_meta = enc_meta_none; shape = Obj_basic Unknown_skip }

  let map' ?(kind = "") ?(doc = "") ?(enc_meta = enc_meta_none) dec =
    let dec = Dec_app (Dec_app (Dec_fun dec, obj_context_arg), obj_meta_arg) in
    { kind; doc; dec; mem_decs = String_map.empty;
      mem_encs = []; enc_meta; shape = Obj_basic Unknown_skip }

  let enc_only ?(kind = "") ?(doc = "") ?(enc_meta = enc_meta_none) () =
    let dec ctx m =
      let kind = if kind = "" then "object" else kind in
      Error.msg ctx m ("No decoder for " ^ kind)
    in
    let dec = Dec_app (Dec_app (Dec_fun dec, obj_context_arg), obj_meta_arg) in
    { kind; doc; dec; mem_decs = String_map.empty;
      mem_encs = []; enc_meta; shape = Obj_basic Unknown_skip }

  let check_name_unicity m =
    let add n kind = function
    | None -> Some kind
    | Some kind' ->
        let ks k = if k = "" then "<object>" else k in
        invalid_arg @@
        Fmt.str "member %s defined both in %s and %s" n (ks kind) (ks kind')
    in
    let rec loop : type o dec. string String_map.t -> (o, dec) obj_map -> unit =
    fun names m ->
      let add_name names n = String_map.update n (add n m.kind) names in
      let add_mem_enc names (Mem_enc m) = add_name names m.name in
      let names = List.fold_left add_mem_enc names m.mem_encs in
      match m.shape with
      | Obj_basic _ -> ()
      | Obj_cases (u, cases) ->
          let names = add_name names cases.tag.name in
          let check_case (Case c) = loop names c.obj_map in
          List.iter check_case cases.cases
    in
    loop String_map.empty m

  let rev_mem_encs m = { m with mem_encs = List.rev m.mem_encs }
  let finish mems =
    let () = check_name_unicity mems in
    Obj (rev_mem_encs mems)

  let unfinish = function
  | Obj map -> rev_mem_encs map | _ -> invalid_arg "Not an object"

  let mem ?(doc = "") ?dec_absent ?enc ?enc_meta ?enc_omit name type' obj_map =
    let mm =
      Mem.map ~doc ?dec_absent ?enc ?enc_meta ?enc_omit name type'
    in
    let mem_decs = String_map.add name (Mem_dec mm) obj_map.mem_decs in
    let mem_encs = Mem_enc mm :: obj_map.mem_encs in
    let dec = Dec_app (obj_map.dec, mm.id) in
    { obj_map with dec; mem_decs; mem_encs }

  let opt_mem ?doc ?enc name dom map =
    let some =
      let dec _ v = Option.some v in
      let enc _ v = Option.get v in
      Map { kind = ""; doc = ""; dom; dec; enc }
    in
    mem ?doc ~dec_absent:None ?enc ~enc_omit:Option.is_none name some map

  module Case = struct
    type ('cases, 'case, 'tag) map = ('cases, 'case, 'tag) case_map
    type ('cases, 'tag) t = ('cases, 'tag) case
    type ('cases, 'tag) value = ('cases, 'tag) case_value

    let case_no_dec _ = Error.msg [] Meta.none "case decode unspecified"
    let map ?(dec = case_no_dec) tag obj =
      let obj_map = unfinish obj in
      { tag; obj_map = rev_mem_encs obj_map; dec; }

    let make c = Case c
    let value c v = Case_value (c, v)
  end

  let case_mem
      ?(doc = "") ?(tag_compare = Stdlib.compare) ?tag_to_string ?dec_absent
      ?enc ?enc_omit ?enc_case name type' cases obj_map
    =
    (* TODO check dec_absent has a case to avoid puzzling decoding errors. *)
    let () = match obj_map.shape with
    | Obj_cases _ -> invalid_arg "Multiple calls to Jsont.Obj.case_mem"
    | _ -> ()
    in
    let tag =
      let id = Type.Id.make () in
      let enc _ t = t (* N.B. this fact may be used by encoders. *) in
      let enc_omit = match enc_omit with
      | None -> fun _ctx _v -> false | Some omit -> fun _ctx v -> omit v
      in
      let enc_meta = enc_meta_none in
      { name; doc; type'; id; dec_absent; enc; enc_omit; enc_meta }
    in
    let enc_case = match enc_case with
    | None -> fun ctx -> Error.msg ctx Meta.none "no case encoder"
    | Some enc_case -> fun _ctx c -> enc_case c
    in
    let enc = match enc with
    | None -> fun _ -> Error.msg Context.root Meta.none "no encoder specified"
    | Some enc -> enc
    in
    let id = Type.Id.make () in
    let shape =
      Obj_cases
        (None, { tag; tag_compare; tag_to_string; id; cases; enc; enc_case })
    in
    let dec = Dec_app (obj_map.dec, id) in
    { obj_map with dec; shape }

  module Mems = struct
    type ('mems, 'a, 'builder) map = ('mems, 'a, 'builder) mems_map
    type ('mems, 'a) enc =
      { enc :
          'acc. Context.t -> (Meta.t -> string -> 'a -> 'acc -> 'acc) ->
          'mems -> 'acc -> 'acc }

    let map
        ?(kind = "") ?(doc = "") mems_type ~dec_empty ~dec_add ~dec_finish
        ~enc:{enc}
      =
      let id = Type.Id.make () in
      { kind; doc; mems_type; id; dec_empty; dec_add; dec_finish; enc }

    let string_map ?kind ?doc type' =
      let dec_empty _ctx = String_map.empty in
      let dec_add _ctx _meta n v m = String_map.add n v m in
      let dec_finish _ctx = Fun.id in
      let enc ctx f m acc =
        String_map.fold (fun n v acc -> f Meta.none n v acc) m acc
      in
      map ?kind ?doc type' ~dec_empty ~dec_add ~dec_finish ~enc:{enc}
  end

  let set_shape_unknown_mems shape u = match shape with
  | Obj_basic (Unknown_keep _) | Obj_cases (Some (Unknown_keep _), _) ->
      invalid_arg "Jsont.Obj.keep_unknown already called on object"
  | Obj_basic _ -> Obj_basic u
  | Obj_cases (_, cases) -> Obj_cases (Some u, cases)

  let skip_unknown obj_map =
    { obj_map with shape = set_shape_unknown_mems obj_map.shape Unknown_skip }

  let error_unknown obj_map =
    { obj_map with shape = set_shape_unknown_mems obj_map.shape Unknown_error }

  let mems_noenc msm _o =
    Error.msg Context.root Meta.none ("No encoder for" ^ (mems_map_kind msm))

  let keep_unknown ?enc msm (obj_map : ('o, 'dec) obj_map) =
    let enc = match enc with None -> mems_noenc msm | Some enc -> enc in
    let dec = Dec_app (obj_map.dec, msm.id) in
    let unknown = Unknown_keep (msm, enc) in
    { obj_map with dec; shape = set_shape_unknown_mems obj_map.shape unknown }

  let ignore = finish (map ~kind:"ignored" ())

  let as_string_map ?kind ?doc t =
    map ?kind ?doc Fun.id
    |> keep_unknown (Mems.string_map t) ~enc:Fun.id
    |> finish
end

let any
    ?(kind = "") ?(doc = "") ?dec_null ?dec_bool ?dec_number ?dec_string
    ?dec_array ?dec_obj ?enc ()
  =
  let enc = match enc with
  | Some enc -> enc
  | None ->
      fun ctx _ ->
        Error.msgf ctx Meta.none "No encoding type specified%a" for_kind kind
  in
  Any { kind; doc; dec_null; dec_bool; dec_number; dec_string; dec_array;
        dec_obj; enc }

let map ?(kind = "") ?(doc = "") ?dec ?enc dom =
  let dec = match dec with
  | Some dec -> dec
  | None -> fun ctx _ -> Error.msgf ctx Meta.none "No decoder%a" for_kind kind
  in
  let enc = match enc with
  | Some enc -> enc
  | None -> fun ctx _ -> Error.msgf ctx Meta.none "No encoder%a" for_kind kind
  in
  Map { kind; doc; dom; dec; enc; }

let rec' t = Rec t

(* Ignoring *)

let ignore =
  let kind = "ignored" in
  let enc = enc_none kind in
  let dec_null = Null Base.ignore and dec_bool = Bool Base.ignore in
  let dec_number = Number Base.ignore and dec_string = String Base.ignore in
  let dec_array = Array.ignore and dec_obj = Obj.ignore in
  any ~kind ~dec_null ~dec_bool ~dec_number ~dec_string ~dec_array ~dec_obj ~enc
    ()

let todo ?(kind = "") ?doc ?dec_stub () =
  let dec_none ctx =
    Error.msgf ctx Meta.none "Decoder%a is todo" for_kind kind
  in
  let dec = match dec_stub with
  | None -> dec_none | Some v -> fun ctx () -> v
  in
  let enc ctx _ = Error.msgf ctx Meta.none "Encoder%a is todo" for_kind kind in
  map ~kind ?doc ~dec ~enc ignore

(* Base types *)

let null ?kind ?doc v =
  let dec _ _ () = v and enc _ _ = () in
  Null (Base.map ?doc ?kind ~dec ~enc ())

let bool = Bool Base.id
let number = Number Base.id
let string = String Base.id

(* Option *)

let none =
  let none = (* Can't use [Base.map] because of the value restriction. *)
    let dec _ _ _ = None and enc _ _ = () in
    { kind = ""; doc = ""; dec; enc; enc_meta = enc_meta_none }
  in
  Null none

let some t =
  map ~dec:(fun _ v -> Option.some v) ~enc:(fun _ v -> Option.get v) t

let option ?kind ?doc t =
  let some = some t in
  let enc _ = function None -> none | _ -> some in
  match t with
  | Null _ -> any ?doc ?kind ~dec_null:none ~enc ()
  | Bool _ -> any ?doc ?kind ~dec_null:none ~dec_bool:some ~enc ()
  | Number _ -> any ?doc ?kind ~dec_null:none ~dec_number:some ~enc ()
  | String _ -> any ?doc ?kind ~dec_null:none ~dec_string:some ~enc ()
  | Array _ -> any ?doc ?kind ~dec_null:none ~dec_array:some ~enc ()
  | Obj _ -> any ?doc ?kind ~dec_null:none ~dec_obj:some ~enc ()
  | (Any _ | Map _ | Rec _) ->
      any ?doc ?kind ~dec_null:none ~dec_bool:some ~dec_number:some
        ~dec_string:some ~dec_array:some ~dec_obj:some ~enc ()

(* Integers *)

let[@inline] check_num_finite ctx meta ~kind v =
  if Float.is_finite v then () else
  let kind = Repr.number_kind ~kind in
  Error.kind ctx meta ~exp:kind ~fnd:Sort.Null

let err_num_range ctx meta ~kind n =
  Error.msgf ctx meta "Number %a not in expected %a range"
    Fmt.code (Fmt.str "%a" Fmt.json_number n) Fmt.code kind

let err_str_num_parse ctx meta ~kind s =
  Error.msgf ctx meta "String %a: does not parse to %a value"
    Fmt.json_string s Fmt.code kind

let err_num_enc_range ctx ~kind n =
  Error.msgf ctx Meta.none "Integer %a not in expected %a range"
    Fmt.code (Fmt.str "%d" n) Fmt.code kind

let int_as_string =
  let kind = "OCaml int" in
  let dec ctx meta v = match int_of_string_opt v with
  | Some v -> v | None -> err_str_num_parse ctx meta ~kind v
  in
  let enc _ v = Int.to_string v in
  Base.string (Base.map ~kind ~dec ~enc ())

let int_number =
  (* Usage by [int] entails there's no need to test for nan or check range on
     encoding. *)
  let kind = "OCaml int" in
  let dec ctx meta v =
    if Jsont_base.Number.in_exact_int_range v then Int.of_float v else
    err_num_range ctx meta ~kind v
  in
  let enc _ v = Int.to_float v in
  Base.number (Base.map ~kind ~dec ~enc ())

let int =
  let enc _ v =
    if Jsont_base.Number.can_store_exact_int v then int_number else
    int_as_string
  in
  let dec_number = int_number and dec_string = int_as_string in
  any ~kind:"OCaml int" ~dec_number ~dec_string ~enc ()

let uint8 =
  let kind = "uint8" in
  let dec ctx meta v =
    check_num_finite ctx meta ~kind v;
    if Jsont_base.Number.in_exact_uint8_range v then Int.of_float v else
    err_num_range ctx meta ~kind v
  in
  let enc ctx v =
    if Jsont_base.Number.int_is_uint8 v then Int.to_float v else
    err_num_enc_range ctx ~kind v
  in
  Base.number (Base.map ~kind ~dec ~enc ())

let uint16 =
  let kind = "uint16" in
  let dec ctx meta v =
    check_num_finite ctx meta ~kind v;
    if Jsont_base.Number.in_exact_uint16_range v then Int.of_float v else
    err_num_range ctx meta ~kind v
  in
  let enc ctx v =
    if Jsont_base.Number.int_is_uint16 v then Int.to_float v else
    err_num_enc_range ctx ~kind v
  in
  Base.number (Base.map ~kind ~dec ~enc ())

let int8 =
  let kind = "int8" in
  let dec ctx meta v =
    check_num_finite ctx meta ~kind v;
    if Jsont_base.Number.in_exact_int8_range v then Int.of_float v else
    err_num_range ctx meta ~kind v
  in
  let enc ctx v =
    if Jsont_base.Number.int_is_int8 v then Int.to_float v else
    err_num_enc_range ctx ~kind v
  in
  Base.number (Base.map ~kind ~dec ~enc ())

let int16 =
  let kind = "int16" in
  let dec ctx meta v =
    check_num_finite ctx meta ~kind v;
    if Jsont_base.Number.in_exact_int16_range v then Int.of_float v else
    err_num_range ctx meta ~kind v
  in
  let enc ctx v =
    if Jsont_base.Number.int_is_int16 v then Int.to_float v else
    err_num_enc_range ctx ~kind v
  in
  Base.number (Base.map ~kind ~dec ~enc ())

let int32 =
  let kind = "int32" in
  let dec ctx meta v =
    check_num_finite ctx meta ~kind v;
    if Jsont_base.Number.in_exact_int32_range v then Int32.of_float v else
    err_num_range ctx meta ~kind v
  in
  let enc _ v = Int32.to_float v (* Everything always fits *)  in
  Base.number (Base.map ~kind ~dec ~enc ())

let int64_as_string =
  let kind = "int64" in
  let dec ctx meta v = match Int64.of_string_opt v with
  | Some v -> v | None -> err_str_num_parse ctx meta ~kind v
  in
  let enc _ v = Int64.to_string v in
  Base.string (Base.map ~kind ~dec ~enc ())

let int64_number =
  (* Usage by [int64] entails there's no need to test for nan or check
     range on encoding. *)
  let kind = "int64" in
  let dec ctx meta v =
    if Jsont_base.Number.in_exact_int64_range v then Int64.of_float v else
    err_num_range ctx meta ~kind v
  in
  let enc _ v = Int64.to_float v in
  Base.number (Base.map ~kind ~dec ~enc ())

let int64 =
  let dec_number = int64_number and dec_string = int64_as_string in
  let enc _ v =
    if Jsont_base.Number.can_store_exact_int64 v then int64_number else
    int64_as_string
  in
  any ~kind:"int64" ~dec_number ~dec_string ~enc ()

(* Floats *)

let any_float =
  let kind = "float" in
  let finite = number in
  let non_finite =
    let dec ctx m v = match Float.of_string_opt v with
    | Some v -> v | None -> err_str_num_parse ctx m ~kind v
    in
    let enc ctx v = Float.to_string v in
    Base.string (Base.map ~kind ~dec ~enc ())
  in
  let enc ctx v = if Float.is_finite v then finite else non_finite in
  any ~kind:"float" ~dec_null:finite ~dec_number:finite
    ~dec_string:non_finite ~enc ()

let float_as_hex_string =
  let kind = "float" in
  let dec ctx meta v = match Float.of_string_opt v with
  | Some v -> v | None -> err_str_num_parse ctx meta ~kind v
  in
  let enc ctx v = Printf.sprintf "%h" v in
  Base.string (Base.map ~kind ~dec ~enc ())

(* Strings *)

let of_of_string ?kind ?doc ?enc of_string =
  let enc = match enc with
  | None -> None | Some enc -> Some (fun ctx s -> enc s)
  in
  let dec ctx meta s = match of_string s with
  | Ok v -> v
  | Error e -> Error.msg ctx meta e
  in
  Base.string (Base.map ?kind ?doc ?enc ~dec ())

let enum (type a) ?(cmp = Stdlib.compare) ?(kind = "") ?doc assoc =
  let kind = Repr.sort_kind ~kind ~sort:"enum" in
  let dec_map =
    let add m (k, v) = String_map.add k v m in
    let m = List.fold_left add String_map.empty assoc in
    fun k -> String_map.find_opt k m
  in
  let enc_map =
    let module M = Map.Make (struct type t = a let compare = cmp end) in
    let add m (k, v) = M.add v k m in
    let m = List.fold_left add M.empty assoc in
    fun v -> M.find_opt v m
  in
  let enc ctx v = match enc_map v with
  | None -> Error.msg ctx Meta.none "Unknown enum value"
  | Some s -> s
  in
  let dec ctx m s = match dec_map s with
  | Some v -> v
  | None ->
      let kind = Repr.string_kind ~kind in
      let pp_kind ppf () = Fmt.pf ppf "%a value" Repr.pp_kind kind in
      Error.msgf ctx m "%a" Fmt.(out_of_dom ~pp_kind ()) (s, List.map fst assoc)
  in
  Base.string (Base.map ~kind ?doc ~dec ~enc ())

let binary_string =
  let kind = "hex" in
  let kind' = Repr.string_kind ~kind in
  let dec = Base.dec_result ~kind:kind' Jsont_base.binary_string_of_hex in
  let enc = Base.enc Jsont_base.binary_string_to_hex in
  Base.string (Base.map ~kind ~dec ~enc ())

(* Arrays *)

let list ?kind ?doc t = Array (Array.list_map ?kind ?doc t)
let array ?kind ?doc t = Array (Array.array_map ?kind ?doc t)
let array_as_string_map ?kind ?doc ~key t =
  let dec_empty _ = String_map.empty in
  let dec_add _ _ elt acc = String_map.add (key elt) elt acc in
  let dec_finish _ _ _ acc = acc in
  let enc ctx f acc m =
    let i = ref (-1) in
    String_map.fold (fun _ elt acc -> incr i; f acc !i elt) m acc
  in
  let enc = Array.{enc} in
  let map = Array.map ?kind ?doc ~dec_empty ~dec_add ~dec_finish ~enc t in
  Array map

let bigarray ?kind ?doc k t =
  Array (Array.bigarray_map ?kind ?doc k Bigarray.c_layout t)

(* Uniform tuples *)

let tuple_dec_none ctx meta =
  Error.msg ctx meta "Tuple decoder unspecified"

let tuple_enc_none ctx _f _acc _v =
  Error.msg ctx Meta.none "Tuple encoder unspecified"

let error_tuple_size ctx meta kind ~exp fnd =
  Error.msgf ctx meta "Expected %a elements in %a but found %a"
    Fmt.code (Int.to_string exp) Fmt.code kind
    Fmt.code (Int.to_string fnd)

let t2 ?(kind = "") ?doc ?dec ?enc t =
  let size = 2 in
  let dec = match dec with
  | None -> fun ctx meta v0 v1 -> tuple_dec_none ctx meta
  | Some dec -> fun _ctx _meta v0 v1 -> dec v0 v1
  in
  let dec_empty ctx = [] in
  let dec_add ctx _ v acc = v :: acc in
  let dec_finish ctx meta _len = function
  | [v1; v0] -> dec ctx meta v0 v1
  | l -> error_tuple_size ctx meta kind ~exp:size (List.length l)
  in
  let enc = match enc with
  | None -> tuple_enc_none
  | Some enc -> fun ctx f acc v -> f (f acc 0 (enc v 0)) 1 (enc v 1)
  in
  let enc = { Array.enc } in
  Array (Array.map ~kind ?doc ~dec_empty ~dec_add ~dec_finish ~enc t)

let t3 ?(kind = "") ?doc ?dec ?enc t =
  let size = 3 in
  let dec = match dec with
  | None -> fun ctx meta v0 v1 v2 -> tuple_dec_none ctx meta
  | Some dec -> fun _ctx _meta v0 v1 v2 -> dec v0 v1 v2
  in
  let dec_empty ctx = [] in
  let dec_add ctx _ v acc = v :: acc in
  let dec_finish ctx meta _len = function
  | [v2; v1; v0] -> dec ctx meta v0 v1 v2
  | l -> error_tuple_size ctx meta kind ~exp:size (List.length l)
  in
  let enc = match enc with
  | None -> tuple_enc_none
  | Some enc ->
      fun ctx f acc v -> f (f (f acc 0 (enc v 0)) 1 (enc v 1)) 2 (enc v 2)
  in
  let enc = { Array.enc } in
  Array (Array.map ~kind ?doc ~dec_empty ~dec_add ~dec_finish ~enc t)

let t4 ?(kind = "") ?doc ?dec ?enc t =
  let size = 4 in
  let dec = match dec with
  | None -> fun ctx meta v0 v1 v2 v3 -> tuple_dec_none ctx meta
  | Some dec -> fun _ctx _meta v0 v1 v2 v3 -> dec v0 v1 v2 v3
  in
  let dec_empty ctx = [] in
  let dec_add ctx _ v acc = v :: acc in
  let dec_finish ctx meta _len = function
  | [v3; v2; v1; v0] -> dec ctx meta v0 v1 v2 v3
  | l -> error_tuple_size ctx meta kind ~exp:size (List.length l)
  in
  let enc = match enc with
  | None -> tuple_enc_none
  | Some enc ->
      fun ctx f acc v ->
        f (f (f (f acc 0 (enc v 0)) 1 (enc v 1)) 2 (enc v 2)) 3 (enc v 3)
  in
  let enc = { Array.enc } in
  Array (Array.map ~kind ?doc ~dec_empty ~dec_add ~dec_finish ~enc t)

let tn ?(kind = "") ?doc ~n elt =
  let dec_empty _ctx = Jsont_base.Rarray.empty () in
  let dec_add _ctx _i v a = Jsont_base.Rarray.add_last v a in
  let dec_finish ctx meta _len a =
    let len = Jsont_base.Rarray.length a in
    if len <> n then error_tuple_size ctx meta kind ~exp:n len else
    Jsont_base.Rarray.to_array a
  in
  let enc = { Array.enc = Array.array_enc } in
  Array (Array.map ~kind ?doc ~dec_empty ~dec_add ~dec_finish ~enc elt)

(* Context *)

(* TODO I think we could get meta aswell, at least in context. *)

let context = map ~dec:(fun ctx () -> ctx) ignore
let with_context t = map ~dec:(fun ctx v -> v, ctx) ~enc:(fun _ (v, ctx) -> v) t

(* Generic JSON *)

type name = string node
type mem = name * json
and obj = mem list
and json =
| Null of unit node
| Bool of bool node
| Number of float node
| String of string node
| Array of json list node
| Obj of obj node

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
  | Obj (o, _) -> pp_obj ppf o
  in
  pp_value ppf j

let pp_json ppf j = pp_json' () ppf j

module Json = struct
  let meta = function
  | Null (_, m) -> m | Bool (_, m) -> m | Number (_, m) -> m
  | String (_, m) -> m | Array (_, m) -> m | Obj (_, m) -> m

  let sort = function
  | Null _ -> Sort.Null | Bool _ -> Sort.Bool | Number _ -> Sort.Number
  | String _ -> Sort.String | Array _ -> Sort.Array | Obj _ -> Sort.Obj

  let rec find_mem n = function
  | [] -> None
  | ((n', _), _ as m) :: ms ->
      if String.equal n n' then Some m else find_mem n ms

  let find_mem' (n, _) ms = find_mem n ms

  let obj_names mems = List.map (fun ((n, _), _) -> n) mems
  let obj_names' mems = List.map fst mems

  (* Constructors *)

  type 'a cons = ?meta:Meta.t -> 'a -> json

  let null' = Null ((), Meta.none)
  let null ?(meta = Meta.none) () = Null ((), meta)
  let bool ?(meta = Meta.none) b = Bool (b, meta)
  let number ?(meta = Meta.none) n = Number (n, meta)
  let string ?(meta = Meta.none) s = String (s, meta)
  let array ?(meta = Meta.none) a = Array (Stdlib.Array.to_list a, meta)
  let name ?(meta = Meta.none) n = n, meta
  let mem n v = n, v
  let obj ?(meta = Meta.none) mems = Obj (mems, meta)
  let option c ?meta = function None -> null ?meta () | Some v -> c ?meta v
  let list ?(meta = Meta.none) l = Array (l, meta)

  let int ?(meta = Meta.none) v =
    if Jsont_base.Number.can_store_exact_int v
    then Number (Int.to_float v, meta)
    else String (Int.to_string v, meta)

  let int32 ?(meta = Meta.none) v = Number (Int32.to_float v, meta)
  let int64 ?(meta = Meta.none) v =
    if Jsont_base.Number.can_store_exact_int64 v
    then Number (Int64.to_float v, meta)
    else String (Int64.to_string v, meta)

  let int_as_string ?(meta = Meta.none) i = String (Int.to_string i, meta)
  let int64_as_string ?(meta = Meta.none) v = String (Int64.to_string v, meta)

  let any_float ?(meta = Meta.none) v =
    if Float.is_finite v
    then Number (v, meta)
    else String (Float.to_string v, meta)

  let stub ?meta j = match sort j with
  | Null -> null ?meta () | Bool -> bool ?meta false
  | Number -> number ?meta 0. | String -> string ?meta ""
  | Array -> list ?meta [] | Obj -> obj ?meta []

  let empty_array = list []
  let empty_obj = obj []

  (* Errors *)

  (* FIXME move to repr ? *)

  let error_sort ctx ~exp j = Error.sort ctx (meta j) ~exp ~fnd:(sort j)
  let error_type ctx t fnd =
      Error.kind ctx (meta fnd) ~exp:(value_kind t) ~fnd:(sort fnd)

  let find_all_unexpected ~mem_decs mems =
    let unexpected ((n, _ as nm), _v) =
      match String_map.find_opt n mem_decs with
      | None -> Some nm | Some _ -> None
    in
    List.filter_map unexpected mems

  (* Decoding *)

  let rec decode : type a. Context.t -> a Repr.t -> json -> a =
  fun ctx t j -> match t with
  | Null map ->
      (match j with Null (n, m) -> map.dec ctx m n | j -> error_type ctx t j)
  | Bool map ->
      (match j with Bool (b, m) -> map.dec ctx m b | j -> error_type ctx t j)
  | Number map ->
      (match j with
      | Number (n, m) -> map.dec ctx m n
      | Null (_, m) -> map.dec ctx m Float.nan
      | j -> error_type ctx t j)
  | String map ->
      (match j with String (s, m) -> map.dec ctx m s | j -> error_type ctx t j)
  | Array map ->
      (match j with
      | Array (vs, m) -> decode_array ctx m map vs
      | j -> error_type ctx t j)
  | Obj map ->
      (match j with
      | Obj (mems, m) -> decode_obj ctx m map mems
      | j -> error_type ctx t j)
  | Map map -> map.dec ctx (decode ctx map.dom j)
  | Any map -> decode_any ctx t map j
  | Rec t -> decode ctx (Lazy.force t) j

  and decode_array :
    type a elt b. Context.t -> Meta.t -> (a, elt, b) array_map -> json list -> a
  =
  fun ctx meta map vs ->
    let rec next ctx (map : (a, elt, b) array_map) b i = function
    | [] -> map.dec_finish ctx meta i b
    | v :: vs ->
        let b =
          if map.dec_skip ctx i b then b else
          let ctxi = Repr.push_nth map i Meta.none ctx in
          map.dec_add ctx i (decode ctxi map.elt v)  b
        in
        next ctx map b (i + 1) vs
    in
    next ctx map (map.dec_empty ctx) 0 vs

  and decode_obj : type o. Context.t -> Meta.t -> (o, o) Obj.map -> obj -> o =
    fun ctx meta map mems ->
    let dict = Dict.empty in
    let umems = Unknown_mems None in
    apply_dict map.dec @@
    decode_obj_map
      ctx meta map umems String_map.empty String_map.empty dict mems

  and decode_obj_map : type o.
    Context.t -> Meta.t -> (o, o) Obj.map -> unknown_mems_option ->
    mem_dec String_map.t -> mem_dec String_map.t ->
    Dict.t -> obj -> Dict.t
  =
  fun ctx meta map umems mem_miss mem_decs dict mems ->
    let u _ _ _ = assert false in
    let mem_miss = String_map.union u mem_miss map.mem_decs in
    let mem_decs = String_map.union u mem_decs map.mem_decs in
    let dict = match map.shape with
    | Obj_cases (umems', cases) ->
        let umems, dict =
          Repr.override_unknown_mems ctx ~by:umems (Unknown_mems umems') dict
        in
        decode_obj_cases ctx map umems cases mem_miss mem_decs dict [] mems
    | Obj_basic umems' ->
        let umems, dict =
          Repr.override_unknown_mems
            ctx ~by:umems (Unknown_mems (Some umems')) dict
        in
        match umems with
        | Unknown_mems (Some Unknown_skip | None) ->
            decode_obj_basic
              ctx meta map Unknown_skip () mem_miss mem_decs dict mems
        | Unknown_mems (Some (Unknown_error as u)) ->
            decode_obj_basic ctx meta map u () mem_miss mem_decs dict mems
        | Unknown_mems (Some (Unknown_keep (umap, _) as u)) ->
            let umap = umap.dec_empty ctx in
            decode_obj_basic ctx meta map u umap mem_miss mem_decs dict mems
    in
    (* XXX !? Note the order is important here. Sub case object maps
       add their own context and meta we need to override these. *)
    let dict = Dict.add obj_context_arg ctx dict in
    let dict = Dict.add obj_meta_arg meta dict in
    dict

  and decode_obj_basic : type o p m b.
    Context.t -> Meta.t -> (o, o) obj_map -> (p, m, b) unknown_mems -> b ->
    mem_dec String_map.t -> mem_dec String_map.t -> Dict.t -> obj -> Dict.t
  =
  fun ctx meta map u umap mem_miss mem_decs dict -> function
  | [] ->
      let dict = match u with
      | Unknown_skip | Unknown_error -> dict
      | Unknown_keep (map, _) ->
          Dict.add map.id (map.dec_finish ctx umap) dict
      in
      let add_default _ (Mem_dec m) dict = match m.dec_absent with
      | Some v -> Dict.add m.id v dict
      | None -> raise Exit
      in
      (try String_map.fold add_default mem_miss dict with
      | Exit ->
          let exp = mem_miss and fnd = [] in
          Repr.missing_mems_error ctx meta map ~exp ~fnd)
  | ((n, nmeta as nm), v) :: mems ->
      match String_map.find_opt n mem_decs with
      | Some (Mem_dec m) ->
          let dict =
            let ctx = Repr.push_mem' map nm ctx in
            Dict.add m.id (decode ctx m.type' v) dict
          in
          let mem_miss = String_map.remove n mem_miss in
          decode_obj_basic ctx meta map u umap mem_miss mem_decs dict mems
      | None ->
          match u with
          | Unknown_skip ->
              decode_obj_basic ctx meta map u umap mem_miss mem_decs dict mems
          | Unknown_error ->
              let fnd = nm :: find_all_unexpected ~mem_decs mems in
              Repr.unexpected_mems_error ctx meta map ~fnd
          | Unknown_keep (umap', _) ->
              let umap =
                let ctx = Repr.push_mem' map nm ctx in
                umap'.dec_add ctx nmeta n (decode ctx umap'.mems_type v) umap
              in
              decode_obj_basic ctx meta map u umap mem_miss mem_decs dict mems

  and decode_obj_cases : type o cs t.
    Context.t -> (o, o) obj_map ->
    unknown_mems_option -> (o, cs, t) obj_cases ->
    mem_dec String_map.t -> mem_dec String_map.t -> Dict.t -> obj -> obj ->
    Dict.t =
  fun ctx map umems cases mem_miss mem_decs dict delay mems ->
    let decode_case_tag tag mems =
      let eq_tag (Case c) = cases.tag_compare c.tag tag = 0 in
      match List.find_opt eq_tag cases.cases with
      | None ->
          let meta = Meta.none in (* FIXME *)
          Repr.unexpected_case_tag_error ctx meta map cases tag
      | Some (Case case) ->
          let mems = List.rev_append delay mems in
          let meta = Meta.none in (* FIXME *)
          let dict =
            decode_obj_map
              ctx meta case.obj_map umems mem_miss mem_decs dict mems
          in
          Dict.add cases.id (case.dec (apply_dict case.obj_map.dec dict)) dict
    in
    match mems with
    | [] ->
        (match cases.tag.dec_absent with
        | Some t -> decode_case_tag t []
        | None ->
            let meta = Meta.none in (* FIXME *)
            let obj_kind = Repr.obj_kind ~kind:map.kind in
            Error.missing_mems ctx meta ~obj_kind
              ~exp:[cases.tag.name]
              ~fnd:(List.map (fun ((n, _), _) -> n) delay))
    | ((n, meta as nm), v as mem) :: mems ->
        if n = cases.tag.name then
          let ctx = Repr.push_mem' map nm ctx in
          decode_case_tag (decode ctx cases.tag.type' v) mems
        else
        match String_map.find_opt n mem_decs with
        | None ->
            let delay = mem :: delay in
            decode_obj_cases
              ctx map umems cases mem_miss mem_decs dict delay mems
        | Some (Mem_dec m) ->
            let dict =
              let ctx = Repr.push_mem' map nm ctx in
              Dict.add m.id (decode ctx m.type' v) dict
            in
            let mem_miss = String_map.remove n mem_miss in
            decode_obj_cases
              ctx map umems cases mem_miss mem_decs dict delay mems

  and decode_any : type a. Context.t -> a Repr.t -> a any_map -> json -> a =
  fun ctx t map j ->
    let dec ctx t map j = match map with
    | Some t -> decode ctx t j | None -> error_type ctx t j
    in
    match j with
    | Null _ -> dec ctx t map.dec_null j
    | Bool _ -> dec ctx t map.dec_bool j
    | Number _ -> dec ctx t map.dec_number j
    | String _ -> dec ctx t map.dec_string j
    | Array _ -> dec ctx t map.dec_array j
    | Obj _ -> dec ctx t map.dec_obj j

  let decode' ?(ctx = Context.root) t j =
    try Ok (decode ctx t j) with Error e -> Result.Error e

  let dec = decode
  let decode ?ctx t j = Result.map_error Error.to_string (decode' ?ctx t j)

  (* Encode *)

  let rec encode : type a. Context.t -> a Repr.t -> a -> json =
  fun ctx t v -> match t with
  | Null map -> null ~meta:(map.enc_meta ctx v) (map.enc ctx v)
  | Bool map -> bool ~meta:(map.enc_meta ctx v) (map.enc ctx v)
  | Number map -> number ~meta:(map.enc_meta ctx v) (map.enc ctx v)
  | String map -> string ~meta:(map.enc_meta ctx v) (map.enc ctx v)
  | Array map ->
      let enc map acc i v =
        encode (Repr.push_nth map i Meta.none ctx) map.elt v :: acc
      in
      list ~meta:(map.enc_meta ctx v) (List.rev (map.enc ctx (enc map) [] v))
  | Obj map ->
      let mems = encode_obj ctx map ~do_unknown:true v [] in
      Obj (List.rev mems, map.enc_meta ctx v)
  | Any map -> encode ctx (map.enc ctx v) v
  | Map map -> encode ctx map.dom (map.enc ctx v)
  | Rec t -> encode ctx (Lazy.force t) v

  and encode_obj :
    type o dec.
    Context.t -> (o, o) obj_map -> do_unknown:bool -> o -> obj -> obj
  =
  fun ctx map ~do_unknown o obj ->
    let encode_mem map obj (Mem_enc mmap) =
      let ctx = Repr.push_mem map mmap.name Meta.none ctx in
      let v = mmap.enc ctx o in
      if mmap.enc_omit ctx v then obj else
      (((mmap.name, mmap.enc_meta ctx v), encode ctx mmap.type' v) :: obj)
    in
    let obj = List.fold_left (encode_mem map) obj map.mem_encs in
    match map.shape with
    | Obj_basic (Unknown_keep (umap, enc)) when do_unknown ->
        encode_unknown_mems ctx map umap (enc o) obj
    | Obj_basic _ -> obj
    | Obj_cases (u, cases) ->
        let Case_value (case, c) = cases.enc_case ctx (cases.enc o) in
        let obj =
          let n = cases.tag.name, Meta.none in
          let ctx = Repr.push_mem' map n ctx in
          if cases.tag.enc_omit ctx case.tag then obj else
          (n, encode ctx cases.tag.type' case.tag) :: obj
        in
        match u with
        | Some (Unknown_keep (umap, enc)) ->
            (* Feels nicer to encode unknowns at the end *)
            let obj = encode_obj ctx case.obj_map ~do_unknown:false c obj in
            encode_unknown_mems ctx map umap (enc o) obj
        | _ -> encode_obj ctx case.obj_map ~do_unknown c obj

   and encode_unknown_mems : type o dec mems a builder.
     Context.t -> (o, o) obj_map -> (mems, a, builder) mems_map -> mems ->
     obj -> obj =
   fun ctx map umap mems obj ->
     let encode_mem map meta name v obj =
       let ctx = Repr.push_mem map name Meta.none ctx in
       let n = (name, meta) in
       (n, encode ctx umap.mems_type v) :: obj
     in
     (umap.enc ctx (encode_mem map) mems obj)

  let encode' ?(ctx = Context.root) t v =
    try Ok (encode ctx t v) with Error e -> Result.Error e

  let enc = encode
  let encode ?ctx t v = Result.map_error Error.to_string (encode' ?ctx t v)

  let update ctx t v = enc ctx t (dec ctx t v)

  (* Standard module interface *)

  type t = json

  let rec compare j0 j1 = match j0, j1 with
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
  | Obj (o0, _), Obj (o1, _) ->
      let order_mem ((n0, _), _) ((n1, _), _) = String.compare n0 n1 in
      let compare_mem ((n0, _), j0) ((n1, _), j1) =
        let c = String.compare n0 n1 in
        if c = 0 then compare j0 j1 else c
      in
      List.compare compare_mem (List.sort order_mem o0) (List.sort order_mem o1)

  let equal j0 j1 = compare j0 j1 = 0
  let pp = pp_json
end

let enc_meta _ctx j = Json.meta j

let json_null =
  let dec _ctx meta () = Json.null ~meta () in
  let enc ctx = function
  | Null ((), _) -> () | j -> Json.error_sort ctx ~exp:Sort.Null j
  in
  Repr.Null (Base.map ~dec ~enc ~enc_meta ())

let json_bool =
  let dec _ctx meta b = Json.bool ~meta b in
  let enc ctx = function
  | Bool (b, _) -> b | j -> Json.error_sort ctx ~exp:Sort.Bool j
  in
  Repr.Bool (Base.map ~dec ~enc ~enc_meta ())

let json_number =
  let dec _ctx meta n = Json.number ~meta n in
  let enc ctx = function
  | Number (n, _) -> n | j -> Json.error_sort ctx ~exp:Sort.Number j
  in
  Repr.Number (Base.map ~dec ~enc ~enc_meta ())

let json_string =
  let dec _ctx meta s = Json.string ~meta s in
  let enc ctx = function
  | String (s, _) -> s | j -> Json.error_sort ctx ~exp:Sort.String j
  in
  Repr.String (Base.map ~dec ~enc ~enc_meta ())

let json, json_array, mem_list, json_obj =
  let rec elt = Rec any
  and array_map = lazy (
    let dec_empty _ctx = [] in
    let dec_add _ctx _i v a = v :: a in
    let dec_finish _ctx meta _len a = Json.list ~meta (List.rev a) in
    let enc ctx f acc = function
    | Array (a, _) -> Array.list_enc ctx f acc a
    | j -> Json.error_sort ctx ~exp:Sort.Array j
    in
    let enc = { Array.enc = enc } in
    Array.map ~dec_empty ~dec_add ~dec_finish ~enc ~enc_meta elt)
  and array = lazy (Array.array (Lazy.force array_map))
  and mems = lazy (
    let dec_empty _ctx = [] in
    let dec_add _ctx meta n v mems = ((n, meta), v) :: mems in
    let dec_finish _ctx mems = List.rev mems in
    let enc _ctx f l a =
      List.fold_left (fun a ((n, m), v) -> f m n v a) a l
    in
    let enc = { Obj.Mems.enc = enc } in
    Obj.Mems.map ~dec_empty ~dec_add ~dec_finish ~enc elt)
  and obj = lazy (
    let enc_meta ctx = function
    | Obj (_, meta) -> meta | j -> Json.error_sort ctx ~exp:Sort.Obj j
    in
    let enc = function
    | Obj (mems, _) -> mems | j -> Json.error_sort [] ~exp:Sort.Obj j
    in
    let dec _ctx meta mems = Obj (mems, meta) in
    Obj.map' dec ~enc_meta
    |> Obj.keep_unknown (Lazy.force mems) ~enc
    |> Obj.finish)
  and any = lazy (
    let json_array = Lazy.force array in
    let json_obj = Lazy.force obj in
    let enc _ctx = function
    | Null _ -> json_null | Bool _ -> json_bool
    | Number _ -> json_number | String _ -> json_string
    | Array _ -> json_array | Obj _ -> json_obj
    in
    Any { kind = "json"; doc = "";
          dec_null = Some json_null; dec_bool = Some json_bool;
          dec_number = Some json_number; dec_string = Some json_string;
          dec_array = Some json_array;
          dec_obj = Some json_obj; enc })
  in
  Lazy.force any, Lazy.force array, Lazy.force mems, Lazy.force obj

let json_mems =
  let dec_empty _ctx = [] in
  let dec_add _ctx meta n v mems = ((n, meta), v) :: mems in
  let dec_finish _ctx mems = Obj (List.rev mems, Meta.none) in
  let enc ctx f j acc = match j with
  | Obj (mems, _) ->
      List.fold_left (fun acc ((n, m), v) -> f m n v acc) acc mems
  | j -> Json.error_sort ctx ~exp:Sort.Obj j
  in
  let enc = { Obj.Mems.enc = enc } in
  Obj.Mems.map ~dec_empty ~dec_add ~dec_finish ~enc json

(* Queries and updates *)

(*
  val app : ('a -> 'b) t -> 'a t -> 'b t
  val product : 'a t -> 'b t -> ('a * 'b) t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t

  (** {1:string String queries} *) *)

let const t v =
  let const _ _ = v in
  let dec = map ~dec:const ignore in
  let enc = map ~enc:const t in
  let enc _ _ = enc in
  any ~dec_null:dec ~dec_bool:dec ~dec_number:dec ~dec_string:dec
    ~dec_array:dec ~dec_obj:dec ~enc ()

let recode ~dec:dom f ~enc =
  let m = map ~dec:f dom in
  any ~dec_null:m ~dec_bool:m ~dec_number:m ~dec_string:m ~dec_array:m
    ~dec_obj:m ~enc:(fun _ctx _v -> enc) ()

let update t =
  let dec ctx v = Json.update ctx t v in
  let enc _ v = v in
  Map { kind = ""; doc = ""; dom = json; dec; enc }

(* Array queries *)

let rec list_repeat n v l = if n <= 0 then l else list_repeat (n - 1) v (v :: l)

let nth ?absent n t =
  let dec_empty _ctx = None in
  let dec_skip _ i _ = i <> n in
  let dec_add _ _ v _ = Some v in
  let dec_finish ctx meta len v = match v with
  | Some v -> v
  | None ->
      match absent with
      | Some v -> v | None -> Error.out_of_range ctx meta ~n ~len
  in
  let enc ctx f acc v = f acc 0 v in
  let enc = { Array.enc } in
  Array.array (Array.map ~dec_empty ~dec_skip ~dec_add ~dec_finish ~enc t)

let update_nth ?stub ?absent n t =
  let update_elt ctx n t v = Json.update (Context.push_nth n ctx) t v in
  let rec update_array ~seen ctx n t i acc = function
  | v :: vs when i = n ->
      let elt = update_elt ctx (i, Json.meta v) t v in
      update_array ~seen:true ctx n t (i + 1) (elt :: acc) vs
  | v :: vs -> update_array ~seen ctx n t (i + 1) (v :: acc) vs
  | [] when seen -> Either.Right (List.rev acc)
  | [] -> Either.Left (acc, i)
  in
  let update ?stub ?absent n t ctx j = match j with
  | Array (vs, meta) ->
      begin match update_array ~seen:false ctx n t 0 [] vs with
      | Either.Right elts -> Array (elts, meta)
      | Either.Left (acc, len) ->
          match absent with
          | None -> Error.out_of_range ctx meta ~n ~len
          | Some absent ->
              let elt = update_elt ctx (n, Meta.none) t (Json.null ()) in
              let stub = match stub with
              | None -> Json.stub elt | Some j -> j
              in
              Array (List.rev (elt :: list_repeat (n - len) stub acc), meta)
      end
  | j -> Json.error_sort ctx ~exp:Sort.Array j
  in
  let dec = update ?stub ?absent n t in
  let enc _ j = j in
  map ~dec ~enc json

let set_nth ?stub ?(allow_absent = false) t n v =
  let absent = if allow_absent then Some (Json.null ()) else None in
  update_nth ?stub ?absent n (const t v)

let delete_nth ?(allow_absent = false) n =
  let dec_empty _ctx = [] in
  let dec_add _ctx i v a = if i = n then a else (v :: a) in
  let dec_finish ctx meta len a =
    if n < len || allow_absent then Json.list ~meta (List.rev a) else
    Error.out_of_range ctx meta ~n ~len
  in
  let enc ctx f acc = function
  | Array (a, _) -> Array.list_enc ctx f acc a
  | j -> Json.error_sort ctx ~exp:Sort.Array j
  in
  let enc_meta _ctx j = Json.meta j in
  let enc = { Array.enc = enc } in
  Array.array (Array.map ~dec_empty ~dec_add ~dec_finish ~enc ~enc_meta json)

let filter_map_array a b f =
  let dec_empty _ctx = [] in
  let dec_add ctx i v acc = match f ctx i (Json.dec ctx a v) with
  | None -> acc | Some v' -> (Json.enc ctx b v') :: acc
  in
  let dec_finish ctx meta _len acc = Json.list ~meta (List.rev acc) in
  let enc ctx f acc = function
  | Array (a, _) -> Array.list_enc ctx f acc a
  | j -> Json.error_sort ctx ~exp:Sort.Array j
  in
  let enc = { Array.enc = enc } in
  let enc_meta _ctx j = Json.meta j in
  Array.array (Array.map ~dec_empty ~dec_add ~dec_finish ~enc ~enc_meta json)

let fold_array t f acc =
  let dec_empty _ctx = acc in
  let dec_add = f in
  let dec_finish _ctx _meta _len acc = acc in
  let enc ctx _f acc _a = acc in
  let enc = { Array.enc = enc } in
  Array.array (Array.map ~dec_empty ~dec_add ~dec_finish ~enc t)

(* Objects *)

let mem ?absent name t =
  Obj.map Fun.id |> Obj.mem ?dec_absent:absent name t ~enc:Fun.id |> Obj.finish

let update_mem ?absent name t =
  let update_mem ctx n t v = n, Json.update (Context.push_mem n ctx) t v in
  let rec update_obj ~seen ctx name t acc = function
  | ((name', _ as n), v) :: mems when String.equal name name' ->
      update_obj ~seen:true ctx name t (update_mem ctx n t v :: acc) mems
  | mem :: mems -> update_obj ~seen ctx name t (mem :: acc) mems
  | [] when seen -> Either.Right (List.rev acc)
  | [] -> Either.Left acc
  in
  let update ?absent name t ctx = function
  | Obj (mems, meta) ->
      let mems = match update_obj ~seen:false ctx name t [] mems with
      | Either.Right mems -> mems
      | Either.Left acc ->
          match absent with
          | None ->
              let fnd = Json.obj_names mems in
              Error.missing_mems ctx meta ~obj_kind:"" ~exp:[name] ~fnd
          | Some j ->
              let m = update_mem ctx (name, Meta.none) t j in
              List.rev (m :: acc)
      in
      Obj (mems, meta)
  | j -> Json.error_sort ctx ~exp:Sort.Obj j
  in
  let update = update ?absent name t in
  let enc _ctx j = j in
  map ~dec:update ~enc json

let set_mem ?(allow_absent = false) t name v =
  let absent = if allow_absent then Some Json.null' else None in
  update_mem ?absent name (const t v)

let update_json_obj ~name ~dec_add ~dec_finish =
  let mems =
    let dec_empty _ctx = false, [] in
    let enc _ctx f (_, l) a =
      List.fold_left (fun a ((n, m), v) -> f m n v a) a l
    in
    let enc = { Obj.Mems.enc = enc } in
    Obj.Mems.map ~dec_empty ~dec_add ~dec_finish ~enc json
  in
  let enc_meta ctx = function
  | Obj (_, meta) -> meta | j -> Json.error_sort ctx ~exp:Sort.Obj j
  in
  let enc = function
  | Obj (mems, _) -> false, mems | j -> Json.error_sort [] ~exp:Sort.Obj j
  in
  let dec ctx meta (ok, mems) =
    let fnd = Json.obj_names mems in
    if not ok
    then Error.missing_mems ctx meta ~obj_kind:"" ~exp:[name] ~fnd else
    Obj (List.rev mems, meta)
  in
  Obj.map' dec ~enc_meta
  |> Obj.keep_unknown mems ~enc
  |> Obj.finish

let delete_mem ?(allow_absent = false) name =
  let dec_add _ctx meta n v (ok, mems) =
    if n = name then true, mems else ok, ((n, meta), v) :: mems
  in
  let dec_finish _ctx (ok, ms as a) = if allow_absent then (true, ms) else a in
  update_json_obj ~name ~dec_add ~dec_finish

let fold_obj t f acc =
  let mems =
    let dec_empty ctx = acc and dec_add = f and dec_finish _ctx acc = acc in
    let enc ctx f _ acc = acc in
    Obj.Mems.map t ~dec_empty ~dec_add ~dec_finish ~enc:{ Obj.Mems.enc }
  in
  Obj.map Fun.id
  |> Obj.keep_unknown mems ~enc:Fun.id
  |> Obj.finish

let filter_map_obj a b f =
  let dec_add ctx meta n v (_, mems) =
    match f ctx meta n (Json.dec ctx a v) with
    | None -> (true, mems)
    | Some (n', v') -> (true, ((n', meta), (Json.enc ctx b v')) :: mems)
  in
  let dec_finish _ctx acc = acc in
  update_json_obj ~name:"" (* irrelevant *) ~dec_add ~dec_finish

(* Indices *)

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

(* Paths *)

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
            loop Json.empty_obj (update_mem ~absent n t) is
        | [] -> t
        in
        match i with
        | Path.Nth (n, _) ->
            loop Json.empty_array (update_nth ?stub ~absent n t) is
        | Path.Mem (n, _) ->
            loop Json.empty_obj (update_mem ~absent n t) is

let delete_path ?allow_absent p = match Path.rev_indices p with
| [] -> recode ~dec:ignore (fun _ctx () -> Json.null') ~enc:json
| i :: is ->
    let upd del i = update_index i del in
    List.fold_left upd (delete_index ?allow_absent i) is

let set_path ?stub ?(allow_absent = false) t p v = match Path.rev_indices p with
| [] -> recode ~dec:ignore (fun ctx () -> Json.enc ctx t v) ~enc:json
| i :: is ->
    let absent = if allow_absent then Some Json.null' else None in
    update_path ?stub ?absent p (const t v)

module Caret = struct
  type pos = Before | Over | After
  type t = Path.t * pos
  let of_string s =
    let rec loop p s i max =
      if i > max then p, Over else
      let next = i + 1 in
      match s.[i] with
      | 'v' when next <= max && s.[next] = '[' ->
          let next, p = Path.parse_index p s next max in
          Path.parse_eoi s next max; p, Before
      | c ->
          let next, p = Path.parse_index p s i max in
          if next > max then p, Over else
          if s.[next] = 'v'
          then (Path.parse_eoi s (next + 1) max; p, After) else
          if s.[next] <> '.' then Path.err_unexp_char next s else
          if next + 1 <= max then loop p s (next + 1) max else
          Path.err_unexp_eoi next
    in
    try
      if s = "" then Ok ([], Over) else
      let start = if s.[0] = '.' then 1 else 0 in
      Ok (loop [] s start (String.length s - 1))
    with Failure e -> Error e

  let pp ppf = function
  | p, Over -> Path.pp ppf p
  | (c :: p), Before ->
      Path.pp ppf p;
      (if p <> [] then Fmt.char ppf '.');
      Fmt.char ppf 'v'; Path.pp_bracketed_index ppf c
  | (c :: p), After ->
      Path.pp ppf p;
      (if p <> [] then Fmt.char ppf '.');
      Path.pp_bracketed_index ppf c; Fmt.char ppf 'v'
  | _ -> ()
end

(* Formatters *)

type number_format = Fmt.json_number_format
let default_number_format = Fmt.json_default_number_format

type format = Minify | Indent | Layout

let pp_value ?number_format t () =
  fun ppf v -> match Json.encode t v with
  | Ok j ->  pp_json' ?number_format () ppf j
  | Error e -> pp_string ppf e
