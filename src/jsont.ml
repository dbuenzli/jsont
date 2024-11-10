(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module String_set = Set.Make (String)

type 'a fmt = 'a Jsont_base.Fmt.t
module Fmt = Jsont_base.Fmt
let pp_code = Fmt.code
let pp_kind = Fmt.code

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

module Sort = struct
  type t = Null | Bool | Number | String | Array | Object
  let to_string = function
  | Null -> "null" | Bool -> "bool" | Number -> "number"
  | String  -> "string" | Array  -> "array" | Object -> "object"

  let pp ppf s = Fmt.code ppf (to_string s)
end

type error_kind = string
type context = (string node * Path.index) list
type error = context * Meta.t * error_kind
exception Error of error

module Error = struct
  module Context = struct
    type t = context
    let empty = []
    let is_empty c = c = []
    let push_array array_kind idx ctx = (array_kind, Path.Nth idx) :: ctx
    let push_object object_kind mem ctx = (object_kind, Path.Mem mem) :: ctx
    let pp ppf ctx =
      let pp_el ppf (kind, index) = match index with
      | Path.Nth (n, meta) ->
          Fmt.pf ppf "@[<v>%a: at index %a of@,%a: array %a@]"
            Textloc.pp (Meta.textloc meta) pp_code (Int.to_string n)
            Textloc.pp (Meta.textloc (snd kind)) pp_kind (fst kind)
      | Path.Mem (_, meta as n) ->
          Fmt.pf ppf "@[<v>%a: in member %a of@,%a: object %a@]"
            Textloc.pp (Meta.textloc meta) Path.pp_name n
            Textloc.pp (Meta.textloc (snd kind)) pp_kind (fst kind)
      in
      if ctx = [] then () else Fmt.pf ppf "@,@[<v>%a@]" (Fmt.list pp_el) ctx
  end

  type kind = error_kind
  type t = error

  let make_msg ctx meta msg = ctx, meta, msg
  let msg meta msg = raise_notrace (Error (Context.empty, meta, msg))
  let msgf meta fmt = Format.kasprintf (fun m -> msg meta m) fmt

  let push_array array_kind idx (ctx, meta, e) =
    raise_notrace (Error (Context.push_array array_kind idx ctx, meta, e))

  let push_object object_kind obj (ctx, meta, e) =
    raise_notrace (Error (Context.push_object object_kind obj ctx, meta, e))

  let pp ppf (ctx, m, msg) =
    let pp_meta ppf m =
      if Meta.is_none m then () else
      Fmt.pf ppf "@,%a" Textloc.pp (Meta.textloc m)
    in
    Fmt.pf ppf "@[<v>%a%a%a@]"
      (Fmt.list Fmt.string) (String.split_on_char '\n' msg)
      pp_meta m Context.pp ctx

  let to_string e = Format.asprintf "%a" pp e

  let sort meta ~exp ~fnd =
    msgf meta "Expected %a but found %a" Sort.pp exp Sort.pp fnd

  let kind meta ~exp ~fnd =
    msgf meta "Expected %a but found %a" Fmt.code exp Sort.pp fnd

  let missing_mems meta ~object_kind ~exp ~fnd =
    let pp_miss ppf m =
      Fmt.pf ppf "@[%a%a@]" Fmt.code m Fmt.similar_mems (m, fnd)
    in
    match exp with
    | [n] ->
        msgf meta "@[<v>Missing member %a in %a%a@]"
          Fmt.code n Fmt.code object_kind Fmt.similar_mems (n, fnd)
    | exp ->
        msgf meta "@[<v1>Missing members in %a:@,%a@]"
          Fmt.code object_kind (Fmt.list pp_miss) exp

  let unexpected_mems meta ~object_kind ~exp ~fnd =
    let pp_unexp ppf m =
      Fmt.pf ppf " @[%a%a@]" Fmt.code m Fmt.should_it_be_mem (m, exp)
    in
    match fnd with
    | [(u, _)] -> (* TODO use the name metas *)
        msgf meta "@[<v>Unexpected member %a for %a%a@]"
          Fmt.code u Fmt.code object_kind Fmt.should_it_be_mem (u, exp)
    | us ->
        msgf meta "@[<v1>Unexpected members for %a:@,%a@]"
          Fmt.code object_kind (Fmt.list pp_unexp) (List.map fst us)

  let unexpected_case_tag meta ~object_kind ~mem_name ~exp ~fnd =
    let pp_kind ppf () =
      Fmt.pf ppf "member %a value in %a" Fmt.code mem_name Fmt.code object_kind
    in
    msgf meta "@[%a@]" (Fmt.out_of_dom ~pp_kind ()) (fnd, exp)

  let out_of_range meta ~n ~len =
    let s = Int.to_string in
    msgf meta "Index %a out of range [%a;%a]"
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
    enc_meta : 'a -> Meta.t;
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
    dec_finish : 'builder -> 'mems;
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

  let of_t = Fun.id
  let unsafe_to_t = Fun.id

  let sort_kind' ~kind ~sort =
    if kind = "" then sort else Printf.sprintf "%s %s" kind sort

  let sort_kind ~kind ~sort = sort_kind' ~kind ~sort:(Sort.to_string sort)
  let kind_or_sort ~kind ~sort =
    if kind <> "" then kind else (Sort.to_string sort)

  let mems_map_kind (map : (_, _, _) mems_map) =
    sort_kind' ~kind:map.kind ~sort:"members"

  let rec value_kind : type a. a t -> string = function
  | Null map -> sort_kind ~kind:map.kind ~sort:Null
  | Bool map -> sort_kind ~kind:map.kind ~sort:Bool
  | Number map -> sort_kind ~kind:map.kind ~sort:Number
  | String map -> sort_kind ~kind:map.kind ~sort:String
  | Array map -> array_kind ~kind:map.kind map.elt
  | Object map -> sort_kind ~kind:map.kind ~sort:Object
  | Any map -> if map.kind = "" then any_map_kind map else map.kind
  | Map map -> if map.kind = "" then value_kind map.dom else map.kind
  | Rec l -> value_kind (Lazy.force l)

  and array_kind : type a. kind:string -> a t -> string = fun ~kind:k t ->
    let type_kind ~kind ~type' = Printf.sprintf "%s<%s>" type' kind in
    sort_kind' ~kind:k ~sort:(type_kind ~kind:(value_kind t) ~type':"array")

  and any_map_kind : type a. a any_map -> string = fun map ->
    let add_case ks sort = function
    | None -> ks
    | Some k ->
        (if map.kind <> ""
         then value_kind k else sort_kind ~kind:map.kind ~sort)
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
  | Null map -> kind_or_sort ~kind:map.kind ~sort:Null
  | Bool map -> kind_or_sort ~kind:map.kind ~sort:Bool
  | Number map -> kind_or_sort ~kind:map.kind ~sort:Number
  | String map -> kind_or_sort ~kind:map.kind ~sort:String
  | Array map -> kind_or_sort ~kind:map.kind ~sort:Array
  | Object map -> kind_or_sort ~kind:map.kind ~sort:Object
  | Any map -> if map.kind <> "" then map.kind else "any"
  | Map map -> if map.kind <> "" then map.kind else kind map.dom
  | Rec l -> kind (Lazy.force l)

  let rec doc : type a. a t -> string = function
  | Null map -> map.doc | Bool map -> map.doc | Number map -> map.doc
  | String map -> map.doc | Array map -> map.doc | Object map -> map.doc
  | Any map -> map.doc | Map map -> map.doc | Rec l -> doc (Lazy.force l)

  let array_map_value_kind map = array_kind ~kind:map.kind map.elt
  let object_map_value_kind (map : ('o, 'dec) object_map) =
    sort_kind ~kind:map.kind ~sort:Object

  let type_error meta t ~fnd = Error.kind meta ~exp:(value_kind t) ~fnd

  let missing_mems_error meta (object_map : ('o, 'o) object_map) ~exp ~fnd =
    let object_kind = object_map_value_kind object_map in
    let exp =
      let add n (Mem_dec m) acc = match m.dec_absent with
      | None -> n :: acc | Some _ -> acc
      in
      List.rev (String_map.fold add exp [])
    in
    Error.missing_mems meta ~object_kind ~exp ~fnd

  let unexpected_mems_error meta (object_map : ('o, 'o) object_map) ~fnd =
    (* FIXME context ? *)
    let object_kind = object_map_value_kind object_map in
    let exp = List.map (fun (Mem_enc m) -> m.name) object_map.mem_encs in
    Error.unexpected_mems meta ~object_kind ~exp ~fnd

  let unexpected_case_tag_error meta object_map object_cases tag =
    (* FIXME context *)
    let object_kind = object_map_value_kind object_map in
    let case_to_string (Case c) = match object_cases.tag_to_string with
    | None -> None | Some str -> Some (str c.tag)
    in
    let exp = List.filter_map case_to_string object_cases.cases in
    let fnd = match object_cases.tag_to_string with
    | None -> "<value>" (* FIXME not good *) | Some str -> str tag
    in
    let mem_name = object_cases.tag.name in
    Error.unexpected_case_tag meta ~object_kind ~mem_name ~exp ~fnd

  let error_push_object meta map name e =
    Error.push_object ((object_map_value_kind map), meta) name e

  let error_push_array meta map i e =
    Error.push_array ((array_map_value_kind map), meta) i e

  let pp_code = pp_code
  let pp_kind = pp_kind

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

  let object_meta_arg : Meta.t Type.Id.t = Type.Id.make ()

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
          let empty = umap.dec_finish (umap.dec_empty ()) in
          let dict =  Dict.add umap.id empty dict in
          by, dict
      | _ -> by, dict

  let finish_object_decode : type o p m mems builder.
    (o, o) object_map -> Meta.t -> (p, mems, builder) unknown_mems -> builder ->
    mem_dec String_map.t -> Dict.t -> Dict.t
    =
    fun map meta umems umap mem_decs dict ->
    let dict = match umems with
    | Unknown_skip | Unknown_error -> dict
    | Unknown_keep (map, _) -> Dict.add map.id (map.dec_finish umap) dict
    in
    let add_default _ (Mem_dec mem_map) dict = match mem_map.dec_absent with
    | Some v -> Dict.add mem_map.id v dict
    | None -> raise Exit (* FIXME *)
    in
    (try String_map.fold add_default mem_decs dict with
    | Exit ->
        let exp = mem_decs in
        missing_mems_error meta map ~exp ~fnd:[])
end

include Repr

let pp_kind = Fmt.code
let for_kind ppf = function "" -> () | k -> Fmt.pf ppf " for %a" pp_kind k
let dec_none k meta _ = Error.msgf meta "No decoder%a" for_kind k
let enc_none k _ = Error.msgf Meta.none "No encoder%a" for_kind k
let enc_meta_none _ = Meta.none
let enc_ignore _ = Error.msg Meta.none "Decoding ignored, cannot encode"

module Base = struct
  type ('a, 'b) map = ('a, 'b) base_map

  let map
      ?(kind = "") ?(doc = "") ?(dec = dec_none kind) ?(enc = enc_none kind)
      ?(enc_meta = enc_meta_none) ()
    =
    { kind; doc; dec; enc; enc_meta }

  let id =
    let dec _meta v = v and enc v = v in
    { kind = ""; doc = ""; dec; enc; enc_meta = enc_meta_none }

  let ignore =
    let dec _meta _v = () and enc = enc_ignore in
    { kind = "ignored"; doc = ""; dec; enc; enc_meta = enc_meta_none }

  let null map = Null map
  let bool map = Bool map
  let number map = Number map
  let string map = String map

  let error meta kind e =
    let pp_kind_header ppf k =
      if k = "" then () else Fmt.pf ppf "%a: " Repr.pp_kind k
    in
    Error.msgf meta "%a%s" pp_kind_header kind e

  let dec dec _meta v = dec v
  let dec_result ?(kind = "") dec meta v = match dec v with
  | Ok v -> v | Error e -> error meta kind e

  let dec_failure ?(kind = "") dec meta v = try dec v with
  | Failure e -> error meta kind e

  let enc enc v = enc v
  let enc_failure ?(kind = "") enc v = try enc v with
  | Failure e -> error Meta.none kind e

  let enc_result ?(kind = "") enc v = match enc v with
  | Ok v -> v | Error e -> error Meta.none kind e
end

module Array = struct
  type ('array, 'elt, 'builder) map = ('array, 'elt, 'builder) array_map
  type ('array, 'elt) enc =
    { enc : 'acc. ('acc -> int -> 'elt -> 'acc) -> 'acc -> 'array -> 'acc }

  let default_skip _i _builder = false
  let map
      ?(kind = "") ?(doc = "") ~dec_empty ?dec_skip ~dec_add ~dec_finish
      ~enc:{enc} ?(enc_meta = enc_meta_none) elt
    =
    let dec_skip = Option.value ~default:default_skip dec_skip in
    { kind; doc; elt; dec_empty; dec_add; dec_skip; dec_finish; enc; enc_meta; }

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

  let array map = Array map

  let ignore =
    let dec_empty () = () in
    let dec_add _i _v () = () in
    let dec_skip _i () = true in
    let dec_finish _meta _len () = () in
    let stub =
      Map { kind = ""; doc = "";
            dom = Base.(null id);
            enc = (fun _ -> assert false);
            dec = (fun _ -> assert false); }
    in
    let enc _f _acc () = Error.msg Meta.none "No encoder specified" in
    let enc = { enc } in
    let kind = "ignored" in
    array (map ~kind ~dec_empty ~dec_skip ~dec_add ~dec_finish ~enc stub)
end

module Object = struct
  module Mem = struct
    type ('o, 'a) map = ('o, 'a) mem_map

    let noenc name = fun _v ->
      Error.msgf Meta.none "No encoder for member %a" Fmt.code name

    let map ?(doc = "") ?dec_absent ?enc ?enc_meta ?enc_omit name type' =
      let id = Type.Id.make () in
      let enc = match enc with None -> noenc name | Some enc -> enc in
      let enc_omit = match enc_omit with
      | None -> fun _v -> false | Some omit -> omit
      in
      let enc_meta = match enc_meta with
      | None -> enc_meta_none | Some enc_meta -> enc_meta
      in
      { name; doc; type'; id; dec_absent; enc; enc_omit; enc_meta }

    let app object_map mm =
      let mem_decs = String_map.add mm.name (Mem_dec mm) object_map.mem_decs in
      let mem_encs = Mem_enc mm :: object_map.mem_encs in
      let dec = Dec_app (object_map.dec, mm.id) in
      { object_map with dec; mem_decs; mem_encs }
  end

  type ('o, 'dec) map = ('o, 'dec) object_map

  let kind = Repr.object_map_value_kind (* FIXME *)
  let map ?(kind = "") ?(doc = "") dec =
    { kind; doc; dec = Dec_fun dec; mem_decs = String_map.empty; mem_encs = [];
      enc_meta = enc_meta_none; shape = Object_basic Unknown_skip }

  let map' ?(kind = "") ?(doc = "") ?(enc_meta = enc_meta_none) dec =
    let dec = Dec_app (Dec_fun dec, object_meta_arg) in
    { kind; doc; dec; mem_decs = String_map.empty;
      mem_encs = []; enc_meta; shape = Object_basic Unknown_skip }

  let enc_only ?(kind = "") ?(doc = "") ?(enc_meta = enc_meta_none) () =
    let dec meta =
      (* FIXME use the kind stuff *)
      let kind = if kind = "" then "object" else kind in
      Error.msg meta ("No decoder for " ^ kind)
    in
    let dec = Dec_app (Dec_fun dec, object_meta_arg) in
    { kind; doc; dec; mem_decs = String_map.empty;
      mem_encs = []; enc_meta; shape = Object_basic Unknown_skip }

  let check_name_unicity m =
    let add n kind = function
    | None -> Some kind
    | Some kind' ->
        let ks k = if k = "" then "<object>" else k in
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

  let rev_mem_encs m = { m with mem_encs = List.rev m.mem_encs }
  let finish mems =
    let () = check_name_unicity mems in
    Object (rev_mem_encs mems)

  let unfinish = function
  | Object map -> rev_mem_encs map | _ -> invalid_arg "Not an object"

  let mem ?(doc = "") ?dec_absent ?enc ?enc_meta ?enc_omit name type' map =
    let mm =
      Mem.map ~doc ?dec_absent ?enc ?enc_meta ?enc_omit name type'
    in
    let mem_decs = String_map.add name (Mem_dec mm) map.mem_decs in
    let mem_encs = Mem_enc mm :: map.mem_encs in
    let dec = Dec_app (map.dec, mm.id) in
    { map with dec; mem_decs; mem_encs }

  let opt_mem ?doc ?enc name dom map =
    let some =
      Map { kind = ""; doc = ""; dom; dec = Option.some; enc = Option.get }
    in
    mem ?doc ~dec_absent:None ?enc ~enc_omit:Option.is_none name some map

  module Case = struct
    type ('cases, 'case, 'tag) map = ('cases, 'case, 'tag) case_map
    type ('cases, 'tag) t = ('cases, 'tag) case
    type ('cases, 'tag) value = ('cases, 'tag) case_value

    let case_no_dec _ = Error.msg Meta.none "No case decoder specified"
    let map ?(dec = case_no_dec) tag obj =
      let object_map = unfinish obj in
      { tag; object_map = rev_mem_encs object_map; dec; }

    let make c = Case c
    let value c v = Case_value (c, v)
  end

  let case_mem
      ?(doc = "") ?(tag_compare = Stdlib.compare) ?tag_to_string ?dec_absent
      ?enc ?enc_omit ?enc_case name type' cases map
    =
    (* TODO check dec_absent has a case to avoid puzzling decoding errors. *)
    let () = match map.shape with
    | Object_cases _ -> invalid_arg "Multiple calls to Jsont.Obj.case_mem"
    | _ -> ()
    in
    let tag =
      let id = Type.Id.make () in
      let enc t = t (* N.B. this fact may be used by encoders. *) in
      let enc_omit = match enc_omit with
      | None -> fun _v -> false | Some omit -> omit
      in
      let enc_meta = enc_meta_none in
      { name; doc; type'; id; dec_absent; enc; enc_omit; enc_meta }
    in
    let enc_case = match enc_case with
    | None -> fun c -> Error.msg Meta.none "No case encoder specified"
    | Some enc_case -> enc_case
    in
    let enc = match enc with
    | None -> fun _ -> Error.msg Meta.none "No encoder specified"
    | Some enc -> enc
    in
    let id = Type.Id.make () in
    let shape =
      Object_cases
        (None, { tag; tag_compare; tag_to_string; id; cases; enc; enc_case })
    in
    let dec = Dec_app (map.dec, id) in
    { map with dec; shape }

  module Mems = struct
    type ('mems, 'a, 'builder) map = ('mems, 'a, 'builder) mems_map
    type ('mems, 'a) enc =
      { enc :
          'acc. (Meta.t -> string -> 'a -> 'acc -> 'acc) -> 'mems -> 'acc ->
          'acc }

    let map
        ?(kind = "") ?(doc = "") mems_type ~dec_empty ~dec_add ~dec_finish
        ~enc:{enc}
      =
      let id = Type.Id.make () in
      { kind; doc; mems_type; id; dec_empty; dec_add; dec_finish; enc }

    let string_map ?kind ?doc type' =
      let dec_empty () = String_map.empty in
      let dec_add _meta n v m = String_map.add n v m in
      let dec_finish = Fun.id in
      let enc f meta acc =
        String_map.fold (fun n v acc -> f Meta.none n v acc) meta acc
      in
      map ?kind ?doc type' ~dec_empty ~dec_add ~dec_finish ~enc:{enc}
  end

  let set_shape_unknown_mems shape u = match shape with
  | Object_basic (Unknown_keep _) | Object_cases (Some (Unknown_keep _), _) ->
      invalid_arg "Jsont.Obj.keep_unknown already called on object"
  | Object_basic _ -> Object_basic u
  | Object_cases (_, cases) -> Object_cases (Some u, cases)

  let skip_unknown map =
    { map with shape = set_shape_unknown_mems map.shape Unknown_skip }

  let error_unknown map =
    { map with shape = set_shape_unknown_mems map.shape Unknown_error }

  let mems_noenc mems _o =
    Error.msg Meta.none ("No encoder for" ^ (mems_map_kind mems))

  let keep_unknown ?enc msm (map : ('o, 'dec) object_map) =
    let enc = match enc with None -> mems_noenc msm | Some enc -> enc in
    let dec = Dec_app (map.dec, msm.id) in
    let unknown = Unknown_keep (msm, enc) in
    { map with dec; shape = set_shape_unknown_mems map.shape unknown }

  let ignore = finish (map ~kind:"ignored" ())

  let as_string_map ?kind ?doc t =
    map ?kind ?doc Fun.id
    |> keep_unknown (Mems.string_map t) ~enc:Fun.id
    |> finish
end

let any
    ?(kind = "") ?(doc = "") ?dec_null ?dec_bool ?dec_number ?dec_string
    ?dec_array ?dec_object ?enc ()
  =
  let enc = match enc with
  | Some enc -> enc
  | None ->
      fun _ -> Error.msgf Meta.none "No encoding type specified%a" for_kind kind
  in
  Any { kind; doc; dec_null; dec_bool; dec_number; dec_string; dec_array;
        dec_object; enc }

let map ?(kind = "") ?(doc = "") ?dec ?enc dom =
  let dec = match dec with
  | Some dec -> dec
  | None -> fun _ -> Error.msgf Meta.none "No decoder%a" for_kind kind
  in
  let enc = match enc with
  | Some enc -> enc
  | None -> fun _ -> Error.msgf Meta.none "No encoder%a" for_kind kind
  in
  Map { kind; doc; dom; dec; enc; }

let rec' t = Rec t

(* Ignoring *)

let ignore =
  let kind = "ignored" in
  let enc = enc_none kind in
  let dec_null = Null Base.ignore and dec_bool = Bool Base.ignore in
  let dec_number = Number Base.ignore and dec_string = String Base.ignore in
  let dec_array = Array.ignore and dec_object = Object.ignore in
  any
    ~kind ~dec_null ~dec_bool ~dec_number ~dec_string ~dec_array ~dec_object
    ~enc ()

let todo ?(kind = "") ?doc ?dec_stub () =
  let dec_none _ = Error.msgf Meta.none "Decoder%a is todo" for_kind kind in
  let dec = match dec_stub with
  | None -> dec_none | Some v -> Fun.const v
  in
  let enc _ = Error.msgf Meta.none "Encoder%a is todo" for_kind kind in
  map ~kind ?doc ~dec ~enc ignore

(* Base types *)

let null ?kind ?doc v =
  let dec _meta () = v and enc _meta = () in
  Null (Base.map ?doc ?kind ~dec ~enc ())

let bool = Bool Base.id
let number = Number Base.id
let string = String Base.id

(* Option *)

let none =
  let none = (* Can't use [Base.map] because of the value restriction. *)
    let dec _meta _v = None and enc _ = () in
    { kind = ""; doc = ""; dec; enc; enc_meta = enc_meta_none }
  in
  Null none

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

(* Integers *)

let[@inline] check_num_finite meta ~kind v =
  if Float.is_finite v then () else
  let kind = Repr.sort_kind ~kind ~sort:Number in
  Error.kind meta ~exp:kind ~fnd:Sort.Null

let err_num_range meta ~kind n =
  Error.msgf meta "Number %a not in expected %a range"
    Fmt.code (Fmt.str "%a" Fmt.json_number n) Fmt.code kind

let err_str_num_parse meta ~kind s =
  Error.msgf meta "String %a: does not parse to %a value"
    Fmt.json_string s Fmt.code kind

let err_num_enc_range ~kind n =
  Error.msgf Meta.none "Integer %a not in expected %a range"
    Fmt.code (Int.to_string n) Fmt.code kind

let int_as_string =
  let kind = "OCaml int" in
  let dec meta v = match int_of_string_opt v with
  | Some v -> v | None -> err_str_num_parse meta ~kind v
  in
  Base.string (Base.map ~kind ~dec ~enc:Int.to_string ())

let int_number =
  (* Usage by [int] entails there's no need to test for nan or check range on
     encoding. *)
  let kind = "OCaml int" in
  let dec meta v =
    if Jsont_base.Number.in_exact_int_range v then Int.of_float v else
    err_num_range meta ~kind v
  in
  Base.number (Base.map ~kind ~dec ~enc:Int.to_float ())

let int =
  let enc v =
    if Jsont_base.Number.can_store_exact_int v then int_number else
    int_as_string
  in
  let dec_number = int_number and dec_string = int_as_string in
  any ~kind:"OCaml int" ~dec_number ~dec_string ~enc ()

let uint8 =
  let kind = "uint8" in
  let dec meta v =
    check_num_finite meta ~kind v;
    if Jsont_base.Number.in_exact_uint8_range v then Int.of_float v else
    err_num_range meta ~kind v
  in
  let enc v =
    if Jsont_base.Number.int_is_uint8 v then Int.to_float v else
    err_num_enc_range ~kind v
  in
  Base.number (Base.map ~kind ~dec ~enc ())

let uint16 =
  let kind = "uint16" in
  let dec meta v =
    check_num_finite meta ~kind v;
    if Jsont_base.Number.in_exact_uint16_range v then Int.of_float v else
    err_num_range meta ~kind v
  in
  let enc v =
    if Jsont_base.Number.int_is_uint16 v then Int.to_float v else
    err_num_enc_range ~kind v
  in
  Base.number (Base.map ~kind ~dec ~enc ())

let int8 =
  let kind = "int8" in
  let dec meta v =
    check_num_finite meta ~kind v;
    if Jsont_base.Number.in_exact_int8_range v then Int.of_float v else
    err_num_range meta ~kind v
  in
  let enc v =
    if Jsont_base.Number.int_is_int8 v then Int.to_float v else
    err_num_enc_range ~kind v
  in
  Base.number (Base.map ~kind ~dec ~enc ())

let int16 =
  let kind = "int16" in
  let dec meta v =
    check_num_finite meta ~kind v;
    if Jsont_base.Number.in_exact_int16_range v then Int.of_float v else
    err_num_range meta ~kind v
  in
  let enc v =
    if Jsont_base.Number.int_is_int16 v then Int.to_float v else
    err_num_enc_range ~kind v
  in
  Base.number (Base.map ~kind ~dec ~enc ())

let int32 =
  let kind = "int32" in
  let dec meta v =
    check_num_finite meta ~kind v;
    if Jsont_base.Number.in_exact_int32_range v then Int32.of_float v else
    err_num_range meta ~kind v
  in
  let enc = Int32.to_float (* Everything always fits *)  in
  Base.number (Base.map ~kind ~dec ~enc ())

let int64_as_string =
  let kind = "int64" in
  let dec meta v = match Int64.of_string_opt v with
  | Some v -> v | None -> err_str_num_parse meta ~kind v
  in
  Base.string (Base.map ~kind ~dec ~enc:Int64.to_string ())

let int64_number =
  (* Usage by [int64] entails there's no need to test for nan or check
     range on encoding. *)
  let kind = "int64" in
  let dec meta v =
    if Jsont_base.Number.in_exact_int64_range v then Int64.of_float v else
    err_num_range meta ~kind v
  in
  Base.number (Base.map ~kind ~dec ~enc:Int64.to_float ())

let int64 =
  let dec_number = int64_number and dec_string = int64_as_string in
  let enc v =
    if Jsont_base.Number.can_store_exact_int64 v then int64_number else
    int64_as_string
  in
  any ~kind:"int64" ~dec_number ~dec_string ~enc ()

(* Floats *)

let any_float =
  let kind = "float" in
  let finite = number in
  let non_finite =
    let dec m v = match Float.of_string_opt v with
    | Some v -> v | None -> err_str_num_parse m ~kind v
    in
    Base.string (Base.map ~kind ~dec ~enc:Float.to_string ())
  in
  let enc v = if Float.is_finite v then finite else non_finite in
  any ~kind ~dec_null:finite ~dec_number:finite ~dec_string:non_finite ~enc ()

let float_as_hex_string =
  let kind = "float" in
  let dec meta v = match Float.of_string_opt v with
  | Some v -> v | None -> err_str_num_parse meta ~kind v
  in
  let enc v = Printf.sprintf "%h" v in
  Base.string (Base.map ~kind ~dec ~enc ())

(* Strings *)

let of_of_string ?kind ?doc ?enc of_string =
  let dec meta s = match of_string s with
  | Ok v -> v | Error e -> Error.msg meta e
  in
  Base.string (Base.map ?kind ?doc ?enc ~dec ())

let enum (type a) ?(cmp = Stdlib.compare) ?(kind = "") ?doc assoc =
  let kind = Repr.sort_kind' ~kind ~sort:"enum" in
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
  let enc v = match enc_map v with
  | None -> Error.msg Meta.none "Unknown enum value"
  | Some s -> s
  in
  let dec meta s = match dec_map s with
  | Some v -> v
  | None ->
      let kind = Repr.sort_kind ~kind ~sort:String in
      let pp_kind ppf () = Fmt.pf ppf "%a value" Repr.pp_kind kind in
      Error.msgf meta "%a" Fmt.(out_of_dom ~pp_kind ()) (s, List.map fst assoc)
  in
  Base.string (Base.map ~kind ?doc ~dec ~enc ())

let binary_string =
  let kind = "hex" in
  let kind' = Repr.sort_kind ~kind ~sort:String in
  let dec = Base.dec_result ~kind:kind' Jsont_base.binary_string_of_hex in
  let enc = Base.enc Jsont_base.binary_string_to_hex in
  Base.string (Base.map ~kind ~dec ~enc ())

(* Arrays *)

let list ?kind ?doc t = Array (Array.list_map ?kind ?doc t)
let array ?kind ?doc t = Array (Array.array_map ?kind ?doc t)
let array_as_string_map ?kind ?doc ~key t =
  let dec_empty () = String_map.empty in
  let dec_add _i elt acc = String_map.add (key elt) elt acc in
  let dec_finish _meta _len acc = acc in
  let enc f acc m =
    let i = ref (-1) in
    String_map.fold (fun _ elt acc -> incr i; f acc !i elt) m acc
  in
  let enc = Array.{enc} in
  let map = Array.map ?kind ?doc ~dec_empty ~dec_add ~dec_finish ~enc t in
  Array map

let bigarray ?kind ?doc k t =
  Array (Array.bigarray_map ?kind ?doc k Bigarray.c_layout t)

(* Uniform tuples *)

let tuple_dec_none meta = Error.msg meta "No tuple decoder specified"
let tuple_enc_none _f _acc _v = Error.msg Meta.none "No tuple encoder specified"

let error_tuple_size meta kind ~exp fnd =
  Error.msgf meta "Expected %a elements in %a but found %a"
    Fmt.code (Int.to_string exp) Fmt.code kind
    Fmt.code (Int.to_string fnd)

let t2 ?(kind = "") ?doc ?dec ?enc t =
  let size = 2 in
  let dec = match dec with
  | None -> fun meta v0 v1 -> tuple_dec_none meta
  | Some dec -> fun _meta v0 v1 -> dec v0 v1
  in
  let dec_empty () = [] in
  let dec_add _i v acc = v :: acc in
  let dec_finish meta _len = function
  | [v1; v0] -> dec meta v0 v1
  | l -> error_tuple_size meta kind ~exp:size (List.length l)
  in
  let enc = match enc with
  | None -> tuple_enc_none
  | Some enc -> fun f acc v -> f (f acc 0 (enc v 0)) 1 (enc v 1)
  in
  let enc = { Array.enc } in
  Array (Array.map ~kind ?doc ~dec_empty ~dec_add ~dec_finish ~enc t)

let t3 ?(kind = "") ?doc ?dec ?enc t =
  let size = 3 in
  let dec = match dec with
  | None -> fun meta v0 v1 v2 -> tuple_dec_none meta
  | Some dec -> fun _meta v0 v1 v2 -> dec v0 v1 v2
  in
  let dec_empty () = [] in
  let dec_add _i v acc = v :: acc in
  let dec_finish meta _len = function
  | [v2; v1; v0] -> dec meta v0 v1 v2
  | l -> error_tuple_size meta kind ~exp:size (List.length l)
  in
  let enc = match enc with
  | None -> tuple_enc_none
  | Some enc ->
      fun f acc v -> f (f (f acc 0 (enc v 0)) 1 (enc v 1)) 2 (enc v 2)
  in
  let enc = { Array.enc } in
  Array (Array.map ~kind ?doc ~dec_empty ~dec_add ~dec_finish ~enc t)

let t4 ?(kind = "") ?doc ?dec ?enc t =
  let size = 4 in
  let dec = match dec with
  | None -> fun meta v0 v1 v2 v3 -> tuple_dec_none meta
  | Some dec -> fun _meta v0 v1 v2 v3 -> dec v0 v1 v2 v3
  in
  let dec_empty () = [] in
  let dec_add _i v acc = v :: acc in
  let dec_finish meta _len = function
  | [v3; v2; v1; v0] -> dec meta v0 v1 v2 v3
  | l -> error_tuple_size meta kind ~exp:size (List.length l)
  in
  let enc = match enc with
  | None -> tuple_enc_none
  | Some enc ->
      fun f acc v ->
        f (f (f (f acc 0 (enc v 0)) 1 (enc v 1)) 2 (enc v 2)) 3 (enc v 3)
  in
  let enc = { Array.enc } in
  Array (Array.map ~kind ?doc ~dec_empty ~dec_add ~dec_finish ~enc t)

let tn ?(kind = "") ?doc ~n elt =
  let dec_empty () = Jsont_base.Rarray.empty () in
  let dec_add _i v a = Jsont_base.Rarray.add_last v a in
  let dec_finish meta _len a =
    let len = Jsont_base.Rarray.length a in
    if len <> n then error_tuple_size meta kind ~exp:n len else
    Jsont_base.Rarray.to_array a
  in
  let enc = { Array.enc = Array.array_enc } in
  Array (Array.map ~kind ?doc ~dec_empty ~dec_add ~dec_finish ~enc elt)

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

module Json = struct
  let meta = function
  | Null (_, m) -> m | Bool (_, m) -> m | Number (_, m) -> m
  | String (_, m) -> m | Array (_, m) -> m | Object (_, m) -> m

  let get_meta = meta

  let sort = function
  | Null _ -> Sort.Null | Bool _ -> Sort.Bool | Number _ -> Sort.Number
  | String _ -> Sort.String | Array _ -> Sort.Array | Object _ -> Sort.Object

  let rec find_mem n = function
  | [] -> None
  | ((n', _), _ as m) :: ms ->
      if String.equal n n' then Some m else find_mem n ms

  let find_mem' (n, _) ms = find_mem n ms

  let object_names mems = List.map (fun ((n, _), _) -> n) mems
  let object_names' mems = List.map fst mems

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
  let object' ?(meta = Meta.none) mems = Object (mems, meta)
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
  | Array -> list ?meta [] | Object -> object' ?meta []

  let empty_array = list []
  let empty_object = object' []

  (* Errors *)

  (* FIXME move to repr ? *)

  let error_sort ~exp j = Error.sort (meta j) ~exp ~fnd:(sort j)
  let error_type t fnd =
    Error.kind (meta fnd) ~exp:(value_kind t) ~fnd:(sort fnd)

  let find_all_unexpected ~mem_decs mems =
    let unexpected ((n, _ as nm), _v) =
      match String_map.find_opt n mem_decs with
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
    let dict = match map.shape with
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
    in
    Dict.add object_meta_arg meta dict

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
            let object_kind = Repr.object_map_value_kind map in
            Error.missing_mems meta ~object_kind
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
        ((mmap.name, mmap.enc_meta v), encode mmap.type' v) :: obj
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
   let update t v = enc t (dec t v)

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
  | Object (o0, _), Object (o1, _) ->
      let order_mem ((n0, _), _) ((n1, _), _) = String.compare n0 n1 in
      let compare_mem ((n0, _), j0) ((n1, _), j1) =
        let c = String.compare n0 n1 in
        if c = 0 then compare j0 j1 else c
      in
      List.compare compare_mem (List.sort order_mem o0) (List.sort order_mem o1)

  let equal j0 j1 = compare j0 j1 = 0
  let pp = pp_json
end

let enc_meta j = Json.meta j

let json_null =
  let dec meta () = Json.null ~meta () in
  let enc = function
  | Null ((), _) -> () | j -> Json.error_sort ~exp:Sort.Null j
  in
  Repr.Null (Base.map ~dec ~enc ~enc_meta ())

let json_bool =
  let dec meta b = Json.bool ~meta b in
  let enc = function
  | Bool (b, _) -> b | j -> Json.error_sort ~exp:Sort.Bool j
  in
  Repr.Bool (Base.map ~dec ~enc ~enc_meta ())

let json_number =
  let dec meta n = Json.number ~meta n in
  let enc = function
  | Number (n, _) -> n | j -> Json.error_sort ~exp:Sort.Number j
  in
  Repr.Number (Base.map ~dec ~enc ~enc_meta ())

let json_string =
  let dec meta s = Json.string ~meta s in
  let enc = function
  | String (s, _) -> s | j -> Json.error_sort ~exp:Sort.String j
  in
  Repr.String (Base.map ~dec ~enc ~enc_meta ())

let json, json_array, mem_list, json_object =
  let rec elt = Rec any
  and array_map = lazy begin
    let dec_empty () = [] in
    let dec_add _i v a = v :: a in
    let dec_finish meta _len a = Json.list ~meta (List.rev a) in
    let enc f acc = function
    | Array (a, _) -> Array.list_enc f acc a
    | j -> Json.error_sort ~exp:Sort.Array j
    in
    let enc = { Array.enc = enc } in
    Array.map ~dec_empty ~dec_add ~dec_finish ~enc ~enc_meta elt
  end

  and array = lazy (Array.array (Lazy.force array_map))
  and mems = lazy begin
    let dec_empty () = [] in
    let dec_add meta n v mems = ((n, meta), v) :: mems in
    let dec_finish = List.rev in
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
    Any { kind = "json"; doc = "";
          dec_null = Some json_null; dec_bool = Some json_bool;
          dec_number = Some json_number; dec_string = Some json_string;
          dec_array = Some json_array;
          dec_object = Some json_object; enc }
   end
  in
  Lazy.force any, Lazy.force array, Lazy.force mems, Lazy.force object'

let json_mems =
  let dec_empty () = [] in
  let dec_add meta n v mems = ((n, meta), v) :: mems in
  let dec_finish mems = Object (List.rev mems, Meta.none) in
  let enc f j acc = match j with
  | Object (ms, _) -> List.fold_left (fun acc ((n, m), v) -> f m n v acc) acc ms
  | j -> Json.error_sort ~exp:Sort.Object j
  in
  let enc = { Object.Mems.enc = enc } in
  Object.Mems.map ~dec_empty ~dec_add ~dec_finish ~enc json

(* Queries and updates *)

(*
  val app : ('a -> 'b) t -> 'a t -> 'b t
  val product : 'a t -> 'b t -> ('a * 'b) t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t

  (** {1:string String queries} *) *)

let const t v =
  let const _ = v in
  let dec = map ~dec:const ignore in
  let enc = map ~enc:const t in
  let enc _v = enc in
  any
    ~dec_null:dec ~dec_bool:dec ~dec_number:dec ~dec_string:dec ~dec_array:dec
    ~dec_object:dec ~enc ()

let recode ~dec:dom f ~enc =
  let m = map ~dec:f dom in
  let enc _v = enc in
  any
    ~dec_null:m ~dec_bool:m ~dec_number:m ~dec_string:m ~dec_array:m
    ~dec_object:m ~enc ()

let update t =
  let dec v = Json.update t v in
  let enc v = v in
  Map { kind = ""; doc = ""; dom = json; dec; enc }

(* Array queries *)

let rec list_repeat n v l = if n <= 0 then l else list_repeat (n - 1) v (v :: l)

let nth ?absent n t =
  let dec_empty () = None in
  let dec_skip i _v = i <> n in
  let dec_add _i v _acc = Some v in
  let dec_finish meta len v = match v with
  | Some v -> v
  | None ->
      match absent with Some v -> v | None -> Error.out_of_range meta ~n ~len
  in
  let enc f acc v = f acc 0 v in
  let enc = { Array.enc } in
  Array.array (Array.map ~dec_empty ~dec_skip ~dec_add ~dec_finish ~enc t)

let update_nth ?stub ?absent n t =
  let update_elt n t v = Json.update t v in
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
          | None -> Error.out_of_range meta ~n ~len
          | Some absent ->
              let elt = update_elt (n, Meta.none) t (Json.null ()) in
              let stub = match stub with
              | None -> Json.stub elt | Some j -> j
              in
              Array (List.rev (elt :: list_repeat (n - len) stub acc), meta)
      end
  | j -> Json.error_sort ~exp:Sort.Array j
  in
  let dec = update ?stub ?absent n t in
  let enc j = j in
  map ~dec ~enc json

let set_nth ?stub ?(allow_absent = false) t n v =
  let absent = if allow_absent then Some (Json.null ()) else None in
  update_nth ?stub ?absent n (const t v)

let delete_nth ?(allow_absent = false) n =
  let dec_empty () = [] in
  let dec_add i v a = if i = n then a else (v :: a) in
  let dec_finish meta len a =
    if n < len || allow_absent then Json.list ~meta (List.rev a) else
    Error.out_of_range meta ~n ~len
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

(* Objects *)

let mem ?absent name t =
  Object.map Fun.id |> Object.mem ?dec_absent:absent name t ~enc:Fun.id
  |> Object.finish

let update_mem ?absent name t =
  let update_mem n t v = n, Json.update t v in
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
              Error.missing_mems meta ~object_kind:"" ~exp:[name] ~fnd
          | Some j ->
              let m = update_mem (name, Meta.none) t j in
              List.rev (m :: acc)
      in
      Object (mems, meta)
  | j -> Json.error_sort ~exp:Sort.Object j
  in
  let update = update ?absent name t in
  let enc j = j in
  map ~dec:update ~enc json

let set_mem ?(allow_absent = false) t name v =
  let absent = if allow_absent then Some Json.null' else None in
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
    then Error.missing_mems meta ~object_kind:"" ~exp:[name] ~fnd else
    Object (List.rev mems, meta)
  in
  Object.map' dec ~enc_meta
  |> Object.keep_unknown mems ~enc
  |> Object.finish

let delete_mem ?(allow_absent = false) name =
  let dec_add meta n v (ok, mems) =
    if n = name then true, mems else ok, ((n, meta), v) :: mems
  in
  let dec_finish (ok, ms as a) = if allow_absent then (true, ms) else a in
  update_json_object ~name ~dec_add ~dec_finish

let fold_object t f acc =
  let mems =
    let dec_empty () = acc and dec_add = f and dec_finish acc = acc in
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
    | Some (n', v') -> (true, ((n', meta), (Json.enc b v')) :: mems)
  in
  let dec_finish acc = acc in
  update_json_object ~name:"" (* irrelevant *) ~dec_add ~dec_finish

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
