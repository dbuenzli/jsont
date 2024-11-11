(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Jsont.Repr

(* Converting between Jsont.Error.t and Jv.Error.t values *)

let error_to_jv_error e = Jv.Error.v (Jstr.of_string (Jsont.Error.to_string e))
let jv_error_to_error e =
  let ctx = Jsont.Error.Context.empty and meta = Jsont.Meta.none in
  Jsont.Error.make_msg ctx meta (Jstr.to_string (Jv.Error.message e))

(* Browser JSON codec *)

let indent = Jstr.v "  "
let json = Jv.get Jv.global "JSON"
let json_parse s = Jv.call json "parse" [|Jv.of_jstr s|]
let json_stringify ~format v =
  let args = match format with
  | Jsont.Minify -> [| v |]
  | Jsont.Indent | Jsont.Layout -> [|v; Jv.null; Jv.of_jstr indent|]
  in
  Jv.to_jstr (Jv.call json "stringify" args)

(* Computing the sort of a Jv.t value *)

let type_bool = Jstr.v "boolean"
let type_object = Jstr.v "object"
let type_number = Jstr.v "number"
let type_string = Jstr.v "string"
let type_array = Jv.get Jv.global "Array"

let jv_sort jv =
  if Jv.is_null jv then Jsont.Sort.Null else
  let t = Jv.typeof jv in
  if Jstr.equal t type_bool then Jsont.Sort.Bool else
  if Jstr.equal t type_number then Jsont.Sort.Number else
  if Jstr.equal t type_string then Jsont.Sort.String else
  if Jstr.equal t type_object
  then (if Jv.is_array jv then Jsont.Sort.Array else Jsont.Sort.Object) else
  Jsont.Error.msgf Jsont.Meta.none "Not a JSON value: %s" (Jstr.to_string t)

(* Getting the members of a Jv.t object in various ways *)

let jv_mem_names jv = Jv.call (Jv.get Jv.global "Object") "keys" [| jv |]
let jv_mem_name_list jv = Jv.to_list Jv.to_string (jv_mem_names jv)
let jv_mem_name_map : Jv.t -> Jstr.t String_map.t = fun jv ->
  (* The map maps OCaml strings their corresponding JavaScript string *)
  let rec loop ns i max m =
    if i > max then m else
    let n = Jv.Jarray.get ns i in
    loop ns (i + 1) max (String_map.add (Jv.to_string n) (Jv.to_jstr n) m)
  in
  let ns = jv_mem_names jv in
  loop ns 0 (Jv.Jarray.length ns - 1) String_map.empty

(* Decoding *)

let error_push_array map i e =
  Jsont.Repr.error_push_array Jsont.Meta.none map (i, Jsont.Meta.none) e

let error_push_object map n e =
  Jsont.Repr.error_push_object Jsont.Meta.none map (n, Jsont.Meta.none) e

let type_error t ~fnd =
  Jsont.Repr.type_error Jsont.Meta.none t ~fnd

let find_all_unexpected ~mem_decs mems =
  let unexpected (n, _jname) = match String_map.find_opt n mem_decs with
  | None -> Some (n, Jsont.Meta.none) | Some _ -> None
  in
  List.filter_map unexpected mems

let rec decode : type a. a Jsont.Repr.t -> Jv.t -> a =
fun t jv -> match t with
| Null map ->
    (match jv_sort jv with
    | Null -> map.dec Jsont.Meta.none ()
    | fnd -> type_error t ~fnd)
| Bool map ->
    (match jv_sort jv with
    | Bool -> map.dec Jsont.Meta.none (Jv.to_bool jv)
    | fnd -> type_error t ~fnd)
| Number map ->
    (match jv_sort jv with
    | Number -> map.dec Jsont.Meta.none (Jv.to_float jv)
    | Null -> map.dec Jsont.Meta.none Float.nan
    | fnd -> type_error t ~fnd)
| String map ->
    (match jv_sort jv with
    | String -> map.dec Jsont.Meta.none (Jv.to_string jv)
    | fnd -> type_error t ~fnd)
| Array map ->
    (match jv_sort jv with
    | Array -> decode_array map jv
    | fnd -> type_error t ~fnd)
| Object map ->
    (match jv_sort jv with
    | Object -> decode_object map jv
    | fnd -> type_error t ~fnd)
| Map map -> map.dec (decode map.dom jv)
| Any map -> decode_any t map jv
| Rec t -> decode (Lazy.force t) jv

and decode_array :
  type a e b. (a, e, b) array_map -> Jv.t -> a
=
fun map jv ->
  let len = Jv.Jarray.length jv in
  let b = ref (map.dec_empty ()) in
  for i = 0 to len - 1 do
    try
      if map.dec_skip i !b then () else
      b := map.dec_add i (decode map.elt (Jv.Jarray.get jv i)) !b
    with Jsont.Error e -> error_push_array map i e
  done;
  map.dec_finish Jsont.Meta.none len !b

and decode_object : type o. (o, o) object_map -> Jv.t -> o =
fun map jv ->
  let names = jv_mem_name_map jv in
  let umems = Unknown_mems None in
  let dict = decode_object_map map umems String_map.empty Dict.empty names jv in
  apply_dict map.dec dict

and decode_object_map : type o.
  (o, o) object_map -> unknown_mems_option -> mem_dec String_map.t -> Dict.t ->
  Jstr.t String_map.t -> Jv.t -> Dict.t
=
fun map umems mem_decs dict names jv ->
  let u _ _ _ = assert false (* They should be disjoint by contruction *) in
  let mem_decs = String_map.union u mem_decs map.mem_decs in
  match map.shape with
  | Object_cases (umems', cases) ->
      let umems' = Unknown_mems umems' in
      let umems,dict = Jsont.Repr.override_unknown_mems ~by:umems umems' dict in
      decode_object_cases map umems cases mem_decs dict names jv
  | Object_basic umems' ->
      let umems' = Unknown_mems (Some umems') in
      let umems,dict = Jsont.Repr.override_unknown_mems ~by:umems umems' dict in
      match umems with
      | Unknown_mems (Some Unknown_skip | None) ->
          let u = Unknown_skip in
          decode_object_basic
            map u () mem_decs dict (String_map.bindings names) jv
      | Unknown_mems (Some (Unknown_error as u)) ->
          decode_object_basic
            map u () mem_decs dict (String_map.bindings names) jv
      | Unknown_mems (Some (Unknown_keep (umap, _) as u)) ->
          let umap = umap.dec_empty () and names = String_map.bindings names in
          decode_object_basic map u umap mem_decs dict names jv

and decode_object_basic : type o p m b.
  (o, o) object_map -> (p, m, b) unknown_mems -> b ->
  mem_dec String_map.t -> Dict.t -> (string * Jstr.t) list -> Jv.t -> Dict.t
=
fun map umems umap mem_decs dict names jv -> match names with
| [] ->
    Jsont.Repr.finish_object_decode map Jsont.Meta.none umems umap mem_decs dict
| (n, jname) :: names ->
    match String_map.find_opt n mem_decs with
    | Some (Mem_dec m) ->
        let dict =
          try Dict.add m.id (decode m.type' (Jv.get' jv jname)) dict with
          | Jsont.Error e -> error_push_object map n e
        in
        let mem_decs = String_map.remove n mem_decs in
        decode_object_basic map umems umap mem_decs dict names jv
    | None ->
        match umems with
        | Unknown_skip ->
            decode_object_basic map umems umap mem_decs dict names jv
        | Unknown_error ->
            let fnd =
              (n, Jsont.Meta.none) :: find_all_unexpected ~mem_decs names
            in
            Jsont.Repr.unexpected_mems_error Jsont.Meta.none map ~fnd
        | Unknown_keep (mmap, _) ->
            let umap =
              let v = try decode mmap.mems_type (Jv.get' jv jname) with
              | Jsont.Error e -> error_push_object map n e
              in
              mmap.dec_add Jsont.Meta.none n v umap
            in
            decode_object_basic map umems umap mem_decs dict names jv

and decode_object_cases : type o cs t.
  (o, o) object_map -> unknown_mems_option -> (o, cs, t) object_cases ->
  mem_dec String_map.t -> Dict.t -> Jstr.t String_map.t -> Jv.t -> Dict.t
=
fun map umems cases mem_decs dict names jv ->
  let decode_case_tag tag =
    let eq_tag (Case c) = cases.tag_compare c.tag tag = 0 in
    match List.find_opt eq_tag cases.cases with
    | None ->
        Jsont.Repr.unexpected_case_tag_error Jsont.Meta.none map cases tag
    | Some (Case case) ->
        let mems = String_map.remove cases.tag.name names in
        let dict =
          decode_object_map case.object_map umems mem_decs dict mems jv
        in
        Dict.add cases.id (case.dec (apply_dict case.object_map.dec dict)) dict
  in
  match String_map.find_opt cases.tag.name names with
  | Some jname ->
      (try decode_case_tag (decode cases.tag.type' (Jv.get' jv jname)) with
      | Jsont.Error e -> error_push_object map cases.tag.name e)
  | None ->
      match cases.tag.dec_absent with
      | Some tag -> decode_case_tag tag
      | None ->
          let exp = String_map.singleton cases.tag.name (Mem_dec cases.tag) in
          let fnd = jv_mem_name_list jv in
          Jsont.Repr.missing_mems_error Jsont.Meta.none map ~exp ~fnd

and decode_any : type a. a t -> a any_map -> Jv.t -> a =
fun t map jv ->
  let case t map sort jv = match map with
  | Some t -> decode t jv | None -> type_error t ~fnd:sort
  in
  match jv_sort jv with
  | Null as s -> case t map.dec_null s jv
  | Bool as s -> case t map.dec_bool s jv
  | Number as s -> case t map.dec_number s jv
  | String as s -> case t map.dec_string s jv
  | Array as s -> case t map.dec_array s jv
  | Object as s -> case t map.dec_object s jv

let decode t jv = decode (Jsont.Repr.of_t t) jv
let decode_jv' t jv = try Ok (decode t jv) with Jsont.Error e -> Error e
let decode_jv t jv = Result.map_error error_to_jv_error (decode_jv' t jv)
let decode' t s = try Ok (decode t (json_parse s)) with
| Jv.Error e -> Error (jv_error_to_error e) | Jsont.Error e -> Error e

let decode t json = Result.map_error error_to_jv_error (decode' t json)

(* Encoding *)

let rec encode : type a. a t -> a -> Jv.t =
fun t v -> match t with
| Null map -> map.enc v; Jv.null
| Bool map -> Jv.of_bool (map.enc v)
| Number map -> Jv.of_float (map.enc v)
| String map -> Jv.of_string (map.enc v)
| Array map ->
    let add map a i vi = try Jv.Jarray.set a i (encode map.elt vi); a with
    | Jsont.Error e -> error_push_array map i e
    in
    map.enc (add map) (Jv.Jarray.create 0) v
| Object map -> encode_object map ~do_unknown:true v (Jv.obj [||])
| Any map -> encode (map.enc v) v
| Map map -> encode map.dom (map.enc v)
| Rec t -> encode (Lazy.force t) v

and encode_object :
  type o. (o, o) Jsont.Repr.object_map -> do_unknown:bool -> o -> Jv.t -> Jv.t
=
fun map ~do_unknown o jv ->
  let encode_mem map o jv (Mem_enc mmap) =
    try
      let v = mmap.enc o in
      if mmap.enc_omit v then jv else
      (Jv.set' jv (Jstr.of_string mmap.name) (encode mmap.type' v); jv)
    with
    | Jsont.Error e -> error_push_object map mmap.name e
  in
  let jv = List.fold_left (encode_mem map o) jv map.mem_encs in
  match map.shape with
  | Object_basic (Unknown_keep (umap, enc)) when do_unknown ->
      encode_unknown_mems map umap (enc o) jv
  | Object_basic _ -> jv
  | Object_cases (u, cases) ->
      let Case_value (case, v) = cases.enc_case (cases.enc o) in
      let jv =
        try
          if cases.tag.enc_omit case.tag then jv else
          let tag = encode cases.tag.type' case.tag in
          Jv.set' jv (Jstr.of_string cases.tag.name) tag; jv
        with
        | Jsont.Error e -> error_push_object map cases.tag.name e
      in
      match u with
      | Some (Unknown_keep (umap, enc)) ->
          (* Feels nicer to encode unknowns at the end *)
          let jv = encode_object case.object_map ~do_unknown:false v jv in
          encode_unknown_mems map umap (enc o) jv
      | _ -> encode_object case.object_map ~do_unknown v jv

and encode_unknown_mems : type o mems a builder.
  (o, o) object_map -> (mems, a, builder) mems_map -> mems -> Jv.t -> Jv.t =
fun map umap mems jv ->
  let encode_mem map meta name v jv =
    try Jv.set' jv (Jstr.of_string name) (encode umap.mems_type v); jv with
    | Jsont.Error e -> error_push_object map name e
  in
  umap.enc (encode_mem map) mems jv

let encode t v = encode (Jsont.Repr.of_t t) v
let encode_jv' t v = try Ok (encode t v) with Jsont.Error e -> Error e
let encode_jv t v = Result.map_error error_to_jv_error (encode_jv' t v)
let encode' ?(format = Jsont.Minify) t v =
  try Ok (json_stringify ~format (encode t v)) with
  | Jv.Error e -> Error (jv_error_to_error e)
  | Jsont.Error e -> Error e

let encode ?format t v =
  Result.map_error error_to_jv_error (encode' ?format t v)

(* Recode *)

let recode ?format t s = match decode t s with
| Error _ as e -> e | Ok v -> encode ?format t v

let recode' ?format t s = match decode' t s with
| Error _ as e -> e | Ok v -> encode' ?format t v

let recode_jv t jv = match decode_jv t jv with
| Error _ as e -> e | Ok v -> encode_jv t v

let recode_jv' t s = match decode_jv' t s with
| Error _ as e -> e | Ok v -> encode_jv' t v
