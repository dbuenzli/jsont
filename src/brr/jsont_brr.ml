(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Jsont.Repr

(* Converting between Jsont.Error.t and Jv.Error.t values *)

let error_to_jv_error e = Jv.Error.v (Jstr.of_string (Jsont.Error.to_string e))
let jv_error_to_error e =
  let ctx = Jsont.Context.make Jsont.Path.root and meta = Jsont.Meta.none in
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

let jv_sort ctx jv =
  if Jv.is_null jv then Jsont.Sort.Null else
  let t = Jv.typeof jv in
  if Jstr.equal t type_bool then Jsont.Sort.Bool else
  if Jstr.equal t type_number then Jsont.Sort.Number else
  if Jstr.equal t type_string then Jsont.Sort.String else
  if Jstr.equal t type_object
  then (if Jv.is_array jv then Jsont.Sort.Array else Jsont.Sort.Obj) else
  Jsont.Error.msgf ctx Jsont.Meta.none "Not a JSON value: %s" (Jstr.to_string t)

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

let find_all_unexpected ~mem_decs mems =
  let unexpected (n, _jname) = match String_map.find_opt n mem_decs with
  | None -> Some (n, Jsont.Meta.none) | Some _ -> None
  in
  List.filter_map unexpected mems

let rec decode : type a. Jsont.Context.t -> a Jsont.Repr.t -> Jv.t -> a =
fun ctx t jv -> match t with
| Null map ->
    (match jv_sort ctx jv with
    | Null -> map.dec ctx Jsont.Meta.none ()
    | fnd -> Jsont.Repr.type_error ctx Jsont.Meta.none t ~fnd)
| Bool map ->
    (match jv_sort ctx jv with
    | Bool -> map.dec ctx Jsont.Meta.none (Jv.to_bool jv)
    | fnd -> Jsont.Repr.type_error ctx Jsont.Meta.none t ~fnd)
| Number map ->
    (match jv_sort ctx jv with
    | Number -> map.dec ctx Jsont.Meta.none (Jv.to_float jv)
    | Null -> map.dec ctx Jsont.Meta.none Float.nan
    | fnd -> Jsont.Repr.type_error ctx Jsont.Meta.none t ~fnd)
| String map ->
    (match jv_sort ctx jv with
    | String -> map.dec ctx Jsont.Meta.none (Jv.to_string jv)
    | fnd -> Jsont.Repr.type_error ctx Jsont.Meta.none t ~fnd)
| Array map ->
    (match jv_sort ctx jv with
    | Array -> decode_array ctx map jv
    | fnd -> Jsont.Repr.type_error ctx Jsont.Meta.none t ~fnd)
| Obj map ->
    (match jv_sort ctx jv with
    | Obj -> decode_obj ctx map jv
    | fnd -> Jsont.Repr.type_error ctx Jsont.Meta.none t ~fnd)
| Map map -> map.dec ctx (decode ctx map.dom jv)
| Any map -> decode_any ctx t map jv
| Rec t -> decode ctx (Lazy.force t) jv

and decode_array :
  type a e b. Jsont.Context.t -> (a, e, b) array_map -> Jv.t -> a
=
fun ctx map jv ->
  let len = Jv.Jarray.length jv in
  let b = ref (map.dec_empty ctx) in
  for i = 0 to len - 1 do
    if map.dec_skip ctx i !b then () else
    let ctxi = Jsont.Repr.push_nth map i Jsont.Meta.none ctx in
    b := map.dec_add ctx i (decode ctxi map.elt (Jv.Jarray.get jv i)) !b
  done;
  map.dec_finish ctx Jsont.Meta.none len !b

and decode_obj : type o. Jsont.Context.t -> (o, o) obj_map -> Jv.t -> o =
fun ctx map jv ->
  let dict = Dict.empty in
  let dict = Dict.add Jsont.Repr.obj_context_arg ctx dict in
  let dict = Dict.add Jsont.Repr.obj_meta_arg Jsont.Meta.none dict in
  let names = jv_mem_name_map jv in
  apply_dict map.dec
    (decode_obj_map ctx map (Unknown_mems None) String_map.empty dict names jv)

and decode_obj_map : type o.
  Jsont.Context.t -> (o, o) obj_map -> unknown_mems_option ->
  mem_dec String_map.t -> Dict.t -> Jstr.t String_map.t -> Jv.t -> Dict.t
=
fun ctx map umems mem_decs dict names jv ->
  let u _ _ _ = assert false (* They should be disjoint by contruction *) in
  let mem_decs = String_map.union u mem_decs map.mem_decs in
  match map.shape with
  | Obj_cases (umems', cases) ->
      let umems, dict =
        Jsont.Repr.override_unknown_mems ctx ~by:umems (Unknown_mems umems')
          dict
      in
      decode_obj_cases ctx map umems cases mem_decs dict names jv
  | Obj_basic umems' ->
      let umems, dict =
        Jsont.Repr.override_unknown_mems ctx ~by:umems
          (Unknown_mems (Some umems')) dict
      in
      match umems with
      | Unknown_mems (Some Unknown_skip | None) ->
          decode_obj_basic
            ctx map Unknown_skip () mem_decs dict (String_map.bindings names) jv
      | Unknown_mems (Some (Unknown_error as u)) ->
          decode_obj_basic
            ctx map u () mem_decs dict (String_map.bindings names) jv
      | Unknown_mems (Some (Unknown_keep (umap, _) as u)) ->
          let umap = umap.dec_empty ctx and names = String_map.bindings names in
          decode_obj_basic ctx map u umap mem_decs dict names jv

and decode_obj_basic : type o p m b.
  Jsont.Context.t -> (o, o) obj_map -> (p, m, b) unknown_mems -> b ->
  mem_dec String_map.t -> Dict.t -> (string * Jstr.t) list -> Jv.t -> Dict.t
=
fun ctx map u umap mem_decs dict names jv -> match names with
| [] ->
    let dict = match u with
    | Unknown_skip | Unknown_error -> dict
    | Unknown_keep (map, _) -> Dict.add map.id (map.dec_finish ctx umap) dict
    in
    let add_default _ (Mem_dec mem_map) dict = match mem_map.dec_absent with
    | Some v -> Dict.add mem_map.id v dict
    | None -> raise Exit
    in
    (try String_map.fold add_default mem_decs dict with
    | Exit ->
        let exp = mem_decs and fnd = jv_mem_name_list jv in
        Jsont.Repr.missing_mems_error ctx Jsont.Meta.none map ~exp ~fnd)
| (n, jname) :: names ->
    match String_map.find_opt n mem_decs with
    | Some (Mem_dec m) ->
        let dict =
          let ctx = Jsont.Repr.push_mem map n Jsont.Meta.none ctx in
          Dict.add m.id (decode ctx m.type' (Jv.get' jv jname)) dict
        in
        let mem_decs = String_map.remove n mem_decs in
        decode_obj_basic ctx map u umap mem_decs dict names jv
    | None ->
        match u with
        | Unknown_skip -> decode_obj_basic ctx map u umap mem_decs dict names jv
        | Unknown_error ->
            let fnd =
              (n, Jsont.Meta.none) :: find_all_unexpected ~mem_decs names
            in
            Jsont.Repr.unexpected_mems_error ctx Jsont.Meta.none map ~fnd
        | Unknown_keep (mmap, _) ->
            let umap =
              let v =
                let ctx = Jsont.Repr.push_mem map n Jsont.Meta.none ctx in
                decode ctx mmap.mems_type (Jv.get' jv jname)
              in
              mmap.dec_add ctx Jsont.Meta.none n v umap
            in
            decode_obj_basic ctx map u umap mem_decs dict names jv

and decode_obj_cases : type o cs t.
  Jsont.Context.t -> (o, o) obj_map -> unknown_mems_option ->
  (o, cs, t) obj_cases -> mem_dec String_map.t -> Dict.t ->
  Jstr.t String_map.t -> Jv.t -> Dict.t
=
fun ctx map umems cases mem_decs dict names jv ->
  let decode_case_tag tag =
    let eq_tag (Case c) = cases.tag_compare c.tag tag = 0 in
    match List.find_opt eq_tag cases.cases with
    | None ->
        let meta = Jsont.Meta.none in
        Jsont.Repr.unexpected_case_tag_error ctx meta map cases tag
    | Some (Case case) ->
        let mems = String_map.remove cases.tag.name names in
        let dict =
          decode_obj_map ctx case.obj_map umems mem_decs dict mems jv
        in
        Dict.add cases.id (case.dec (apply_dict case.obj_map.dec dict)) dict
  in
  match String_map.find_opt cases.tag.name names with
  | Some jname ->
      let ctx = Jsont.Repr.push_mem map cases.tag.name Jsont.Meta.none ctx in
      decode_case_tag (decode ctx cases.tag.type' (Jv.get' jv jname))
  | None ->
      match cases.tag.dec_absent with
      | Some tag -> decode_case_tag tag
      | None ->
          let meta = Jsont.Meta.none in
          let obj_kind = Jsont.Repr.obj_kind ~kind:map.kind in
          Jsont.Error.missing_mems ctx meta ~obj_kind
            ~exp:[cases.tag.name]
            ~fnd:(jv_mem_name_list jv)

and decode_any : type a. Jsont.Context.t -> a t -> a any_map -> Jv.t -> a =
fun ctx t map jv ->
  let case ctx t map sort jv = match map with
  | None -> Jsont.Repr.type_error ctx Jsont.Meta.none t ~fnd:sort
  | Some t -> decode ctx t jv
  in
  match jv_sort ctx jv with
  | Null as s -> case ctx t map.dec_null s jv
  | Bool as s -> case ctx t map.dec_bool s jv
  | Number as s -> case ctx t map.dec_number s jv
  | String as s -> case ctx t map.dec_string s jv
  | Array as s -> case ctx t map.dec_array s jv
  | Obj as s -> case ctx t map.dec_obj s jv

let dec ?(ctx = Jsont.Context.root) t jv = decode ctx (Jsont.Repr.of_t t) jv
let decode_jv' ?ctx t jv = try Ok (dec ?ctx t jv) with Jsont.Error e -> Error e

let decode_jv ?ctx t jv =
  Result.map_error error_to_jv_error (decode_jv' ?ctx t jv)

let decode' ?ctx t s = try Ok (dec ?ctx t (json_parse s)) with
| Jv.Error e -> Error (jv_error_to_error e)
| Jsont.Error e -> Error e

let decode ?ctx t json =
  Result.map_error error_to_jv_error (decode' ?ctx t json)

(* Encoding *)

let rec encode : type a. Jsont.Context.t -> a t -> a -> Jv.t =
fun ctx t v -> match t with
| Null map -> map.enc ctx v; Jv.null
| Bool map -> Jv.of_bool (map.enc ctx v)
| Number map -> Jv.of_float (map.enc ctx v)
| String map -> Jv.of_string (map.enc ctx v)
| Array map ->
    let add ctx map a i vi =
      let ctxi = Jsont.Repr.push_nth map i Jsont.Meta.none ctx in
      Jv.Jarray.set a i (encode ctxi map.elt vi); a
    in
    map.enc ctx (add ctx map) (Jv.Jarray.create 0) v
| Obj map -> encode_obj ctx map ~do_unknown:true v (Jv.obj [||])
| Any map -> encode ctx (map.enc ctx v) v
| Map map -> encode ctx map.dom (map.enc ctx v)
| Rec t -> encode ctx (Lazy.force t) v

and encode_obj :
  type o. Jsont.Context.t -> (o, o) Jsont.Repr.obj_map -> do_unknown:bool ->
  o -> Jv.t -> Jv.t
=
fun ctx map ~do_unknown o jv ->
  let encode_mem ctx map o jv (Mem_enc mmap) =
    let ctx = Jsont.Repr.push_mem map mmap.name Jsont.Meta.none ctx in
    let v = mmap.enc ctx o in
    if mmap.enc_omit ctx v then jv else
    (Jv.set' jv (Jstr.of_string mmap.name) (encode ctx mmap.type' v); jv)
  in
  let jv = List.fold_left (encode_mem ctx map o) jv map.mem_encs in
  match map.shape with
  | Obj_basic (Unknown_keep (umap, enc)) when do_unknown ->
      encode_unknown_mems ctx map umap (enc o) jv
  | Obj_basic _ -> jv
  | Obj_cases (u, cases) ->
      let Case_value (case, c) = cases.enc_case ctx (cases.enc o) in
      let jv =
        let ctxn = Jsont.Repr.push_mem map cases.tag.name Jsont.Meta.none ctx in
        if cases.tag.enc_omit ctxn case.tag then jv else
        let tag = encode ctxn cases.tag.type' case.tag in
        Jv.set' jv (Jstr.of_string cases.tag.name) tag; jv
      in
      match u with
      | Some (Unknown_keep (umap, enc)) ->
          (* Feels nicer to encode unknowns at the end *)
          let jv = encode_obj ctx case.obj_map ~do_unknown:false c jv in
          encode_unknown_mems ctx map umap (enc o) jv
      | _ -> encode_obj ctx case.obj_map ~do_unknown c jv

and encode_unknown_mems : type o mems a builder.
  Jsont.Context.t -> (o, o) obj_map -> (mems, a, builder) mems_map ->
  mems -> Jv.t -> Jv.t =
fun ctx map umap mems jv ->
  let encode_mem map meta name v jv =
    let ctxn = Jsont.Repr.push_mem map name Jsont.Meta.none ctx in
    Jv.set' jv (Jstr.of_string name) (encode ctxn umap.mems_type v); jv
  in
  umap.enc ctx (encode_mem map) mems jv

let enc ?(ctx = Jsont.Context.root) t v = encode ctx (Jsont.Repr.of_t t) v
let encode_jv' ?ctx t v = try Ok (enc ?ctx t v) with Jsont.Error e -> Error e
let encode_jv ?ctx t v =
  Result.map_error error_to_jv_error (encode_jv' ?ctx t v)

let encode' ?ctx ?(format = Jsont.Minify) t v =
  try Ok (json_stringify ~format (enc ?ctx t v)) with
  | Jv.Error e -> Error (jv_error_to_error e)
  | Jsont.Error e -> Error e

let encode ?ctx ?format t v =
  Result.map_error error_to_jv_error (encode' ?ctx ?format t v)

(* Recode *)

let recode ?ctx ?format t s = match decode ?ctx t s with
| Error _ as e -> e | Ok v -> encode ?ctx ?format t v

let recode' ?ctx ?format t s = match decode' ?ctx t s with
| Error _ as e -> e | Ok v -> encode' ?ctx ?format t v

let recode_jv ?ctx t jv = match decode_jv ?ctx t jv with
| Error _ as e -> e | Ok v -> encode_jv ?ctx t v

let recode_jv' ?ctx t s = match decode_jv' ?ctx t s with
| Error _ as e -> e | Ok v -> encode_jv' ?ctx t v
