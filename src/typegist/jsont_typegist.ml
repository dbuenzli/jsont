(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Typegist

let invalid_argf fmt =
  Format.ksprintf invalid_arg ("Jsont_typegist.to_jsont: " ^^ fmt)

let invalid_arg_can't_ignore kind s =
  invalid_argf "%s %s cannot ignore without a Type.Gist.Default value" s kind

(* Metadata *)

module Mem_map = struct
  type ('a, 'b) t = ('a, 'b) Jsont.Object.Mem.map
  let ignore name v =
    let dec_absent = v in
    let enc _ = v in
    let enc_omit _ = true in
    Jsont.Object.Mem.map ~dec_absent ~enc ~enc_omit name (Jsont.null v)

  module V = struct type nonrec ('a, 'b) t = ('a, 'b) t end
  include Type.Gist.Meta.Key (V)
end

module Mjsont = struct
  type 'a t = 'a Jsont.t
  let ignore v = Jsont.null v
  module V = struct type ('a, 'b) t = 'b Jsont.t end
  include Type.Gist.Meta.Key (V)
end

(* Deriving *)

type 'a derive = 'a Type.Gist.t -> 'a Jsont.t

let nativeint_jsont ~kind ?doc () =
  let dec = Int64.to_nativeint and enc = Int64.of_nativeint in
  Jsont.map ~kind ?doc ~dec ~enc Jsont.int64

let char_jsont ~kind ?doc () =
  let err_exp ~fnd = Jsont.Error.expected Jsont.Meta.none "one hex byte" ~fnd in
  let dec = function
  | "" -> err_exp ~fnd:{|""|}
  | s when String.length s > 1 -> err_exp ~fnd:"multiple bytes"
  | s -> s.[0]
  in
  let enc c = String.make 1 c in
  Jsont.map ~kind ?doc ~dec ~enc Jsont.binary_string

let uchar_jsont ~kind ?doc () =
  let err_exp meta ~fnd =
    Jsont.Error.expected meta "one Unicode character" ~fnd
  in
  let dec meta = function
  | "" -> err_exp meta ~fnd:{|""|}
  | s ->
      (* N.B. Jsont guarantees UTF-8 validity on input. *)
      let dec = String.get_utf_8_uchar s 0 in
      let len = Uchar.utf_decode_length dec in
      if len <> String.length s
      then err_exp meta ~fnd:"multiple characters"
      else Uchar.utf_decode_uchar dec
  in
  let enc u =
    let b = Bytes.create (Uchar.utf_8_byte_length u) in
    ignore (Bytes.set_utf_8_uchar b 0 u);
    Bytes.unsafe_to_string b
  in
  Jsont.Base.string (Jsont.Base.map ~kind ?doc ~dec ~enc ())

let string_encoding_jsont = function
| Type.Gist.Array_like.Utf_8 -> Jsont.string
| Type.Gist.Array_like.Binary -> Jsont.binary_string

let map_like_binding_jsont key value  =
  Jsont.Object.map (fun k v -> k, v)
  |> Jsont.Object.mem "key" key ~enc:fst
  |> Jsont.Object.mem "value" value ~enc:snd
  |> Jsont.Object.finish

let abstract_value_object jsont =
  Jsont.Object.map Fun.id
  |> Jsont.Object.mem "value" jsont ~enc:Fun.id
  |> Jsont.Object.finish

let rec field_mem_map :
type p v. int -> (p, v) Type.Gist.Field.t -> (p, v) Jsont.Object.Mem.map =
fun n f ->
  let meta = Type.Gist.Field.meta f in
  match Mem_map.find meta with
  | Some map -> map
  | None ->
      let doc = Type.Gist.Field.doc f in
      let name = match Type.Gist.Field.name f with
      | "" -> Int.to_string n | name -> name
      in
      match Type.Gist.Meta.Ignore.find meta with
      | Some true ->
          begin match Type.Gist.Meta.Default.find meta with
          | None -> invalid_arg_can't_ignore name "field"
          | Some v ->
              let dec_absent = v in
              let enc_omit _ = true in
              let enc _ = v in
              let value_jsont = Jsont.null v in
              Jsont.Object.Mem.map
                ~doc ~dec_absent ~enc ~enc_omit name value_jsont
          end
      | None | Some false ->
          let enc = Type.Gist.Field.get f in
          let value_jsont = to_jsont (Type.Gist.Field.gist f) in
          Jsont.Object.Mem.map ~doc ~enc name value_jsont

and fields_jsont :
type o a. kind:string -> doc:string -> int -> (o, a) Type.Gist.Product.fields ->
  (o, a) Jsont.Object.map =
fun ~kind ~doc n fields -> match fields with
| Ctor f -> Jsont.Object.map ~kind ~doc f
| App (fs, f) ->
    let fields = fields_jsont ~kind ~doc (n - 1) fs in
    Jsont.Object.Mem.app fields (field_mem_map n f)

and product_jsont : type p. p Type.Gist.Product.t -> p derive =
fun p g ->
  let kind = Type.Gist.name g and doc = Type.Gist.doc g in
  let field_count = Type.Gist.Product.field_count p in
  fields_jsont ~kind ~doc (field_count - 1) p |> Jsont.Object.finish

and scalar_jsont : type a. a Type.Gist.Scalar.t -> a derive = fun s g ->
  let kind = Type.Gist.name g and doc = Type.Gist.doc g in
  match s with
  | Unit -> Jsont.null ~kind ~doc ()
  | Bool -> Jsont.with_doc ~kind ~doc Jsont.bool
  | Char -> char_jsont ~kind ~doc ()
  | Uchar -> uchar_jsont ~kind ~doc ()
  | Int -> Jsont.with_doc ~kind ~doc Jsont.int
  | Int32 -> Jsont.with_doc ~kind ~doc Jsont.int32
  | Int64 -> Jsont.with_doc ~kind ~doc Jsont.int64
  | Nativeint -> nativeint_jsont ~kind ~doc ()
  | Float -> Jsont.with_doc ~kind ~doc Jsont.any_float

and variant_like_jsont :
type v. v Type.Gist.Variant_like.t -> v derive =
fun variant g ->
   let kind = Type.Gist.name g and doc = Type.Gist.doc g in
   match variant with
   | Option t -> Jsont.option ~kind ~doc (to_jsont t)
   | List t -> Jsont.list ~kind ~doc (to_jsont t)
   | variant ->
       let variant = Type.Gist.Variant_like.to_variant variant in
       let variant_case_jsont c =
         let tag = Type.Gist.Variant.Case.name c in
         let doc = Type.Gist.Variant.Case.doc c in
         let product = Type.Gist.Variant.Case.product c in
         let case =
           let field_count = Type.Gist.Product.field_count product in
           fields_jsont ~kind:tag ~doc (field_count - 1) product
           |> Jsont.Object.finish
         in
         Jsont.Object.Case.map ~dec:Fun.id tag case
       in
       let cases = Type.Gist.Variant.cases variant in
       let cases = Iarray.map variant_case_jsont cases in
       let cmap =
         let add_case m mc =
           let tag = Jsont.Object.Case.map_tag mc in
           Jsont.Repr.String_map.add tag mc m
         in
         Iarray.fold_left add_case Jsont.Repr.String_map.empty cases
       in
       let enc_case value =
         let case = Type.Gist.Variant.case variant value in
         let tag = Type.Gist.Variant.Case.name case in
         let case = Option.get (Jsont.Repr.String_map.find_opt tag cmap) in
         Jsont.Object.Case.value case value
       in
       let cases = Iarray.(to_list (map Jsont.Object.Case.make cases)) in
       let tag_compare = String.compare and tag_to_string = Fun.id in
       let case_mem =
         Jsont.Object.case_mem "case"
           Jsont.string ~tag_compare ~tag_to_string ~enc:Fun.id ~enc_case cases
       in
       Jsont.Object.map ~kind ~doc Fun.id |> case_mem |> Jsont.Object.finish

and array_like_jsont : type elt a. (elt, a) Type.Gist.Array_like.t -> a derive =
fun a g ->
  let kind = Type.Gist.name g and doc = Type.Gist.doc g in
  match a with
  | String encoding ->
      Jsont.with_doc ~kind ~doc (string_encoding_jsont encoding)
  | Bytes encoding ->
      let dec = Bytes.unsafe_of_string and enc = Bytes.unsafe_to_string in
      Jsont.map ~kind ~doc ~dec ~enc @@ string_encoding_jsont encoding
  | Array elt -> Jsont.array ~kind ~doc (to_jsont elt)
  | Iarray elt ->
      (* These externals from the upstream implementation of iarrays *)
      let external unsafe_of_array : 'a array -> 'a iarray = "%opaque" in
      let external unsafe_to_array : 'a iarray -> 'a array = "%opaque" in
      let dec = unsafe_of_array and enc = unsafe_to_array in
      Jsont.map ~kind ~doc ~dec ~enc @@ Jsont.array ~kind ~doc (to_jsont elt)
  | Bigarray1 (elt, k, l) ->
      Jsont.Array.(array @@ bigarray_map ~kind ~doc k l (to_jsont elt))
  | Module (elt, (module A)) ->
      let dec a = A.init (Array.length a) (fun i -> a.(i)) in
      let enc a = Array.init (A.length a) (fun i -> A.get a i) in
      Jsont.map ~kind ~doc ~dec ~enc @@ Jsont.array ~kind ~doc (to_jsont elt)

and map_like_jsont :
type k v m. (k, v, m) Type.Gist.Map_like.t -> m derive =
fun m g ->
  let kind = Type.Gist.name g and doc = Type.Gist.doc g in
  match m with
  | Map_module (key, value, (module M)) ->
      let value_jsont = to_jsont value in
      begin match Type.Gist.expr key with
      | Array_like (String encoding) -> (* object as key/value map *)
          let mems =
            let dec_empty () = M.empty in
            let dec_add = match encoding with
            | Utf_8 -> fun _jmeta k v m -> M.add k v m
            | Binary -> fun jmeta k v m ->
              match Jsont_base.binary_string_of_hex k with
              | Error msg -> Jsont.Error.msg jmeta msg
              | Ok k -> M.add k v m
            in
            let dec_finish _jmeta m = m in
            let enc = match encoding with
            | Utf_8 -> fun f -> M.fold (f Jsont.Meta.none)
            | Binary -> fun f ->
              M.fold (fun k ->
                  f Jsont.Meta.none (Jsont_base.binary_string_to_hex k))
            in
            Jsont.Object.Mems.map
              ~dec_empty ~dec_add ~dec_finish ~enc:{enc} value_jsont
          in
          Jsont.Object.map ~doc ~kind Fun.id
          |> Jsont.Object.keep_unknown mems ~enc:Fun.id
          |> Jsont.Object.finish
      |  _ ->
          (* array of key/value objs *)
          let dec_empty () = M.empty in
          let dec_add _ (k, v) m = M.add k v m in
          let dec_finish _jmeta _i m = m in
          let enc f acc m =
            let add k v (acc, i) = f acc i (k, v), i + 1 in
            fst (M.fold add m (acc, 0))
          in
          Jsont.Array.array @@
          Jsont.Array.map ~kind ~doc ~dec_empty ~dec_add ~dec_finish ~enc:{enc}
            (map_like_binding_jsont (to_jsont key) value_jsont)
      end
  | Hashtbl_module (key, value, (module H)) ->
      let value_jsont = to_jsont value in
      begin match Type.Gist.expr key with
      | Array_like (String encoding) -> (* object as key/value map *)
          let mems =
            let dec_empty () = H.create 10 in
            let dec_add = match encoding with
            | Utf_8 -> fun _jmeta k v m -> H.add m k v; m
            | Binary -> fun jmeta k v m ->
              match Jsont_base.binary_string_of_hex k with
              | Error msg -> Jsont.Error.msg jmeta msg
              | Ok k -> H.add m k v; m
            in
            let dec_finish _jmeta h = h in
            let enc = match encoding with
            | Utf_8 -> fun f -> H.fold (f Jsont.Meta.none)
            | Binary -> fun f ->
              H.fold (fun k ->
                  f Jsont.Meta.none (Jsont_base.binary_string_to_hex k))
            in
            Jsont.Object.Mems.map
              ~kind ~doc ~dec_empty ~dec_add ~dec_finish ~enc:{enc} value_jsont
          in
          Jsont.Object.map ~kind ~doc Fun.id
          |> Jsont.Object.keep_unknown ~enc:Fun.id mems
          |> Jsont.Object.finish
      | _ ->
          (* array of key/value objs *)
          let dec_empty () = H.create 10 in
          let dec_add _ (k, v) h = H.add h k v; h in
          let dec_finish _jmeta _i h = h in
          let enc f acc m =
            let add k v (acc, i) = f acc i (k, v), i + 1 in
            fst (H.fold add m (acc, 0))
          in
          Jsont.Array.array @@
          Jsont.Array.map ~kind ~doc ~dec_empty ~dec_add ~dec_finish ~enc:{enc}
            (map_like_binding_jsont (to_jsont key) value_jsont)
      end

and cell_like_jsont :
type a cell. (a, cell) Type.Gist.Cell_like.t -> cell derive =
fun c g ->
  let kind = Type.Gist.name g and doc = Type.Gist.doc g in
  let (module Cell) = Type.Gist.Cell_like.to_module c in
  let contents = Type.Gist.Cell_like.contents c in
  let dec v = Cell.make v and enc v = Cell.get v in
  Jsont.map ~kind ~doc ~dec ~enc (to_jsont contents)

and func_jsont : type a b. (a, b) Type.Gist.Func.t -> (a -> b) derive =
fun func -> invalid_argf "functional value"

and abstract_jsont : type a. a Type.Gist.Abstract.t -> a derive =
fun a g ->
  Type.Gist.Abstract.check_non_opaque g a;
  let kind = Type.Gist.name g and doc = Type.Gist.doc g in
  let case_map_of_version version =
    let gist = Type.Gist.Abstract.Version.gist version in
    let version_jsont = abstract_value_object (to_jsont gist) in
    let tag = Type.Gist.Abstract.Version.name version in
    Jsont.Object.Case.map ~dec:Fun.id tag version_jsont
  in
  let versions = Type.Gist.Abstract.versions a in
  let case_maps = Iarray.map case_map_of_version versions in
  let enc_case v =
    let case = Iarray.get case_maps (Type.Gist.Abstract.version_index a v) in
    Jsont.Object.Case.value case v
  in
  let cases = Iarray.map Jsont.Object.Case.make case_maps in
  let tag_compare = String.compare and tag_to_string = Fun.id in
  Jsont.Object.map ~kind ~doc Fun.id
  |> Jsont.Object.case_mem "version" Jsont.string ~tag_compare
    ~tag_to_string ~enc:Fun.id ~enc_case (Iarray.to_list cases)
  |> Jsont.Object.finish

and view_jsont : type a b. (a, b) Type.Gist.View.t -> a derive =
fun w g ->
  let kind = Type.Gist.name g and doc = Type.Gist.doc g in
  let dec = Type.Gist.View.project w and enc = Type.Gist.View.inject w in
  Jsont.map ~kind ~doc ~dec ~enc (to_jsont (Type.Gist.View.gist w))

and to_jsont : type a. a Type.Gist.t -> a Jsont.t = fun g ->
  let meta = Type.Gist.meta g in
  match Mjsont.find meta with
  | Some jsont -> jsont
  | None ->
      match Type.Gist.Meta.Ignore.find meta with
      | Some true ->
          let kind = Type.Gist.name g and doc = Type.Gist.doc g in
          begin match Type.Gist.Meta.Default.find meta with
          | Some v -> Jsont.null ~kind ~doc v
          | None -> invalid_arg_can't_ignore kind "gist"
          end
      | None | Some false ->
          match Type.Gist.expr g with
          | Scalar s -> scalar_jsont s g
          | Tuple t -> product_jsont t g
          | Record r -> product_jsont r g
          | Variant_like v -> variant_like_jsont v g
          | Array_like a -> array_like_jsont a g
          | Map_like m -> map_like_jsont m g
          | Cell_like c -> cell_like_jsont c g
          | Func f -> func_jsont f g
          | Abstract a -> abstract_jsont a g
          | View w -> view_jsont w g
          | Rec (lazy g) -> Jsont.rec' (lazy (to_jsont g))

let derivation_object jsont =
  Jsont.Object.map (fun _version value -> value)
  |> Jsont.Object.mem "version" Jsont.string ~enc:(fun _ -> "1")
  |> Jsont.Object.mem "value" jsont ~enc:Fun.id
  |> Jsont.Object.finish

let to_jsont ~versioned g =
  if not versioned then to_jsont g else derivation_object (to_jsont g)

module Jsont = Mjsont
