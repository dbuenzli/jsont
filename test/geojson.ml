(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* GeoJSON codec https://datatracker.ietf.org/doc/html/rfc7946

   Note: a few length constraints on arrays should be checked,
   a combinators should be added for that.

   In contrast to Topojson the structure is a bit more annoying to
   model because there is subtyping on the "type" field: GeoJSON
   objects can be Feature, FeatureCollection or any Geometry object
   and Geometry objects are recursive on themselves (but not on
   Feature or Feature collection) and FeatureCollection only have
   Feature objects. We handle this by redoing the cases to handle only
   the subsets. *)

type float_array = float array
let float_array_jsont ~kind = Jsont.array ~kind Jsont.number

type 'a garray = 'a array
let garray = Jsont.array

module Bbox = struct
  type t = float_array
  let jsont = float_array_jsont ~kind:"Bbox"
end

module Position = struct
  type t = float_array
  let jsont = float_array_jsont ~kind:"Position"
end

module Geojson_object = struct
  type 'a t =
    { type' : 'a;
      bbox : Bbox.t option;
      unknown : Jsont.json }

  let make type' bbox unknown = { type'; bbox; unknown }
  let type' o = o.type'
  let bbox o = o.bbox
  let unknown o = o.unknown

  let finish_jsont map =
    map
    |> Jsont.Object.opt_mem "bbox" Bbox.jsont ~enc:bbox
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:unknown
    |> Jsont.Object.finish

  let geometry ~kind coordinates =
    Jsont.Object.map ~kind make
    |> Jsont.Object.mem "coordinates" coordinates ~enc:type'
    |> finish_jsont
end

module Point = struct
  type t = Position.t
  let jsont = Geojson_object.geometry ~kind:"Point" Position.jsont
end

module Multi_point = struct
  type t = Position.t garray
  let jsont =
    Geojson_object.geometry ~kind:"MultiPoint" (garray Position.jsont)
end

module Line_string = struct
  type t = Position.t garray
  let jsont =
    Geojson_object.geometry ~kind:"LineString" (garray Position.jsont)
end

module Multi_line_string = struct
  type t = Line_string.t garray
  let jsont =
    Geojson_object.geometry ~kind:"LineString" (garray (garray Position.jsont))
end

module Polygon = struct
  type t = Line_string.t garray
  let jsont =
    Geojson_object.geometry ~kind:"Polygon" (garray (garray Position.jsont))
end

module Multi_polygon = struct
  type t = Polygon.t garray
  let jsont =
    Geojson_object.geometry ~kind:"MultiPolygon"
      (garray (garray (garray Position.jsont)))
end

module Geojson = struct
  type 'a object' = 'a Geojson_object.t
  type geometry =
  [ `Point of Point.t object'
  | `Multi_point of Multi_point.t object'
  | `Line_string of Line_string.t object'
  | `Multi_line_string of Multi_line_string.t object'
  | `Polygon of Polygon.t object'
  | `Multi_polygon of Multi_polygon.t object'
  | `Geometry_collection of geometry_collection object' ]
  and geometry_collection = geometry list

  module Feature = struct
    type id = [ `Number of float | `String of string ]
    type t =
      { id : id option;
        geometry : geometry option;
        properties : Jsont.json option; }

    let make id geometry properties = { id; geometry; properties }
    let make_geojson_object id geometry properties =
      Geojson_object.make (make id geometry properties)

    let id f = f.id
    let geometry f = f.geometry
    let properties f = f.properties

    type collection = t object' list
  end

  type t =
  [ `Feature of Feature.t object'
  | `Feature_collection of  Feature.collection object'
  | geometry ]

  let point v = `Point v
  let multi_point v = `Multi_point v
  let line_string v = `Line_string v
  let multi_line_string v = `Multi_line_string v
  let polygon v = `Polygon v
  let multi_polygon v = `Multi_polygon v
  let geometry_collection vs = `Geometry_collection vs
  let feature v = `Feature v
  let feature_collection vs = `Feature_collection vs

  let feature_id_jsont =
    let number =
      let dec = Jsont.Base.dec (fun n -> `Number n) in
      let enc = Jsont.Base.enc (function `Number n -> n | _ -> assert false) in
      Jsont.Base.number (Jsont.Base.map ~enc ~dec ())
    in
    let string =
      let dec = Jsont.Base.dec (fun n -> `String n) in
      let enc = Jsont.Base.enc (function `String n -> n | _ -> assert false) in
      Jsont.Base.string (Jsont.Base.map ~enc ~dec ())
    in
    let enc = function `Number _ -> number | `String _ -> string in
    Jsont.any ~kind:"id" ~dec_number:number ~dec_string:string ~enc ()

  (* The first two Json types below handle subtyping by redoing
     cases for subsets of types.  *)

  let case_map obj dec = Jsont.Object.Case.map (Jsont.kind obj) obj ~dec

  let rec geometry_jsont = lazy begin
    let case_point = case_map Point.jsont point in
    let case_multi_point = case_map Multi_point.jsont multi_point in
    let case_line_string = case_map Line_string.jsont line_string in
    let case_multi_line_string =
      case_map Multi_line_string.jsont multi_line_string
    in
    let case_polygon = case_map Polygon.jsont polygon in
    let case_multi_polygon = case_map Multi_polygon.jsont multi_polygon in
    let case_geometry_collection =
      case_map (Lazy.force geometry_collection_jsont) geometry_collection
    in
    let enc_case = function
    | `Point v -> Jsont.Object.Case.value case_point v
    | `Multi_point v -> Jsont.Object.Case.value case_multi_point v
    | `Line_string v -> Jsont.Object.Case.value case_line_string v
    | `Multi_line_string v -> Jsont.Object.Case.value case_multi_line_string v
    | `Polygon v -> Jsont.Object.Case.value case_polygon v
    | `Multi_polygon v -> Jsont.Object.Case.value case_multi_polygon v
    | `Geometry_collection v ->
        Jsont.Object.Case.value case_geometry_collection v
    in
    let cases = Jsont.Object.Case.[
        make case_point; make case_multi_point; make case_line_string;
        make case_multi_line_string; make case_polygon; make case_multi_polygon;
        make case_geometry_collection ]
    in
    Jsont.Object.map ~kind:"Geometry object" Fun.id
    |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
      ~tag_to_string:Fun.id ~tag_compare:String.compare
    |> Jsont.Object.finish
  end

  and feature_jsont : Feature.t object' Jsont.t Lazy.t = lazy begin
    let case_feature = case_map (Lazy.force case_feature_jsont) Fun.id in
    let enc_case v = Jsont.Object.Case.value case_feature v in
    let cases = Jsont.Object.Case.[ make case_feature ] in
    Jsont.Object.map ~kind:"Feature" Fun.id
    |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
      ~tag_to_string:Fun.id ~tag_compare:String.compare
    |> Jsont.Object.finish
  end

  and case_feature_jsont : Feature.t object' Jsont.t Lazy.t = lazy begin
    Jsont.Object.map ~kind:"Feature" Feature.make_geojson_object
    |> Jsont.Object.opt_mem "id" feature_id_jsont
      ~enc:(fun o -> Feature.id (Geojson_object.type' o))
    |> Jsont.Object.mem "geometry" (Jsont.option (Jsont.rec' geometry_jsont))
      ~enc:(fun o -> Feature.geometry (Geojson_object.type' o))
    |> Jsont.Object.mem "properties" (Jsont.option Jsont.json_object)
      ~enc:(fun o -> Feature.properties (Geojson_object.type' o))
    |> Geojson_object.finish_jsont
  end

  and geometry_collection_jsont = lazy begin
    Jsont.Object.map ~kind:"GeometryCollection" Geojson_object.make
    |> Jsont.Object.mem "geometries" (Jsont.list (Jsont.rec' geometry_jsont))
      ~enc:Geojson_object.type'
    |> Geojson_object.finish_jsont
  end

  and feature_collection_json = lazy begin
    Jsont.Object.map ~kind:"FeatureCollection" Geojson_object.make
    |> Jsont.Object.mem "features" Jsont.(list (Jsont.rec' feature_jsont))
      ~enc:Geojson_object.type'
    |> Geojson_object.finish_jsont
  end

  and jsont : t Jsont.t Lazy.t = lazy begin
    let case_point = case_map Point.jsont point in
    let case_multi_point = case_map Multi_point.jsont multi_point in
    let case_line_string = case_map Line_string.jsont line_string in
    let case_multi_line_string =
      case_map Multi_line_string.jsont multi_line_string
    in
    let case_polygon = case_map Polygon.jsont polygon in
    let case_multi_polygon = case_map Multi_polygon.jsont multi_polygon in
    let case_geometry_collection =
      case_map (Lazy.force geometry_collection_jsont) geometry_collection
    in
    let case_feature = case_map (Lazy.force case_feature_jsont) feature in
    let case_feature_collection =
      case_map (Lazy.force feature_collection_json) feature_collection
    in
    let enc_case = function
    | `Point v -> Jsont.Object.Case.value case_point v
    | `Multi_point v -> Jsont.Object.Case.value case_multi_point v
    | `Line_string v -> Jsont.Object.Case.value case_line_string v
    | `Multi_line_string v -> Jsont.Object.Case.value case_multi_line_string v
    | `Polygon v -> Jsont.Object.Case.value case_polygon v
    | `Multi_polygon v -> Jsont.Object.Case.value case_multi_polygon v
    | `Geometry_collection v ->
        Jsont.Object.Case.value case_geometry_collection v
    | `Feature v -> Jsont.Object.Case.value case_feature v
    | `Feature_collection v -> Jsont.Object.Case.value case_feature_collection v
    in
    let cases = Jsont.Object.Case.[
        make case_point; make case_multi_point; make case_line_string;
        make case_multi_line_string; make case_polygon; make case_multi_polygon;
        make case_geometry_collection; make case_feature;
        make case_feature_collection ]
    in
    Jsont.Object.map ~kind:"GeoJSON" Fun.id
    |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
      ~tag_to_string:Fun.id ~tag_compare:String.compare
    |> Jsont.Object.finish
  end

  let jsont = Lazy.force jsont
end

(* Command line interface *)

let ( let* ) = Result.bind
let strf = Printf.sprintf

let log_if_error ~use = function
| Ok v -> v
| Error e ->
    let lines = String.split_on_char '\n' e in
    Format.eprintf "@[%a @[<v>%a@]@]"
      Jsont.Error.puterr () (Format.pp_print_list Format.pp_print_string) lines;
    use

let with_infile file f = (* XXX add something to bytesrw. *)
  let process file ic = try Ok (f (Bytesrw.Bytes.Reader.of_in_channel ic)) with
  | Sys_error e -> Error (Format.sprintf "@[<v>%s:@,%s@]" file e)
  in
  try match file with
  | "-" -> process file In_channel.stdin
  | file -> In_channel.with_open_bin file (process file)
  with Sys_error e -> Error e

let trip ~file ~format ~locs ~dec_only =
  log_if_error ~use:1 @@
  with_infile file @@ fun r ->
  log_if_error ~use:1 @@
  let* t = Jsont_bytesrw.decode ~file ~locs Geojson.jsont r in
  if dec_only then Ok 0 else
  let w = Bytesrw.Bytes.Writer.of_out_channel stdout in
  let* () = Jsont_bytesrw.encode ~format ~eod:true Geojson.jsont t w in
  Ok 0

open Cmdliner
open Cmdliner.Term.Syntax

let geojson =
  Cmd.v (Cmd.info "geojson" ~doc:"round trip GeoJSON") @@
  let+ file =
    let doc = "$(docv) is the GeoJSON file. Use $(b,-) for stdin." in
    Arg.(value & pos 0 string "-" & info [] ~doc ~docv:"FILE")
  and+ locs =
    let doc = "Preserve locations (better errors)." in
    Arg.(value & flag & info ["l"; "locs"] ~doc)
  and+ format =
    let fmt = [ "indent", Jsont.Indent; "minify", Jsont.Minify ] in
    let doc = strf "Output style. Must be %s." (Arg.doc_alts_enum fmt)in
    Arg.(value & opt (enum fmt) Jsont.Minify &
         info ["f"; "format"] ~doc ~docv:"FMT")
  and+ dec_only =
    let doc = "Decode only." in
    Arg.(value & flag & info ["d"] ~doc)
  in
  trip ~file ~format ~locs ~dec_only

let main () = Cmd.eval' geojson
let () = if !Sys.interactive then () else exit (main ())
