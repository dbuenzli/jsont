(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Topojson codec https://github.com/topojson/topojson-specification *)

module String_map = Map.Make (String)

module Position = struct
  type t = float array
  let jsont = Jsont.(array ~doc:"Position" number)
end

module Bbox = struct
  type t = float array
  let jsont = Jsont.(array ~doc:"Bbox" number)
end

module Arcs = struct
  type t = Position.t array array
  let jsont = Jsont.(array ~doc:"Arcs" (array Position.jsont))
end

module Transform = struct
  type v2 = float * float
  type t = { scale : v2; translate : v2 }

  let make scale translate = { scale; translate }
  let scale t = t.scale
  let translate t = t.translate

  let v2_jsont =
    let dec x y = x, y in
    let enc (x, y) i = if i = 0 then x else y in
    Jsont.t2 ~dec ~enc Jsont.number

  let jsont =
    Jsont.Object.map ~kind:"Transform" make
    |> Jsont.Object.mem "scale" v2_jsont ~enc:scale
    |> Jsont.Object.mem "translate" v2_jsont ~enc:translate
    |> Jsont.Object.finish
end

module Point = struct
  type t = { coordinates : Position.t }
  let make coordinates = { coordinates }
  let coordinates v = v.coordinates
  let jsont =
    Jsont.Object.map ~kind:"Point" make
    |> Jsont.Object.mem "coordinates" Position.jsont ~enc:coordinates
    |> Jsont.Object.finish
end

module Multi_point = struct
  type t = { coordinates : Position.t list }
  let make coordinates = { coordinates }
  let coordinates v = v.coordinates
  let jsont =
    Jsont.Object.map ~kind:"MultiPoint" make
    |> Jsont.Object.mem "coordinates"
      (Jsont.list Position.jsont) ~enc:coordinates
    |> Jsont.Object.finish
end

module Line_string = struct
  type t = { arcs : int32 list }
  let make arcs = { arcs }
  let arcs v = v.arcs
  let jsont =
    Jsont.Object.map ~kind:"LineString" make
    |> Jsont.Object.mem "arcs" Jsont.(list int32) ~enc:arcs
    |> Jsont.Object.finish
end

module Multi_line_string = struct
  type t = { arcs : int32 list list }
  let make arcs = { arcs }
  let arcs v = v.arcs
  let jsont =
    Jsont.Object.map ~kind:"MultiLineString" make
    |> Jsont.Object.mem "arcs" Jsont.(list (list int32)) ~enc:arcs
    |> Jsont.Object.finish
end

module Polygon = struct
  type t = { arcs : int32 list list }
  let make arcs = { arcs }
  let arcs v = v.arcs
  let jsont =
    Jsont.Object.map ~kind:"Polygon" make
    |> Jsont.Object.mem "arcs" Jsont.(list (list int32)) ~enc:arcs
    |> Jsont.Object.finish
end

module Multi_polygon = struct
  type t = { arcs : int32 list list list }
  let make arcs = { arcs }
  let arcs v = v.arcs
  let jsont =
    Jsont.Object.map ~kind:"MultiPolygon" make
    |> Jsont.Object.mem "arcs" Jsont.(list (list (list int32))) ~enc:arcs
    |> Jsont.Object.finish
end

module Geometry = struct
  type t =
    { type' : type';
      id : string option;
      properties : Jsont.json String_map.t option;
      bbox : Bbox.t option;
      unknown : Jsont.json }

  and type' =
  | Point of Point.t
  | Multi_point of Multi_point.t
  | Line_string of Line_string.t
  | Multi_line_string of Multi_line_string.t
  | Polygon of Polygon.t
  | Multi_polygon of Multi_polygon.t
  | Geometry_collection of t list

  let make type' id properties bbox unknown =
    { type'; id; properties; bbox; unknown }

  let type' g = g.type'
  let id g = g.id
  let properties g = g.properties
  let bbox g = g.bbox
  let unknown g = g.unknown

  let point v = Point v
  let multi_point v = Multi_point v
  let line_string v = Line_string v
  let multi_linestr v = Multi_line_string v
  let polygon v = Polygon v
  let multi_polygon v = Multi_polygon v
  let collection vs = Geometry_collection vs

  let properties_type = Jsont.Object.as_string_map ~kind:"properties" Jsont.json

  let rec collection_jsont = lazy begin
    Jsont.Object.map ~kind:"GeometryCollection" Fun.id
    |> Jsont.Object.mem "geometries" (Jsont.list (Jsont.rec' jsont)) ~enc:Fun.id
    |> Jsont.Object.finish
  end

  and jsont = lazy begin
    let case_map obj dec = Jsont.Object.Case.map (Jsont.kind obj) obj ~dec in
    let case_point = case_map Point.jsont point in
    let case_multi_point = case_map Multi_point.jsont multi_point in
    let case_line_string = case_map Line_string.jsont line_string in
    let case_multi_linestr = case_map Multi_line_string.jsont multi_linestr in
    let case_polygon = case_map Polygon.jsont polygon in
    let case_multi_polygon = case_map Multi_polygon.jsont multi_polygon in
    let case_coll = case_map (Lazy.force collection_jsont) collection in
    let enc_case = function
    | Point p -> Jsont.Object.Case.value case_point p
    | Multi_point m -> Jsont.Object.Case.value case_multi_point m
    | Line_string l -> Jsont.Object.Case.value case_line_string l
    | Multi_line_string m -> Jsont.Object.Case.value case_multi_linestr m
    | Polygon p -> Jsont.Object.Case.value case_polygon p
    | Multi_polygon m -> Jsont.Object.Case.value case_multi_polygon m
    | Geometry_collection gs -> Jsont.Object.Case.value case_coll gs
    and cases = Jsont.Object.Case.[
        make case_point; make case_multi_point; make case_line_string;
        make case_multi_linestr; make case_polygon; make case_multi_polygon;
        make case_coll ]
    in
    Jsont.Object.map ~kind:"Geometry" make
    |> Jsont.Object.case_mem "type" Jsont.string ~enc:type' ~enc_case cases
      ~tag_to_string:Fun.id
    |> Jsont.Object.opt_mem "id" Jsont.string ~enc:id
    |> Jsont.Object.opt_mem "properties" properties_type ~enc:properties
    |> Jsont.Object.opt_mem "bbox" Bbox.jsont ~enc:bbox
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:unknown
    |> Jsont.Object.finish
  end

  let jsont = Lazy.force jsont
  type objects = t String_map.t
  let objects_jsont = Jsont.Object.as_string_map ~kind:"objects" jsont
end

module Topology = struct
  type t =
    { objects : Geometry.objects;
      arcs : Arcs.t;
      transform : Transform.t option;
      bbox : Bbox.t option;
      unknown : Jsont.json }

  let make objects arcs transform bbox unknown =
    { objects; arcs; transform; bbox; unknown }

  let objects t = t.objects
  let arcs t = t.arcs
  let transform t = t.transform
  let bbox t = t.bbox
  let unknown t = t.unknown
  let jsont =
    let kind = "Topology" in
    Jsont.Object.map ~kind (fun () -> make)
    |> Jsont.Object.mem "type" (Jsont.enum [kind, ()]) ~enc:(Fun.const ())
    |> Jsont.Object.mem "objects" Geometry.objects_jsont ~enc:objects
    |> Jsont.Object.mem "arcs" Arcs.jsont ~enc:arcs
    |> Jsont.Object.opt_mem "transform" Transform.jsont ~enc:transform
    |> Jsont.Object.opt_mem "bbox" Bbox.jsont ~enc:bbox
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:unknown
    |> Jsont.Object.finish
end

(* Command line interface *)

let ( let* ) = Result.bind
let strf = Printf.sprintf

let log_err fmt = Format.eprintf ("@[Error: " ^^ fmt ^^ "@]@.")
let log_if_error ~use = function Error e -> log_err "%s" e; use | Ok v -> v

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
  let* t = Jsont_bytesrw.decode ~file ~locs Topology.jsont r in
  if dec_only then Ok 0 else
  let w = Bytesrw.Bytes.Writer.of_out_channel stdout in
  let* () = Jsont_bytesrw.encode ~format ~eod:true Topology.jsont t w in
  Ok 0

open Cmdliner
open Cmdliner.Term.Syntax

let topojson =
  Cmd.v (Cmd.info "topojson" ~doc:"round trip TopoJSON") @@
  let+ file =
    let doc = "$(docv) is the TopoJSON file. Use $(b,-) for stdin." in
    Arg.(value & pos 0 string "-" & info [] ~doc ~docv:"FILE")
  and+ locs =
    let doc = "Preserve locations (for errors)." in
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

let main () = Cmd.eval' topojson
let () = if !Sys.interactive then () else exit (main ())
