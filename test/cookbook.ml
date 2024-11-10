(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* Dealing with null values. *)

let string_null_is_empty =
  let null = Jsont.null "" in
  let enc = function "" -> null | _ -> Jsont.string in
  Jsont.any ~dec_null:null ~dec_string:Jsont.string ~enc ()

(* Objects as records *)

module Person = struct
  type t = { name : string; age : int }
  let make name age = { name; age }
  let name p = p.name
  let age p = p.age
  let jsont =
    Jsont.Object.map make ~kind:"Person"
    |> Jsont.Object.mem "name" Jsont.string ~enc:name
    |> Jsont.Object.mem "age" Jsont.int ~enc:age
    |> Jsont.Object.finish
end

(* Objects as key-value maps *)

module String_map = Map.Make (String)

let map : ?kind:string -> 'a Jsont.t -> 'a String_map.t Jsont.t =
fun ?kind t ->
  Jsont.Object.map ?kind Fun.id
  |> Jsont.Object.keep_unknown (Jsont.Object.Mems.string_map t) ~enc:Fun.id
  |> Jsont.Object.finish

(* Optional members *)

module Person_opt_age = struct
  type t = { name : string; age : int option }
  let make name age = { name; age }
  let name p = p.name
  let age p = p.age
  let jsont =
    Jsont.Object.map make ~kind:"Person"
    |> Jsont.Object.mem "name" Jsont.string ~enc:name
    |> Jsont.Object.mem "age" Jsont.(some int)
      ~dec_absent:None ~enc_omit:Option.is_none ~enc:age
    |> Jsont.Object.finish
end

(* Unknown object members *)

module Person_strict = struct
  type t = { name : string; age : int; }
  let make name age = { name; age }
  let name p = p.name
  let age p = p.age
  let jsont =
    Jsont.Object.map ~kind:"Person" make
    |> Jsont.Object.mem "name" Jsont.string ~enc:name
    |> Jsont.Object.mem "age" Jsont.int ~enc:age
    |> Jsont.Object.error_unknown
    |> Jsont.Object.finish
end

module Person_keep = struct
  type t = { name : string; age : int; unknown : Jsont.json ; }
  let make name age unknown = { name; age; unknown }
  let name p = p.name
  let age p = p.age
  let unknown v = v.unknown
  let jsont =
    Jsont.Object.map ~kind:"Person" make
    |> Jsont.Object.mem "name" Jsont.string ~enc:name
    |> Jsont.Object.mem "age" Jsont.int ~enc:age
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:unknown
    |> Jsont.Object.finish
end

(* Dealing with recursive JSON *)

module Tree = struct
  type 'a t = Node of 'a * 'a t list
  let make v children = Node (v, children)
  let value (Node (v, _)) = v
  let children (Node (_, children)) = children
  let jsont value_type =
    let rec t = lazy
      (Jsont.Object.map ~kind:"Tree" make
       |> Jsont.Object.mem "value" value_type ~enc:value
       |> Jsont.Object.mem "children" (Jsont.list (Jsont.rec' t)) ~enc:children
       |> Jsont.Object.finish)
    in
    Lazy.force t
end

(* Dealing with object types or classes *)

module Geometry_variant = struct
  module Circle = struct
    type t = { name : string; radius : float; }
    let make name radius = { name; radius }
    let name c = c.name
    let radius c = c.radius
    let jsont =
      Jsont.Object.map ~kind:"Circle" make
      |> Jsont.Object.mem "name" Jsont.string ~enc:name
      |> Jsont.Object.mem "radius" Jsont.number ~enc:radius
      |> Jsont.Object.finish
  end

  module Rect = struct
    type t = { name : string; width : float; height : float }
    let make name width height = { name; width; height }
    let name r = r.name
    let width r = r.width
    let height r = r.height
    let jsont =
      Jsont.Object.map ~kind:"Rect" make
      |> Jsont.Object.mem "name" Jsont.string ~enc:name
      |> Jsont.Object.mem "width" Jsont.number ~enc:width
      |> Jsont.Object.mem "height" Jsont.number ~enc:height
      |> Jsont.Object.finish
  end

  type t = Circle of Circle.t | Rect of Rect.t
  let circle c = Circle c
  let rect r = Rect r
  let jsont =
    let circle = Jsont.Object.Case.map "Circle" Circle.jsont ~dec:circle in
    let rect = Jsont.Object.Case.map "Rect" Rect.jsont ~dec:rect in
    let enc_case = function
    | Circle c -> Jsont.Object.Case.value circle c
    | Rect r -> Jsont.Object.Case.value rect r
    in
    let cases = Jsont.Object.Case.[make circle; make rect] in
    Jsont.Object.map ~kind:"Geometry" Fun.id
    |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
    |> Jsont.Object.finish
end

module Geometry_record = struct
  module Circle = struct
    type t = { radius : float; }
    let make radius = { radius }
    let radius c = c.radius
    let jsont =
      Jsont.Object.map ~kind:"Circle" make
      |> Jsont.Object.mem "radius" Jsont.number ~enc:radius
      |> Jsont.Object.finish
  end

  module Rect = struct
    type t = { width : float; height : float }
    let make width height = { width; height }
    let width r = r.width
    let height r = r.height
    let jsont =
      Jsont.Object.map ~kind:"Rect" make
      |> Jsont.Object.mem "width" Jsont.number ~enc:width
      |> Jsont.Object.mem "height" Jsont.number ~enc:height
      |> Jsont.Object.finish
  end

  type type' = Circle of Circle.t | Rect of Rect.t
  let circle c = Circle c
  let rect r = Rect r

  type t = { name : string; type' : type' }
  let make name type' = { name; type' }
  let name g = g.name
  let type' g = g.type'

  let jsont =
    let circle = Jsont.Object.Case.map "Circle" Circle.jsont ~dec:circle in
    let rect = Jsont.Object.Case.map "Rect" Rect.jsont ~dec:rect in
    let enc_case = function
    | Circle c -> Jsont.Object.Case.value circle c
    | Rect r -> Jsont.Object.Case.value rect r
    in
    let cases = Jsont.Object.Case.[make circle; make rect] in
    Jsont.Object.map ~kind:"Geometry" make
    |> Jsont.Object.mem "name" Jsont.string ~enc:name
    |> Jsont.Object.case_mem "type" Jsont.string ~enc:type' ~enc_case cases
    |> Jsont.Object.finish
end
