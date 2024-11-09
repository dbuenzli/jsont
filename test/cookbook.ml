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
    Jsont.Obj.map make ~kind:"Person"
    |> Jsont.Obj.mem "name" Jsont.string ~enc:name
    |> Jsont.Obj.mem "age" Jsont.int ~enc:age
    |> Jsont.Obj.finish
end

(* Objects as key-value maps *)

module String_map = Map.Make (String)

let map : ?kind:string -> 'a Jsont.t -> 'a String_map.t Jsont.t =
fun ?kind t ->
  Jsont.Obj.map ?kind Fun.id
  |> Jsont.Obj.keep_unknown (Jsont.Obj.Mems.string_map t) ~enc:Fun.id
  |> Jsont.Obj.finish

(* Optional members *)

module Person_opt_age = struct
  type t = { name : string; age : int option }
  let make name age = { name; age }
  let name p = p.name
  let age p = p.age
  let jsont =
    Jsont.Obj.map make ~kind:"Person"
    |> Jsont.Obj.mem "name" Jsont.string ~enc:name
    |> Jsont.Obj.mem "age" Jsont.(some int)
      ~dec_absent:None ~enc_omit:Option.is_none ~enc:age
    |> Jsont.Obj.finish
end

(* Unknown object members *)

module Person_strict = struct
  type t = { name : string; age : int; }
  let make name age = { name; age }
  let name p = p.name
  let age p = p.age
  let jsont =
    Jsont.Obj.map ~kind:"Person" make
    |> Jsont.Obj.mem "name" Jsont.string ~enc:name
    |> Jsont.Obj.mem "age" Jsont.int ~enc:age
    |> Jsont.Obj.error_unknown
    |> Jsont.Obj.finish
end

module Person_keep = struct
  type t = { name : string; age : int; unknown : Jsont.json ; }
  let make name age unknown = { name; age; unknown }
  let name p = p.name
  let age p = p.age
  let unknown v = v.unknown
  let jsont =
    Jsont.Obj.map ~kind:"Person" make
    |> Jsont.Obj.mem "name" Jsont.string ~enc:name
    |> Jsont.Obj.mem "age" Jsont.int ~enc:age
    |> Jsont.Obj.keep_unknown Jsont.json_mems ~enc:unknown
    |> Jsont.Obj.finish
end

(* Dealing with recursive JSON *)

module Tree = struct
  type 'a t = Node of 'a * 'a t list
  let make v children = Node (v, children)
  let value (Node (v, _)) = v
  let children (Node (_, children)) = children
  let jsont value_type =
    let rec t = lazy
      (Jsont.Obj.map ~kind:"Tree" make
       |> Jsont.Obj.mem "value" value_type ~enc:value
       |> Jsont.Obj.mem "children" (Jsont.list (Jsont.rec' t)) ~enc:children
       |> Jsont.Obj.finish)
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
      Jsont.Obj.map ~kind:"Circle" make
      |> Jsont.Obj.mem "name" Jsont.string ~enc:name
      |> Jsont.Obj.mem "radius" Jsont.number ~enc:radius
      |> Jsont.Obj.finish
  end

  module Rect = struct
    type t = { name : string; width : float; height : float }
    let make name width height = { name; width; height }
    let name r = r.name
    let width r = r.width
    let height r = r.height
    let jsont =
      Jsont.Obj.map ~kind:"Rect" make
      |> Jsont.Obj.mem "name" Jsont.string ~enc:name
      |> Jsont.Obj.mem "width" Jsont.number ~enc:width
      |> Jsont.Obj.mem "height" Jsont.number ~enc:height
      |> Jsont.Obj.finish
  end

  type t = Circle of Circle.t | Rect of Rect.t
  let circle c = Circle c
  let rect r = Rect r
  let jsont =
    let circle = Jsont.Obj.Case.map "Circle" Circle.jsont ~dec:circle in
    let rect = Jsont.Obj.Case.map "Rect" Rect.jsont ~dec:rect in
    let enc_case = function
    | Circle c -> Jsont.Obj.Case.value circle c
    | Rect r -> Jsont.Obj.Case.value rect r
    in
    let cases = Jsont.Obj.Case.[make circle; make rect] in
    Jsont.Obj.map ~kind:"Geometry" Fun.id
    |> Jsont.Obj.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
    |> Jsont.Obj.finish
end

module Geometry_record = struct
  module Circle = struct
    type t = { radius : float; }
    let make radius = { radius }
    let radius c = c.radius
    let jsont =
      Jsont.Obj.map ~kind:"Circle" make
      |> Jsont.Obj.mem "radius" Jsont.number ~enc:radius
      |> Jsont.Obj.finish
  end

  module Rect = struct
    type t = { width : float; height : float }
    let make width height = { width; height }
    let width r = r.width
    let height r = r.height
    let jsont =
      Jsont.Obj.map ~kind:"Rect" make
      |> Jsont.Obj.mem "width" Jsont.number ~enc:width
      |> Jsont.Obj.mem "height" Jsont.number ~enc:height
      |> Jsont.Obj.finish
  end

  type type' = Circle of Circle.t | Rect of Rect.t
  let circle c = Circle c
  let rect r = Rect r

  type t = { name : string; type' : type' }
  let make name type' = { name; type' }
  let name g = g.name
  let type' g = g.type'

  let jsont =
    let circle = Jsont.Obj.Case.map "Circle" Circle.jsont ~dec:circle in
    let rect = Jsont.Obj.Case.map "Rect" Rect.jsont ~dec:rect in
    let enc_case = function
    | Circle c -> Jsont.Obj.Case.value circle c
    | Rect r -> Jsont.Obj.Case.value rect r
    in
    let cases = Jsont.Obj.Case.[make circle; make rect] in
    Jsont.Obj.map ~kind:"Geometry" make
    |> Jsont.Obj.mem "name" Jsont.string ~enc:name
    |> Jsont.Obj.case_mem "type" Jsont.string ~enc:type' ~enc_case cases
    |> Jsont.Obj.finish
end
