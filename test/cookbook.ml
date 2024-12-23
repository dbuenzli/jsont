(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* Dealing with null values. *)

let string_null_is_empty =
  let null = Jsont.null "" in
  let enc = function "" -> null | _ -> Jsont.string in
  Jsont.any ~dec_null:null ~dec_string:Jsont.string ~enc ()


(* Base maps *)

module M = struct
  type t = unit
  let result_of_string s : (t, string) result = invalid_arg "unimplemented"
  let of_string_or_failure s : t = invalid_arg "unimplemented"
  let to_string v : string = invalid_arg "unimplemented"
end

let m_jsont =
  let dec = Jsont.Base.dec_result M.result_of_string in
  let enc = Jsont.Base.enc M.to_string in
  Jsont.Base.string (Jsont.Base.map ~kind:"M.t" ~dec ~enc ())

let m_jsont' =
  let dec = Jsont.Base.dec_failure M.of_string_or_failure in
  let enc = Jsont.Base.enc M.to_string in
  Jsont.Base.string (Jsont.Base.map ~kind:"M.t" ~dec ~enc ())

let m_jsont'' =
  Jsont.of_of_string ~kind:"M.t" M.result_of_string ~enc:M.to_string

(* Objects as records *)

module Person = struct
  type t = { name : string; age : int }
  let make name age = { name; age }
  let name p = p.name
  let age p = p.age
  let jsont =
    Jsont.Object.map ~kind:"Person" make
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
    Jsont.Object.map ~kind:"Person" make
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


(* Untagged object types *)

module Response = struct
  type t =
    { id : int;
      value : (Jsont.json, string) result }

  let make id result error =
    let pp_mem = Jsont.Repr.pp_code in
    match result, error with
    | Some result, None -> { id; value = Ok result }
    | None, Some error -> { id; value = Error error }
    | Some _ , Some _ ->
        Jsont.Error.msgf Jsont.Meta.none "Both %a and %a members are defined"
          pp_mem "result" pp_mem "error"
    | None, None ->
        Jsont.Error.msgf Jsont.Meta.none "Missing either %a or %a member"
          pp_mem "result" pp_mem "error"

  let result r = match r.value with Ok v -> Some v | Error _ -> None
  let error r = match r.value with Ok _ -> None | Error e -> Some e

  let jsont =
    Jsont.Object.map make
    |> Jsont.Object.mem "id" Jsont.int ~enc:(fun r -> r.id)
    |> Jsont.Object.opt_mem "result" Jsont.json ~enc:result
    |> Jsont.Object.opt_mem "error" Jsont.string ~enc:error
    |> Jsont.Object.finish
end

(* Flattening objects on queries *)

module Group = struct
  type t = { id : int; name : string; persons : Person.t list }
  let make id name persons = { id; name; persons }

  let info_jsont =
    Jsont.Object.map make
    |> Jsont.Object.mem "id" Jsont.int
    |> Jsont.Object.mem "name" Jsont.string
    |> Jsont.Object.finish

  let jsont =
    Jsont.Object.map (fun k persons -> k persons)
    |> Jsont.Object.mem "info" info_jsont
    |> Jsont.Object.mem "persons" (Jsont.list Person.jsont)
    |> Jsont.Object.finish
end
