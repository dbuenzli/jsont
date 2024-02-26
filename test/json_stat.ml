(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* https://json-stat.org/ *)

open Jsonit

module Int_map = Map.Make (Int)
module String_map = Map.Make (String)

type 'a vec = Array of 'a list | Sparse of 'a Int_map.t

type status =
| All of string
| Vec of string vec

type index = (* ?? *)
| Array of string list
| Map of int String_map.t

type category =
  { index : index;
    label : string String_map.t }

type date =
  (* https://262.ecma-international.org/6.0/#sec-date-time-string-format *)
  string

module Dimension_id = struct
  type t =
    { category : Json.obj;
      label : string option;
      extension : Json.obj option; }
end


type dataset =
  { id : string list;
    size : int list;
    value : float vec;
    dimension : Dimension_id.t String_map.t;
    status : status vec option;
    label : string option;
    source : string option;
    updated : date option;
    extension : Json.obj option; }

type collection = unit

type class' =
| Dataset of dataset
| Dimension of Dimension_id.t
| Collection of collection

type t =
  { version : string;
    class' : class'; }
