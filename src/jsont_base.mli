(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Low-level internal tools for {!Jsont}. *)

val string_subrange : ?first:int -> ?last:int -> string -> string
val binary_string_of_hex : string -> (string, string) result
val binary_string_to_hex : string -> string

(** Type identifiers. Can be removed once we require OCaml 5.1 *)
module Type : sig
  type (_, _) eq = Equal : ('a, 'a) eq
  module Id : sig
    type 'a t
    val make : unit -> 'a t
    val uid : 'a t -> int
    val provably_equal : 'a t -> 'b t -> ('a, 'b) eq option
  end
end

(** Resizable arrays. *)
module Rarray :  sig
  type 'a t
  val get : 'a t -> int -> 'a
  val empty : unit -> 'a t
  val grow : 'a t -> 'a -> unit
  val length : 'a t -> int
  val add_last : 'a -> 'a t -> 'a t
  val to_array : 'a t -> 'a array
end

(** Resizable bigarrays. *)
module Rbigarray1 : sig
  type ('a, 'b, 'c) t
  val get : ('a, 'b, 'c) t -> int -> 'a
  val empty : ('a, 'b) Bigarray.kind -> 'c Bigarray.layout -> ('a, 'b, 'c) t
  val grow : ('a, 'b, 'c) t -> 'a -> unit
  val length : ('a, 'b, 'c) t -> int
  val add_last : 'a -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  val to_bigarray : ('a, 'b, 'c) t -> ('a, 'b, 'c) Bigarray.Array1.t
end

(** Mini fmt *)
module Fmt : sig
  type 'a t = Format.formatter -> 'a -> unit
  val pf : Format.formatter -> ('a, Format.formatter, unit) format -> 'a
  val str : ('a, Format.formatter, unit, string) format4 -> 'a
  val disable_ansi_styler : unit -> unit

  val nop : unit t
  val sp : unit t
  val list : ?pp_sep:unit t -> 'a t -> 'a list t
  val char : char t
  val string : string t
  val substring : int -> int -> string t
  val lines : string t
  val bold : string t
  val bold_red : string t
  val code : string t
  val puterr : unit t
  val out_of_dom : ?pp_kind:unit t -> unit -> (string * string list) t
  val should_it_be_mem : (string * string list) t
  val similar_mems : (string * string list) t


  type json_number_format = (float -> unit, Format.formatter, unit) format
  val json_null : unit t
  val json_bool : bool t
  val json_default_number_format : json_number_format
  val json_number' : json_number_format-> float t
  val json_number : float t
  val json_string : string t
end

(** See {!Jsont.Textloc} *)
module Textloc : sig
  type fpath = string
  val file_none : fpath

  type byte_pos = int
  val byte_pos_none : byte_pos

  type line_num = int
  val line_num_none : line_num

  type line_pos = line_num * byte_pos
  val line_pos_first : line_pos
  val line_pos_none : line_pos

  type t
  val none : t
  val make :
    file:fpath -> first_byte:byte_pos -> last_byte:byte_pos ->
    first_line:line_pos -> last_line:line_pos -> t

  val file : t -> fpath
  val first_byte : t -> byte_pos
  val last_byte : t -> byte_pos
  val first_line : t -> line_pos
  val last_line : t -> line_pos
  val is_none : t -> bool
  val is_empty : t -> bool
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val set_first : t -> first_byte:byte_pos -> first_line:line_pos -> t
  val set_last : t -> last_byte:byte_pos -> last_line:line_pos -> t
  val to_first : t -> t
  val to_last : t -> t
  val before : t -> t
  val after : t -> t
  val span : t -> t -> t
  val reloc : first:t -> last:t -> t
  val pp_ocaml : Format.formatter -> t -> unit
  val pp_gnu : Format.formatter -> t -> unit
  val pp : Format.formatter -> t -> unit
  val pp_dump : Format.formatter -> t -> unit
end

type 'a fmt = Stdlib.Format.formatter -> 'a -> unit

(** See {!Jsont.Meta} *)
module Meta : sig
  type t
  val make : ?ws_before:string -> ?ws_after:string -> Textloc.t -> t
  val none : t
  val is_none : t -> bool
  val textloc : t -> Textloc.t
  val ws_before : t -> string
  val ws_after : t -> string
  val with_textloc : t -> Textloc.t -> t
  val clear_ws : t -> t
  val clear_textloc : t -> t
  val copy_ws : t -> dst:t -> t
end

type 'a node = 'a * Meta.t

(** JSON number tools. *)
module Number : sig
  val number_contains_int : bool
  val int_is_uint8 : int -> bool
  val int_is_uint16 : int -> bool
  val int_is_int8 : int -> bool
  val int_is_int16 : int -> bool
  val can_store_exact_int : int -> bool
  val can_store_exact_int64 : Int64.t -> bool
  val in_exact_int_range : float -> bool
  val in_exact_uint8_range : float -> bool
  val in_exact_uint16_range : float -> bool
  val in_exact_int8_range : float -> bool
  val in_exact_int16_range : float -> bool
  val in_exact_int32_range : float -> bool
  val in_exact_int64_range : float -> bool
end

(** See {!Jsont.Path} *)
module Path : sig
  type index =
  | Mem of string node
  | Nth of int node

  val pp_index : index fmt
  val pp_index_trace : index fmt

  type t
  val root : t
  val is_root : t -> bool
  val nth : ?meta:Meta.t -> int -> t -> t
  val mem : ?meta:Meta.t -> string -> t -> t
  val rev_indices : t -> index list
  val of_string : string -> (t, string) result
  val pp : t fmt
  val pp_trace : t fmt

  module Caret : sig
    type path := t
    type pos = Before | Over | After
    type t = pos * path
    val of_string : string -> (t, string) result
    val pp : t fmt
  end

  val over : t -> Caret.t
  val before : t -> Caret.t
  val after : t -> Caret.t
end

(** See {!Jsont.Sort} *)
module Sort : sig
  type t = Null | Bool | Number | String | Array | Object
  val to_string : t -> string

  val kinded' : kind:string -> string -> string
  val kinded : kind:string -> t -> string
  val or_kind : kind:string -> t -> string
  val pp : Format.formatter -> t -> unit
end
