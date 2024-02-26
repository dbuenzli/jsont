(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Types for JSON values.

    This module provides a type for describing JSON values as a
    bidirectional map between subset of JSON values and arbitrary
    OCaml values. We call these values {e JSON types}.

    In these maps we call {e decoding} the direction from JSON values
    to OCaml values and {e encoding} the direction from OCaml values
    to JSON values. Depending on your usage one direction or the other
    can be left unspecified. Some of the maps may be lossy or creative
    which leads to JSON queries and transforms.

    See the {{!page-index.quick_start}quick start} and the
    {{!page-cookbook}cookbook}. *)

(** {1:preliminaries Preliminaries} *)

type 'a fmt = Format.formatter -> 'a -> unit
(** The type for formatters of type ['a]. *)

(** Text locations.

    A text location identifies a text span in a given UTF-8 encoded file
    by an inclusive range of absolute {{!Textloc.type-byte_pos}byte} positions
    and the {{!Textloc.type-line_pos}line positions} on which those occur. *)
module Textloc : sig

  (** {1:fpath File paths} *)

  type fpath = string
  (** The type for file paths. *)

  val file_none : fpath
  (** [file_none] is ["-"]. A file path to use when there is none. *)

  (** {1:pos Positions} *)

  (** {2:byte_pos Byte positions} *)

  type byte_pos = int
  (** The type for zero-based, absolute, byte positions in text. If
      the text has [n] bytes, [0] is the first position and [n-1] is
      the last position. *)

  val byte_pos_none : byte_pos
  (** [byte_pos_none] is [-1]. A position to use when there is none. *)

  (** {2:lines Lines} *)

  type line_num = int
  (** The type for one-based, line numbers in the text. Lines
      increment after a {e newline} which is either a line feed ['\n']
      (U+000A), a carriage return ['\r'] (U+000D) or a carriage return
      and a line feed ["\r\n"] (<U+000D,U+000A>). *)

  val line_num_none : line_num
  (** [line_num_none] is [-1]. A line number to use when there is none. *)

  (** {2:line_pos Line positions} *)

  type line_pos = line_num * byte_pos
  (** The type for line positions. This identifies a line by its line
      number and the absolute byte position following its newline
      (or the start of text for the first line). That byte position:
      {ul
      {- Indexes the first byte of text of the line if the line is non-empty.}
      {- Indexes the first byte of the next {e newline} if the line is empty.}
      {- Is out of bounds and equal to the text's length for a last empty
         line (this includes when the text is empty).}} *)

  val line_pos_first : line_pos
  (** [line_pos_first] is [1, 0]. Note that this is the only line position
      of the empty text. *)

  val line_pos_none : line_pos
  (** [line_pos_none] is [(line_pos_none, pos_pos_none)]. *)

  (** {1:tloc Text locations} *)

  type t
  (** The type for text locations. A text location identifies a text
      span in an UTF-8 encoded file by an inclusive range of absolute
      {{!type-byte_pos}byte positions} and the {{!type-line_pos}line
      positions} on which they occur.

      If the first byte equals the last byte the range contains
      exactly that byte. If the first byte is greater than the last
      byte this represents an insertion point before the first
      byte. In this case information about the last position should
      be ignored: it can contain anything. *)

  val none : t
  (** [none] is a position to use when there is none. *)

  val make :
    file:fpath -> first_byte:byte_pos -> last_byte:byte_pos ->
    first_line:line_pos -> last_line:line_pos -> t
  (** [v ~file ~first_byte ~last_byte ~first_line ~last_line] is a text
      location with the given arguments, see corresponding accessors for
      the semantics. If you don't have a file use {!file_none}. *)

  val file : t -> fpath
  (** [file l] is [l]'s file. *)

  val first_byte : t -> byte_pos
  (** [first_byte l] is [l]'s first byte. Irrelevant if {!is_none} is
      [true]. *)

  val last_byte : t -> byte_pos
  (** [last_byte l] is [l]'s last byte. Irrelevant if {!is_none} or
      {!is_empty} is [true]. *)

  val first_line : t -> line_pos
  (** [first_line l] is the line position on which [first_byte l] lies.
      Irrelevant if {!is_none} is [true].*)

  val last_line : t -> line_pos
  (** [last_line l] is the line position on which [last_byte l] lies.
      Irrelevant if {!is_none} or {!is_empty} is [true].*)

  (** {2:preds Predicates and comparisons} *)

  val is_none : t -> bool
  (** [is_none t] is [true] iff [first_byte < 0]. *)

  val is_empty : t -> bool
  (** [is_empty t] is [true] iff [first_byte t > last_byte t]. *)

  val equal : t -> t -> bool
  (** [equal t0 t1] is [true] iff [t0] and [t1] are equal. This checks
      that {!file}, {!first_byte} and {!last_byte} are equal. Line information
      is ignored. *)

  val compare : t -> t -> int
  (** [compare t0 t1] orders [t0] and [t1]. The order is compatible
      with {!equal}. Comparison starts with {!file}, follows with
      {!first_byte} and ends, if needed, with {!last_byte}. Line
      information is ignored. *)

  (** {2:shrink_and_stretch Shrink and stretch} *)

  val set_first : t -> first_byte:byte_pos -> first_line:line_pos -> t
  (** [set_first l ~first_byte ~first_line] sets the the first position of
      [l] to given values. *)

  val set_last : t -> last_byte:byte_pos -> last_line:line_pos -> t
  (** [set_last l ~last_byte ~last_line] sets the last position of [l]
      to given values. *)

  val to_first : t -> t
  (** [to_first l] has both first and last positions set to [l]'s first
      position. The range spans {!first_byte}. See also {!before}. *)

  val to_last : t -> t
  (** [to_last l] has both first and last positions set to [l]'s last
        position. The range spans {!last_byte}. See also {!after}. *)

  val before : t -> t
  (** [before t] is the {{!is_empty}empty} text location starting at
      {!first_byte}. *)

  val after : t -> t
  (** [after t] is the empty {{!is_empty}empty} location starting at
      [last_byte t + 1]; note that at the end of input this may be an
      invalid byte {e index}. The {!first_line} and {!last_line} of the
      result is [last_line t]. *)

  val span : t -> t -> t
  (** [span l0 l1] is the span from the smallest byte position of [l0] and
      [l1] to the largest byte position of [l0] and [l1]. The file path is
      taken from the greatest byte position. *)

  val reloc : first:t -> last:t -> t
  (** [reloc ~first ~last] uses the first position of [first], the
      last position of [last] and the file of [last]. *)

  (** {2:fmt Formatting} *)

  val pp_ocaml : Format.formatter -> t -> unit
  (** [pp_ocaml] formats text locations like the OCaml compiler. *)

  val pp_gnu : Format.formatter -> t -> unit
  (** [pp_gnu] formats text locations according to the
      {{:https://www.gnu.org/prep/standards/standards.html#Errors}GNU
      convention}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] is {!pp_gnu}. *)

  val pp_dump : Format.formatter -> t -> unit
  (** [pp_dump] formats raw data for debugging. *)
end

(** Node metadata.

    This type keeps information about source JSON text locations
    and whitespace. *)
module Meta : sig
  type t
  (** The type for node metadata. *)

  val make : ?ws_before:string -> ?ws_after:string -> Textloc.t -> t
  (** [make textloc ~ws_before ~ws_after] is metadata with text location
      [textloc]. *)

  val none : t
  (** [none] is metadata for when there is none. Its {!Textloc.t}
      is {!Textloc.none} and its whitespace is empty. *)

  val textloc : t -> Textloc.t
  (** [textloc m] is the text location of [m]. *)

  val ws_before : t -> string
  (** [ws_before m] is source whitespace before the node. *)

  val ws_after : t -> string
  (** [ws_after m] is source whitespace after the node. *)

  val with_textloc : t -> Textloc.t -> t
  (** [with_textloc m l] is [m] with text location [l] *)

  val clear_ws : t -> t
  (** [clear_ws m] is [m] with {!ws_before} and {!ws_after} set to [""]. *)
end

type 'a node = 'a * Meta.t
(** The type for JSON nodes. The data of type ['a] and its metadata. *)

(** JSON paths.

    Paths are used for keeping track of encoding and decoding
    {{!Context}contexts} and for specifying queries. *)
module Path : sig

  (** {1:indices Indices} *)

  type index =
  | Mem of string node (** Indexes the value of the member [n] of an object. *)
  | Nth of int node
  (** Indexes the value of the [n]th element of an array. If negative counts
      the number of elements from the end: [-1] is that last element. This
      may not supported by all operations accepting index values. *)
  (** The type for indexing operations on JSON values. *)

  val pp_index : index fmt
  (** [pp_index] formats indexes. *)

  val pp_index_trace : index fmt
  (** [pp_index] formats indexes and their location. *)

  (** {1:path Paths} *)

  type t
  (** The type for paths, a sequence of indexing operations. *)

  val root : t
  (** [root] is the root path. *)

  val is_root : t -> bool
  (** [is_root p] is [true] iff [p] is the root path. *)

  val nth : ?meta:Meta.t -> int -> t -> t
  (** [nth n p] indexes the array indexed by [p] at index [n]. *)

  val mem : ?meta:Meta.t -> string -> t -> t
  (** [mem n p] indexes the object indexed by [p] at member [n]. *)

  val rev_indices : t -> index list
  (** [rev_indices p] are the indices of [p] in reverse order, the last
      indexing operation appears first. *)

  val of_string : string -> (t, string) result
  (** [of_string s] parses a path according to the
      {{!Caret.path_caret_syntax}path syntax}. *)

  val pp : t fmt
  (** [pp] formats paths. *)

  val pp_trace : t fmt
  (** [pp_trace] formats paths as a stack trace, if not empty. *)
end

(** JSON encoding and decoding contexts. *)
module Context : sig
  type t
  (** The type for JSON encoding or decoding contexts. *)

  val make : Path.t -> t
  (** [make path] is the context at [path]. *)

  val root : t
  (** [root] is the toplevel context. *)

  val path : t -> Path.t
  (** [path] is the JSON path of the context. *)

  val is_root : t -> bool
  (** [is_top c] is {!Path.is_root}[ (path c)]. *)

  val push_nth : int node -> t -> t
  (** [push_nth n] is a context for the [n]th element of an array. *)

  val push_mem : string node -> t -> t
  (** [push_mem n] is a context for the member named [n] of an object. *)
end

(** Sorts of JSON values. *)
module Sort : sig
  type t =
  | Null (** Nulls *)
  | Bool (** Booleans *)
  | Number (** Numbers *)
  | String (** Strings *)
  | Array (** Arrays *)
  | Obj (** Objects *)
  (** The type for sorts of JSON values. *)

  val to_string : t -> string
  (** [to_string s] is a string for sort [s]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats sorts. *)
end

(** Encoding, decoding and query errors. *)
module Error : sig

  type kind
  (** The type for kind of errors. *)

  type t = Context.t * Meta.t * kind
  (** The type for errors. *)

  val to_string : t -> string
  (** [error_to_string e] formats [e] using {!pp_error} to a string. *)

  val make_msg : Context.t -> Meta.t -> string -> t
  (** [make_msg ctx m msg] is an error with message [msg] for meta [m]
      in context [ctx]. *)

  val msg : Context.t -> Meta.t -> string -> 'a
  (** [error ctx m msg] raises an error with message [msg] for meta
      [m] in context [ctx]. *)

  val msgf :
    Context.t -> Meta.t -> ('a, Stdlib.Format.formatter, unit, 'b) format4 -> 'a
  (** [fmt] is like {!val-msg} but formats an error message. *)

  val pp : t fmt
  (** [pp_error] formats errors. *)

  (* FIXME some rationalized form of that is likely useful
     for custom queries.

  val sort : Context.t -> Meta.t -> exp:Sort.t -> fnd:Sort.t -> 'a
  (** [sort p m ~exp ~fnd] errors when sort [exp] was expected
      but [fnd] was found. See also {!Jsont.Repr.type_error} *)

  val kind : Context.t -> Meta.t -> exp:string -> fnd:Sort.t -> 'a
  (** [error_kind p m ~exp ~fnd] errors when kind [exp] was expected
      but sort [fnd] was found. See also {!Jsont.Repr.type_error}. *)
  *)

  val missing_mems :
    Context.t -> Meta.t -> obj_kind:string -> exp:string list ->
    fnd:string list -> 'a
  (** [missing_mems ctx m ~obj_kind ~exp ~mems] errors when member
      named [exp] were expected in an object of kind [obj_kind] with
      metadata [meta] but the object had only [fnd] names (leave empty
      if the info is no longer available). [m]'s location should span
      the object. *)

  (*
  val unexpected_mems :
    Context.t -> Meta.t -> obj_kind:string -> exp:string list ->
    fnd:string list -> 'a
  (** [unexpected_mems ctx m ~obj_kind ~exp fnd] errors when member name
      [n] is found in an object but of kind [obj_kind] is not expected. TODO what does
      meta represent. *)
*)
end

exception Error of Error.t
(** The exception raised on map errors. In general codec and query
    functions turn that for you into a {!result} value, see for
    example {{!Json.converting}these conversion} functions. *)

(** {1:types Types} *)

type 'a t
(** The type for JSON types.

    A value of this type represents a subset of JSON values mapped to
    a subset of values of type ['a] (and vice-versa). *)


val value_kind : 'a t -> string
(** [value_kind t] is a human readable string describing the JSON values typed
    by [t]. *)

val kind : 'a t -> string
(** [kind t] is the [kind] of the underlying map. *)

val doc : 'a t -> string
(** [doc t] is a documentation string for the JSON values typed by [t]. *)

(** Mapping JSON base types. *)
module Base : sig

  (** {1:maps Maps} *)

  type ('a, 'b) map
  (** The type for mapping values of type ['a] to values of type ['b]. *)

  val map :
    ?kind:string -> ?doc:string -> ?dec:(Context.t -> Meta.t -> 'a -> 'b) ->
    ?enc:(Context.t -> 'b -> 'a) -> ?enc_meta:(Context.t -> 'b -> Meta.t) ->
    unit -> ('a, 'b) map
  (** [map ~kind ~doc ~dec ~enc ~enc_meta ()] maps JSON base types
      represented by value of type ['a] to values of type ['b] with:
      {ul
      {- [kind] names the entities represented by type ['b].
         Defaults to [""].}
      {- [doc] is a documentation string for [kind]. Defaults to [""].}
      {- [dec] is used to decode values of type ['a] to values of
         type ['b]. Can be omitted if the result is only used for
         encoding, the default unconditionally errors.}
      {- [enc] is used to encode values of type ['b] to values of
         type ['a]. Can be omitted if the result is only used for
         decoding, the default unconditionally errors.}
      {- [enc_meta] is used to recover JSON metadata (e.g. source
         layout information) from a value to encode. The default
         unconditionnaly returns {!Json.Meta.none}.}}

      {{!decenc}These functions} can be used to quickly devise
      [dec] and [enc] functions from standard OCaml conversion
      interfaces. *)

  val id : ('a, 'a) map
  (** [id] is the identity map. *)

  val ignore : ('a, unit) map
  (** [ignore] is the ignoring map. It ignores decodes and errors on
      encodes. *)

  (** {1:types JSON types} *)

  val null : (unit, 'a) map -> 'a t
  (** [null m] maps with [m] JSON nulls represented by [()] to
      values of type ['a]. See also {!Jsont.null}. *)

  val bool : (bool, 'a) map -> 'a t
  (** [bool m] maps with [m] JSON booleans represented by [bool]
      values to values of type ['a]. See also {!Jsont.bool}. *)

  val number : (float, 'a) map -> 'a t
  (** [number m] maps with [m] JSON nulls or numbers represented by
      [float] values to values of type ['a]. The [float]
      representation decodes JSON nulls to {!Float.nan} and lossily
      encodes any {{!Float.is_finite}non-finite} to JSON null
      ({{!page-cookbook.numbers_as_nulls}explanation}). See also
      {!Jsont.number}. *)

  val string : (string, 'a) map -> 'a t
  (** [string m] maps with [m] unescaped JSON strings represented by
      UTF-8 encoded [string] values to values of type ['a]. See also
      {!Jsont.string}. *)

  (** {1:decenc Decoding and encoding functions}

      These function create suitable [dec] and [enc] functions
      to give to {!map} from standard OCaml conversion interfaces. *)

  val dec : ('a -> 'b) -> (Context.t -> Meta.t -> 'a -> 'b)
  (** [dec f] is a decoding function from [f]. This assumes [f] never fails. *)

  val dec_result :
    ?kind:string -> ('a -> ('b, string) result) ->
    (Context.t -> Meta.t -> 'a -> 'b)
  (** [dec f] is a decoding function from [f]. [Error _] values are given to
      {!Error.msg}, prefixed by [kind:] (if specified). *)

  val dec_failure :
    ?kind:string -> ('a -> 'b) -> (Context.t -> Meta.t -> 'a -> 'b)
  (** [dec f] is a decoding function from [f]. [Failure _] exceptions
      are catched and given to {!Error.msg}, prefixed by [kind:] (if
      specified). *)

  val enc : ('b -> 'a) -> (Context.t -> 'b -> 'a)
  (** [enc f] is an encoding function from [f]. This assumes [f] never fails. *)

  val enc_result :
    ?kind:string -> ('b -> ('a, string) result) -> (Context.t -> 'b -> 'a)
  (** [enc_result f] is an encoding function from [f]. [Error _] values are
      given to {!Error.msg}, prefixed by [kind:] (if specified) *)

  val enc_failure :
    ?kind:string -> ('b -> 'a) -> (Context.t -> 'b -> 'a)
  (** [enc_failure f] is an encoding function from [f]. [Failure _]
      exceptions are catched and given to {!Error.msg}, prefixed by [kind:]
      (if specified). *)
end

(** Mapping JSON arrays. *)
module Array : sig

  (** {1:maps Maps} *)

  type ('array, 'elt) enc =
    { enc : 'acc. Context.t -> ('acc -> int -> 'elt -> 'acc) -> 'acc ->
       'array -> 'acc }
  (** The type for specifying array encoding functions. A function to fold
      over the elements of type ['elt] of the array of type ['array]. *)

  type ('array, 'elt, 'builder) map
  (** The type for mapping JSON arrays with elements of type ['elt] to arrays
      of type ['array] using values of type ['builder] to build them. *)

  val map :
    ?kind:string -> ?doc:string ->
    dec_empty:(Context.t -> 'builder) ->
    ?dec_skip:(Context.t -> int -> 'builder -> bool) ->
    dec_add:(Context.t -> int -> 'elt -> 'builder -> 'builder) ->
    dec_finish:(Context.t -> Meta.t -> int -> 'builder -> 'array) ->
    enc:('array, 'elt) enc ->
    ?enc_meta:(Context.t -> 'array -> Meta.t) -> 'elt t ->
    ('array, 'elt, 'builder) map
  (** [map elt] maps JSON arrays of type ['elt] to arrays of
      type ['array] built with type ['builder].
      {ul
      {- [kind] names the entities represented by type ['a].
         Defaults to [""].}
      {- [doc] is a documentation string for [kind]. Defaults to [""].}
      {- [dec_empty] is used to create a builder for the empty array.}
      {- [dec_add] is used to add the [i]th JSON element to the builder.}
      {- [dec_skip] is used to skip the [i]th index of the JSON array.
         If [true], the element is not decoded with [elt] and not added with
         [dec_add] but skipped. Defaults returns always [false].}
      {- [dec_finish b] converts the builder to the final array.}
      {- [enc.enc ctx f acc v] folds over the elements of array [v] in
         increasing order with [f] and starting with [acc] to encode it
         in a JSON array.}
      {- [enc_meta v] is the metadata to use for encoding [v] to a JSON
         array.}} *)

  val list_map :
    ?kind:string -> ?doc:string ->
    ?dec_skip:(Context.t -> int -> 'a list -> bool) -> 'a t ->
    ('a list, 'a, 'a list) map
  (** [list_map elt] maps JSON arrays with elements of type [elt]
      to [list] values. See also {!Jsont.list}. *)

  type 'a array_builder
  (** The type for array builders. *)

  val array_map :
    ?kind:string -> ?doc:string ->
    ?dec_skip:(Context.t -> int -> 'a array_builder -> bool) -> 'a t ->
    ('a array, 'a, 'a array_builder) map
  (** [array_map elt] maps JSON arrays with elements of type [elt]
      to [array] values. See also {!Jsont.array}. *)

  type ('a, 'b, 'c) bigarray_builder
  (** The type for bigarray_builders. *)

  val bigarray_map :
    ?kind:string -> ?doc:string ->
    ?dec_skip:(Context.t -> int -> ('a, 'b, 'c) bigarray_builder -> bool) ->
    ('a, 'b) Bigarray.kind -> 'c Bigarray.layout -> 'a t ->
    (('a, 'b, 'c) Bigarray.Array1.t, 'a, ('a, 'b, 'c) bigarray_builder) map
  (** [bigarray k l elt] maps JSON arrays with elements of
      type [elt] to bigarray values of kind [k] and layout [l].  See
      also {!Jsont.bigarray}. *)

  (** {1:types JSON types} *)

  val array : ('a, _, _) map -> 'a t
  (** [array m] maps with [m] JSON arrays to values of type ['a].
      See the the {{!section-derived_arrays}array combinators}.  *)

  val ignore : unit t
  (** [ignore] ignores JSON arrays on decoding and errors on encoding. *)
end

(** Mapping JSON objects.

    This module allows to describe JSON objects. See a
    {{!page-cookbook.objects_as_records}simple example},
    more examples can be found in the {{!page-cookbook}cookbook}. *)
module Obj : sig

  (** {1:maps Object maps} *)

  type ('o, 'dec) map
  (** The type for mapping JSON objects to values of type ['o]. The
      ['dec] type is used to construct ['o] from members see {!val-mem}. *)

  val map : ?kind:string -> ?doc:string -> 'dec -> ('o, 'dec) map
  (** [map dec] is an empty JSON object decoded by [dec].
      {ul
      {- [kind] documents the entities represented by type ['o].
         Defaults to [""].}
      {- [doc] is a documentation string for [kind]. Defaults to [""].}
      {- [dec] is a constructor eventually returning a value of
         type ['o] to be saturated with calls to {!val-mem}, {!val-case_mem}
         or {!val-keep_unknown}. This is needed for decoding. Use {!enc_only}
         if the result is only used for encoding.}} *)

  val map' :
    ?kind:string -> ?doc:string -> ?enc_meta:(Context.t -> 'o -> Meta.t) ->
    (Context.t -> Meta.t -> 'dec) -> ('o, 'dec) map
  (** [map' dec] is like {!val-map} except you get the full decoding
      context in [dec] and [enc_meta] is used to recover it on encoding. *)

  val enc_only :
    ?kind:string -> ?doc:string -> ?enc_meta:(Context.t -> 'o -> Meta.t) ->
    unit -> ('o, 'a) map
  (** [enc_only ()] is like {!val-map'} but can only be used for
      encoding. *)

  val finish : ('o, 'o) map -> 'o t
  (** [finish m] is a JSON type for objects mapped by [m].  Raises
      [Invalid_argument] if [m] describes a member name more than
      once. *)

  val unfinish : 'o t -> ('o, 'o) map
  (** [unfinish t] is the object description of [t]. Raises
      [Invalid_argument] if [t] is not the result of {!finish}. *)

  val kind : ('o, _) map -> string
  (** [kind m] is the kind of object represented by ['o]. *)

  (** {1:mems Members} *)

  (** Member maps.

      Usually it's better to use {!Jsont.Obj.mem} or {!Jsont.Obj.opt_mem}
      directly. But this may be useful in certain abstraction contexts. *)
  module Mem : sig

    type ('o, 'dec) obj_map := ('o, 'dec) map

    type ('o, 'a) map
    (** The type for mapping a member object to a value ['a] stored
        in an OCaml value of type ['o]. *)

    val map :
      ?doc:string -> ?dec_absent:'a -> ?enc:('o -> 'a) ->
      ?enc_meta:('a -> Meta.t) ->
      ?enc_omit:('a -> bool) -> string -> 'a t -> ('o, 'a) map
    (** [map name t] maps member named [name] of type [t] in an object
        of type ['o]. See {!Jsont.Obj.mem} for the field semantics. *)

    val app : ('o, 'a -> 'b) obj_map -> ('o, 'a) map -> ('o, 'b) obj_map
    (** [app map mmap] applies the member map [map] to the contructor of
        the object map [mmap]. In turn this adds the [map] member definition
        to the object described by [map]. *)
  end

  val mem :
    ?doc:string -> ?dec_absent:'a -> ?enc:('o -> 'a) ->
    ?enc_meta:('a -> Meta.t) -> ?enc_omit:('a -> bool) -> string -> 'a t ->
    ('o, 'a -> 'b) map -> ('o, 'b) map
  (** [mem name t map] is a member named [name] of type
      [t] for an object of type ['o] being constructed by [map].
      {ul
      {- [doc] is a documentation string for the member. Defaults to [""].}
      {- [dec_absent], if specified, is the value used for the decoding
         direction when the member named [named] is missing. If unspecified
         decoding errors when the member is absent. See also {!opt_mem}
         and {{!page-cookbook.optional_members}this example}.}
      {- [enc] is used to project the member's value from the object
         representation ['o] for encoding to JSON with [t]. It can be omitted
         if the result is only used for decoding.}
      {- [enc_omit] is for the encoding direction. If the member value returns
         [true] the member is omited in the JSON. Defaults to [Fun.const false].
         See {{!page-cookbook.optional_members}this example}.}} *)

  val opt_mem :
    ?doc:string -> ?enc:('o -> 'a option) -> string -> 'a t ->
    ('o, 'a option -> 'b) map -> ('o, 'b) map
  (** [opt_mem name t map] is:
{[
  Jsont.Obj.mem name (Jsont.some t) map
    ~dec_absent:None ~enc_omit:Option.is_none
]}
      A shortcut to represent optional members of type ['a] with ['a option]
      values. *)

  (** {1:cases Case objects}

      This is for dealing with JSON object types or classes.
      See {{!page-cookbook.cases}this example}. *)

  (** Case objects.

      Case objects are used to describe objects whose member list depend
      on the tag value of a distinguished case member. See an
      {{!page-cookbook.cases}example}. *)
  module Case : sig

    type 'a jsont := 'a t

    type ('o, 'dec) obj_map := ('o, 'dec) map

    (** {1:cases Cases} *)

    type ('cases, 'case, 'tag) map
    (** The type for a case object represented by ['case] belonging to
        a common type represented by ['cases] depending on the value
        of a case member of type ['tag]. *)

    type ('cases, 'tag) t
    (** The type for a case part of the type ['cases]. This is
        {!type-t} with its ['case] representation hidden. *)

    type ('cases, 'tag) value
    (** The type for case values. This holds a case value and
        its case decription {!type-t}. Use {!val-value} to construct them. *)

    val map :
      ?dec:('case -> 'cases) -> 'tag -> 'case jsont ->
      ('cases, 'case, 'tag) map
    (** [map ~dec v obj] defines the object map [obj] as being the
        case for the tag value [v] of the case member. [dec] indicates how to
        inject the object case into the type for cases.

        Raises [Invalid_argument] if [obj] is not a direct result of
        {!finish}, that is does not describe an object. *)

    val make : ('cases, 'case, 'tag) map -> ('cases, 'tag) t
    (** [make cm] is [cm] as a case. *)

    val value : ('cases, 'case, 'tag) map -> 'case -> ('cases, 'tag) value
    (** [value c cv] is a case value [cv] described by [c]. *)
  end

  val case_mem :
    ?doc:string -> ?tag_compare:('tag -> 'tag -> int) ->
    ?tag_to_string:('tag -> string) -> ?dec_absent:'tag ->
    ?enc:('o -> 'cases) -> ?enc_omit:('tag -> bool) ->
    ?enc_case:('cases -> ('cases, 'tag) Case.value) -> string -> 'tag t ->
    ('cases, 'tag) Case.t list -> ('o, 'cases -> 'a) map -> ('o, 'a) map
  (** [case_mem name t cases map] is mostly like {!mem} except the member [name]
      selects an object representation according to the member value of type
      [t]:
      {ul
      {- [doc] is a documentation string for the member. Defaults to [""].}
      {- [tag_compare] is used to compare tags. Defaults to {!Stdlib.compare}}
      {- [tag_to_string] is used to stringify tags for improving
         error reporting.}
      {- [dec_absent], if specified, is the case value used for the decoding
         direction when the case member named [name] is missing. If unspecified
         decoding errors when the member is absent.}
      {- [enc] is  used to project the value in which cases are stored
         from the object representation ['o] for encoding to JSON. It
         can be omitted if the result is only used for decoding.}
      {- [enc_case] determines the actual case value from the value returned
         by [enc].}
      {- [cases] enumerates all the cases, it is needed for decoding.}}

      [map], [name] and the object maps of cases must declare disjoint
      member names otherwise [Invalid_argument] is raised on
      {!finish}. Raises [Invalid_argument] if [case_mem] was already called
      on map. *)

  (** {1:unknown_members Unknown members}

      These functions define the behaviour on unknown members.  By
      default unknown members are skipped.

      On {{!cases}case objects} each individual case has its own
      behaviour unless the combinators are used on the case object map
      in which case it overrides the behaviour of cases. For those
      that use {!keep_unknown} they will get the result of an empty
      builder in their decoding function and the encoder is ignored on
      encode. *)

  (** Uniform members. *)
  module Mems : sig
    type 'a jsont := 'a t

    type ('mems, 'a, 'builder) map
    (** The type for mapping members of type ['a] to values of type ['mems]. *)

    type ('mems, 'a) enc =
      { enc :
          'acc. Context.t -> (Meta.t -> string -> 'a -> 'acc -> 'acc) ->
          'mems -> 'acc -> 'acc }
    (** The type for specifying unknown member folds. *)

    val map :
      ?kind:string -> ?doc:string -> 'a jsont ->
      dec_empty:(Context.t -> 'builder) ->
      dec_add:(Context.t -> Meta.t -> string -> 'a -> 'builder -> 'builder) ->
      dec_finish:(Context.t -> 'builder -> 'mems) ->
      enc:('mems, 'a) enc -> ('mems, 'a, 'builder) map
      (** [map type' ~empty ~add ~fold] defines a structure of type
          ['mems] to hold unknown members with values of type
          ['a]. [empty] is the empty collection, [add] adds a member
          to the collection and [fold] folds over it. See {!keep_unknown}. *)

    val string_map :
      ?kind:string -> ?doc:string -> 'a jsont ->
      ('a Stdlib.Map.Make(String).t, 'a, 'a Stdlib.Map.Make(String).t) map
      (** [string_map t] collects unknown member by name and types their
          values with [t]. See {!keep_unknown} and see also {!as_string_map}. *)
  end

  val skip_unknown : ('o, 'dec) map -> ('o, 'dec) map
  (** [skip_unknown map] makes [map] skip unknown members. This is the
      default, no need to specify it. Raises [Invalid_argument] if
      {!keep_unknown} was already specified on [map]. *)

  val error_unknown : ('o, 'dec) map -> ('o, 'dec) map
  (** [error_unknown m] makes [m] error on unknown members. Raises
      [Invalid_argument] if {!keep_unknown} was already specified on
      [map]. See {{!page-cookbook.erroring}this example}. *)

  val keep_unknown :
    ?enc:('o -> 'mems) -> ('mems, _, _) Mems.map ->
    ('o, 'mems -> 'a) map -> ('o, 'a) map
  (** [keep_unknown mems m] makes [m] keep unknown member with [mems].
      Raises [Invalid_argument] if {!keep_unknown} was already
      specified on [map]. See this {{!page-cookbook.keeping}this
      example}. *)

  (** {1:types JSON types } *)

  val as_string_map :
    ?kind:string -> ?doc:string -> 'a t -> 'a Stdlib.Map.Make(String).t t
  (** [as_string_map t] maps object to key-value maps of type [t].
      See also {!Mems.string_map}. *)

  val ignore : unit t
  (** [ignore] ignores JSON objects on decoding and errors on encoding. *)
end

val any :
  ?kind:string -> ?doc:string -> ?dec_null:'a t -> ?dec_bool:'a t ->
  ?dec_number:'a t -> ?dec_string:'a t -> ?dec_array:'a t ->
  ?dec_obj:'a t -> ?enc:(Context.t -> 'a -> 'a t) -> unit -> 'a t
(** [any ()] maps subsets of JSON value of different sorts to values
    of type ['a]. The unspecified cases are not part of the subset and
    error on decoding. [enc] selects the type on encoding and errors
    if omitted. [kind] is the kind of JSON value represented and [doc]
    a documentation string. *)

val map :
  ?kind:string -> ?doc:string -> ?dec:(Context.t -> 'a -> 'b) ->
  ?enc:(Context.t -> 'b -> 'a) -> 'a t -> 'b t
(** [map t] changes the type of [t] from ['a] to ['b].
    {ul
    {- [kind] names the entities represented by type ['b].
       Defaults to [""].}
    {- [doc] is a documentation string for [kind]. Defaults to [""].}
    {- [dec] decodes values of type ['a] to values of type ['b].
       Can be omitted if the result is only used for
       encoding. The default errors.}
    {- [enc] encodes values of type ['b] to values of type ['a].
       Can be omitted if the result is only used for
       decoding. The default errors.}} *)

val rec' : 'a t Lazy.t -> 'a t
(** [rec'] maps recursive JSON values. See the {{!page-cookbook.recursion}
    cookbook}. *)

(** {2:ignoring Ignoring}

    See also {!const}. *)

val ignore : unit t
(** [ignore] lossily maps all JSON values to [()] on decoding and
    errors on encoding. *)

val todo : ?kind:string -> ?doc:string -> ?dec_stub:'a -> unit -> 'a t
(** [todo ?dec_stub ()]  maps all JSON values to [dec_stub] if
    specified (errors otherwise) and errors on encoding. *)

(** {2:base Base types} *)

val null : ?kind:string -> ?doc:string -> 'a -> 'a t
(** [null v] maps JSON nulls to [v]. On encodes
    any value of type ['a] is encoded by null. [doc] and [kind] are
    given to the underlying {!Base.map}. See also {!Base.null}. *)

val bool : bool t
(** [bool] maps JSON booleans to [bool] values. See also {!Base.bool}. *)

val number : float t
(** [number] maps JSON nulls or numbers to [float] values.  On decodes
    JSON null is mapped to {!Float.nan}. On encodes any
    {{!Float.is_finite}non-finite} float is lossily mapped to JSON
    null ({{!page-cookbook.numbers_as_nulls}explanation}). See also
    {!Base.number}, {!any_float} and {{!ints}integer combinators}. *)

val string : string t
(** [string] maps unescaped JSON strings to UTF-8 encoded [string]
    values. See also {!Base.string}.

    {b Warning.} Encoders assume OCaml [string]s have been checked for
    UTF-8 validity.  *)

(** {2:option Options} *)

val none : 'a option t
(** [none] maps JSON nulls to [None]. *)

val some : 'a t -> 'a option t
(** [some t] maps JSON like [t] does but wraps results in [Some].
    Encoding fails if the value is [None]. *)

val option : ?kind:string -> ?doc:string -> 'a t -> 'a option t
(** [option t] maps JSON nulls to [None] and other values by [t]. *)

(** {2:ints Integers}

    See {{!page-cookbook.numbers_as_integers}this note} about (not)
    representing integers by JSON number values. *)

val int : int t
(** [int] maps truncated JSON numbers or JSON strings to [int] values.
    {ul
    {- JSON numbers are sucessfully decoded if after truncation they can
       be represented on the [int] range, otherwise the decoder
       errors. [int] values are encoded as JSON numbers if the
       integer is in the \[-2{^53};2{^53}\] range.}
    {- JSON strings are decoded using {!int_of_string_opt}, this
       allows binary, octal, decimal and hex syntaxes and errors on
       overflow and syntax errors. [int] values are encoded as JSON
       strings with {!Int.to_string} when the integer is outside the
       \[-2{^53};2{^53}\] range}} *)

val uint8 : int t
(** [uint8] maps JSON numbers to unsigned 8-bit integers. JSON numbers
    are sucessfully decoded if after truncation they can be represented
    on the \[0;255\] range. Encoding errors if the integer is out of
    range.*)

val uint16 : int t
(** [uint16] maps JSON numbers to unsigned 16-bit integers. JSON numbers
    are sucessfully decoded if after truncation they can be represented
    on the \[0;65535\] range. Encoding errors if the integer is out of
    range.*)

val int8 : int t
(** [int8] maps JSON numbers to 8-bit integers. JSON numbers
    are sucessfully decoded if after truncation they can be represented
    on the \[-128;127\] range. Encoding errors if the integer is out of
    range.*)

val int16 : int t
(** [int16] maps JSON numbers to 16-bit integers. JSON numbers
    are sucessfully decoded if after truncation they can be represented
    on the \[-32768;32767\] range. Encoding errors if the integer is out
    of range. *)

val int32 : int32 t
(** [int32] maps JSON numbers to 32-bit integers. JSON numbers
    are sucessfully decoded if after truncation they can be represented
    on the [int32] range, otherwise the decoder errors. *)

val int64 : int64 t
(** [int] maps truncated JSON numbers or JSON strings to 64-bit
    integers.
    {ul
    {- JSON numbers are sucessfully decoded if after truncation they can
       be represented on the [int64] range, otherwise the decoder
       errors. [int64] values are encoded as JSON numbers if the
       integer is in the \[-2{^53};2{^53}\] range.}
    {- JSON strings are decoded using {!int_of_string_opt}, this
       allows binary, octal, decimal and hex syntaxes and errors on
       overflow and syntax errors. [int] values are encoded as JSON
       strings with {!Int.to_string} when the integer is outside the
       \[-2{^53};2{^53}\] range}} *)

val int_as_string : int t
(** [int_as_string] maps JSON strings to [int] values. On
    decodes this uses {!int_of_string_opt} which allows binary,
    octal, decimal and hex syntaxes and errors on overflow and
    syntax errors. On encodes uses {!Int.to_string}. *)

val int64_as_string : int64 t
(** [int64_as_string] maps JSON strings to 64-bit integers. On
    decodes this uses {!Int64.of_string_opt} which allows binary,
    octal, decimal and hex syntaxes and errors on overflow and
    syntax errors. On encodes uses {!Int64.to_string}. *)

(** {2:float Floats} *)

val any_float : float t
(** [any_float] is a lossless representation for IEEE 754 doubles. It
    maps {{!Float.is_finite}non-finite} floats by the JSON
    strings defined by {!Float.to_string}. This contrasts with
    {!val-number} which maps them to JSON null values
    ({{!page-cookbook.numbers_as_nulls}explanation}). Note that on
    decodes this still maps JSON nulls to {!Float.nan} and any successful
    string decode of {!Float.of_string_opt} (so numbers can also be written
    as strings). See also {!val-number}.

    {b Warning.} [any_float] should only be used between parties that
    have agreed on such an encoding. To maximize interoperability you
    should rather use the lossy {!val-number} map. *)

val float_as_hex_string : float t
(** [float_as_hex_string] maps JSON strings made of IEEE 754 doubles in hex
    notation to float values. On encodes strings this uses the ["%h"]
    format string. On decodes it accepts anything sucessfully decoded
    by {!Float.of_string_opt}. *)

(** {2:enums Strings and enums} *)

val of_of_string : ?kind:string -> ?doc:string ->
  ?enc:('a -> string) -> (string -> ('a, string) result) -> 'a t
(** [of_of_string of_string] maps JSON string with a {{!Base.map}base_map} using
    [of_string] for decoding and [enc] for encoding. *)

val enum :
  ?cmp:('a -> 'a -> int) -> ?kind:string -> ?doc:string ->
  (string * 'a) list -> 'a t
(** [enum assoc] maps JSON strings member of the [assoc] list to the
    corresponding OCaml value and vice versa (in log(n)).
    [cmp] is used to compare the OCaml values, it defaults to {!Stdlib.compare}.
    Decoding and encoding error on strings or values not part of
    [assoc] *)

val binary_string : string t
(** [binary_string] maps JSON strings made of an even number of
    hexdecimal ASCII upper or lower case digits to the corresponding
    byte sequence. On encoding uses only lower case hexadecimal
    digits to encode the byte sequence. *)

(** {2:derived_arrays Arrays and tuples}

    See also {{!array_queries}queries and updates}. *)

val list :
  ?kind:string -> ?doc:string -> 'a t -> 'a list t
(** [list t] maps JSON arrays of type [t] to [list] values.
    See also {!Array.list_map}. *)

val array : ?kind:string -> ?doc:string -> 'a t -> 'a array t
(** [array t] maps JSON arrays of type [t] to [array] values.
    See also {!Array.array_map}. *)

val array_as_string_map :
  ?kind:string -> ?doc:string -> key:('a -> string) -> 'a t ->
  'a Map.Make(String).t t
(** [array_as_string_map ~key t] maps JSON array elements of type [t] to
    string maps by indexing them with [key]. If two elements have
    the same [key] the element with the greatest index takes over. *)

val bigarray :
  ?kind:string -> ?doc:string -> ('a, 'b) Bigarray.kind -> 'a t ->
  ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t t
(** [bigarray k t] maps JSON arrays of type [t] to [Bigarray.Array1.t] values.
    See also {!Array.bigarray_map}. *)

val t2 :
  ?kind:string -> ?doc:string -> ?dec:('a -> 'a -> 't2) ->
  ?enc:('t2 -> int -> 'a) -> 'a t -> 't2 t
(** [t2 ?dec ?enc t] maps JSON arrays with exactly 2 elements of type
    [t] to value of type ['t2]. Decodes error if there are more
    elements. [enc v i] must return the zero-based [i]th element. *)

val t3 :
  ?kind:string -> ?doc:string -> ?dec:('a -> 'a -> 'a -> 't3) ->
  ?enc:('t3 -> int -> 'a) -> 'a t -> 't3 t
(** [t3] is like {!t2} but for 3 elements. *)

val t4 :
  ?kind:string -> ?doc:string -> ?dec:('a -> 'a -> 'a -> 'a -> 't4) ->
  ?enc:('t4 -> int -> 'a) -> 'a t -> 't4 t
(** [t4] is like {!t2} but for 4 elements. *)

val tn : ?kind:string -> ?doc:string -> n:int -> 'a t -> 'a array t
(** [tn ~n t] maps JSON arrays of exactly [n] elements of type [t] to
    [array] values. This is {!val-array} limited by [n]. *)

(** {2:context Context} *)

val context : Context.t t
(** [context] maps any JSON to its decoding context and errors on encoding. *)

val with_context : 'a t -> ('a * Context.t) t
(** [with_context t] maps JSON like [t] does but on decodes returns
    the context in which [t] is used. On encodes the context is
    ignored and dropped. *)

(** {1:generic_json Generic JSON} *)

type name = string node
(** The type for JSON member names. *)

type mem = name * json
(** The type for JSON object members. *)

and obj = mem list
(** The type for JSON objects. *)

and json =
| Null of unit node
| Bool of bool node
| Number of float node
(** Encoders must use [Null] if float is {{!Float.is_finite}not finite}. *)
| String of string node
| Array of json list node
| Obj of obj node (** *)
(** The type for generic JSON values. *)

(** Generic JSON values. *)
module Json : sig

  (** {1:json JSON values} *)

  val meta : json -> Meta.t
  (** [meta v] is the metadata of value [v]. *)

  val sort : json -> Sort.t
  (** [sort v] is the sort of value [v]. *)

  val find_mem : string -> obj -> mem option
  (** [find_mem n ms] find the first member whose name matches [n] in [ms]. *)

  val find_mem' : name -> obj -> mem option
  (** [find_mem n ms] is [find_mem (fst n) ms]. *)

  val obj_names : obj -> string list
  (** [obj_names ms] are the names [ms]. *)

  val obj_names' : obj -> name list
  (** [obj_names ms] are the names [ms]. *)

  (** {1:cons Constructors} *)

  type 'a cons = ?meta:Meta.t -> 'a -> json
  (** The type for constructing JSON values from an OCaml value of type ['a].
      [meta] defaults to {!Meta.none}. *)

  (** {2:base Base types} *)

  val null : unit cons
  (** [null] is [Null (unit, meta)]. *)

  val bool : bool cons
  (** [bool b] is [Bool (b, meta)]. *)

  val number : float cons
  (** [number n] is [Number (n, meta)]. *)

  val string : string cons
  (** [string s] is [String (s, meta)]. *)

  val array : json array cons
  (** [array l] is [Array (Array.to_list a, meta)]. See also {!list}. *)

  (** {2:objects Objects} *)

  val name : ?meta:Meta.t -> string -> name
  (** [name ?meta n] is [(n, meta)]. [meta] defaults to {!Meta.none}. *)

  val mem : name -> json -> mem
  (** [mem n v] is [(n, v)]. [meta] defaults to {!Meta.none}. *)

  val obj : obj cons
  (** [obj o] is [Obj (o, meta)]. *)

  (** {2:derived Derived} *)

  val option : 'a cons -> 'a option cons
  (** [option c] constructs [Some v] values with [c v] and [None] ones
      with {!null}. *)

  val list : json list cons
  (** [list l] is [Array (l, meta)]. *)

  val int : int cons
  (** [int] is [i] as a JSON number or a JSON string if not
        in the range \[-2{^53};2{^53}\]. See also {!int_as_string}. *)

  val int32 : int32 cons
  (** [int32] is [i] as a JSON number. *)

  val int64 : int64 cons
    (** [int64 i] is [i] as a JSON number or a JSON string if
        not in the range \[-2{^53};2{^53}\]. See also {!int64_as_string}. *)

  val int_as_string : int cons
  (** [int_as_string i] is [i] as a JSON string. See also {!int}. *)

  val int64_as_string : int64 cons
  (** [int64_as_string i] is [i] as a JSON string. See also {!int64}. *)

  val any_float : float cons
  (** [any_float v] is [number v] if {!Float.is_finite}[ v] is [true]
        and [string (Float.to_string v)] otherwise. See {!Jsont.any_float}. *)

  val stub : json cons
  (** [stub j] is a stub value of the sort value of [j]. The stub
      value is the “natural” zero: null, false, 0, empty string,
      empty array, empty object. *)

  (** {1:converting Converting} *)

  val decode : ?ctx:Context.t -> 'a t -> json -> ('a, string) result
  (** [dcode t j] decodes a value from the generic JSON [j] according
      to type [t]. [ctx] defaults to {!Context.root}. *)

  val decode' : ?ctx:Context.t -> 'a t -> json -> ('a, Error.t) result
  (** [decode'] is like {!decode} but preserves the error structure. *)

  val encode : ?ctx:Context.t -> 'a t -> 'a -> (json, string) result
  (** [encode t v] encodes a generic JSON value for [v] according
      to type [t]. [ctx] default to {!Contet.root}. *)

  val encode' : ?ctx:Context.t -> 'a t -> 'a -> (json, Error.t) result
  (** [encode'] is like {!encode} but preserves the error structure. *)

  (*
  (** {1:errors Errors} *)

  val error_sort : Context.t -> exp:Sort.t -> json -> 'a
  (** [error_sort ctx ~exp fnd] errors when sort [exp] was expected
        but generic JSON [fnd] was found. *)

  val error_type : Context.t -> 'a t -> json -> 'a
  (** [error_type ctx ~exp fnd] is [error_kind ctx ~exp:(kind t) fnd]. *)
*)

  (** {1:std Standard module interface} *)

  type t = json
  (** See {!json}. *)

  val equal : t -> t -> bool
  (** [equal j0 j1] is {!compare}[ j0 j1 = 0]. *)

  val compare : t -> t -> int
  (** [compare j0 j1] is a total order on JSON values:
      {ul
      {- Floating point values are compared with {!Float.compare},
         this means NaN values are equal.}
      {- Strings are compared byte wise.}
      {- Objects members are sorted before being compared.}
      {- Metadata is ignored.}} *)

  val pp : t fmt
  (** See {!Jsont.pp_json}. *)
end

(** {2:gen_json_types Types} *)

val json : json t
(** [json] maps any JSON value to its generic representation. *)

val json_null : json t
(** [json_null] maps JSON nulls to their generic representation. *)

val json_bool : json t
(** [json_bool] maps JSON booleans to their generic representation. *)

val json_number : json t
(** [json_number] maps JSON nulls or numbers
    ({{!page-cookbook.numbers_as_nulls}explanation}) to their generic
    representation. *)

val json_string : json t
(** [json_string] represents JSON strings by their generic representation. *)

val json_array : json t
(** [json_array] represents JSON arrays by their generic representation. *)

val json_obj : json t
(** [json_obj] represents JSON objects by their generic representation. *)

val json_mems : (json, json, mem list) Obj.Mems.map
(** [json_mems] is a members map collecting unknown members into a
    generic JSON object. *)

(** {1:queries Queries and updates}

    Queries are lossy or aggregating decodes. Updates decode to
    {!json} values but transform the data along the way. They allow to
    process JSON data without having to fully model it
    (see the update example in the {{!page-index.quick_start}quick start}). *)

val const : 'a t -> 'a -> 'a t
(** [const t v] maps any JSON value to [v] on decodes and
    unconditionally encodes [v] with [t]. *)

val recode : dec:'a t -> (Context.t -> 'a -> 'b) -> enc:'b t -> 'b t
(** [recode ~dec f ~enc] maps on decodes like [dec] does followed by
    [f] and on encodes uses [enc]. This can be used to change the JSON
    sort of value. For example:
{[
recode ~dec:int (fun _ i -> string_of_int s) ~enc:string
]}
    decodes an integer but encodes the integer as a string. *)

val update : 'a t -> json t
(** [update t] decodes any JSON with [t] and directly encodes it back
    with [t] to yield the decode result. Encodes any JSON like {!json}
    does. *)

(** {2:array_queries Arrays} *)

val nth : ?absent:'a -> int -> 'a t -> 'a t
(** [nth n t] decodes the [n]th index of a JSON array with [t]. Other
    indices are skipped. The decode errors if there is no such index
    unless [absent] is specified in which case this value is returned.
    Encodes a singleton array. *)

val set_nth : ?stub:json -> ?allow_absent:bool -> 'a t -> int -> 'a -> json t
(** [set_nth t n v] on decodes sets the [n]th value of a JSON array to
    [v] encoded by [t]. Other indices are left untouched. Errors if
    there is no such index unless [~allow_absent:true] is specified in
    which case the index is created preceeded by as many [stub]
    indices as needed. [stub] defaults to {!Json.stub} applied to the
    value [v] encoded by [t]. Encodes like {!json_array} does. *)

val update_nth : ?stub:json -> ?absent:json -> int -> 'a t -> json t
(** [update_nth n t] on decode recodes the [n]th value of a JSON array
    with [t]. Errors if there is no such index unless [absent] is
    specified in which case the index is created with [absent],
    recoded with [t] and preceeded by as many [stub] values as
    needed. [stub] defaults to {!Json.stub} applied to the recode.
    Encodes like {!json_array} does. *)

val delete_nth : ?allow_absent:bool -> int -> json t
(** [delete_nth n] drops the [n]th index of a JSON array on both
    decode and encodes. Other indices are left untouched.  Errors if
    there is no such index unless [~allow_absent:true] is specified in
    which case the data is left untouched. *)

val filter_map_array :
  'a t -> 'b t -> (Context.t -> int -> 'a -> 'b option) -> json t
(** [filter_map_array a b f] maps the [a] elements of a JSON array
    with [f] to [b] elements or deletes them on [None]. Encodes
    generic JSON arrays like {!json_array} does. *)

val fold_array : 'a t -> (Context.t -> int -> 'a -> 'b -> 'b) -> 'b -> 'b t
(** [fold_array t f acc] fold [f] over the [t] elements of a JSON
    array starting with [acc]. Encodes an empty JSON array. *)

(** {2:objects Objects} *)

val mem : ?absent:'a -> string -> 'a t -> 'a t
(** [mem name t] decodes the member named [name] of a JSON object with
    [t]. Other members are skipped. The decode errors if there is no
    such member unless [absent] is specified in which case this value
    is returned. Encodes an object with a single [name] member. *)

val set_mem : ?allow_absent:bool -> 'a t -> string -> 'a -> json t
(** [set_mem t name v] sets the member value of [name] of a [JSON]
    object to an encoding of [v] with [t]. This happens both on
    decodes and encodes. Errors if there is no such member unless
    [allow_absent:true] is specified in which case a member is added
    to the object. *)

val update_mem : ?absent:json -> string -> 'a t -> json t
(** [update_mem name t] recodes the member value of [name] of a JSON
    object with [t]. This happens both on decodes and encodes.  Errors
    if there is no such member unless [absent] is specified in which
    case a member with this value is added to the object and recoded
    with [t]. *)

val delete_mem : ?allow_absent:bool -> string -> json t
(** [delete_mem name] deletes the member named [name] of a JSON object
    on decode. Other members are left untouched. The decode errors if
    there is no such member unless [~allow_absent:true] is specified
    in which case the data is left untouched. Encodes generic JSON
    objects like {!json_obj} does. *)

val fold_obj :
  'a t -> (Context.t -> Meta.t -> string -> 'a -> 'b -> 'b) -> 'b -> 'b t
(** [fold_obj t f acc] folds [f] over the [t] members of a JSON object
    starting with [acc]. Encodes an empty JSON object. *)

val filter_map_obj :
  'a t -> 'b t ->
  (Context.t -> Meta.t -> string -> 'a -> (string * 'b) option) -> json t
(** [filter_map_obj a b f] maps the [a] members of a JSON object with
    [f] to [(n, b)] members or deletes them on [None]. Encodes generic
    JSON arrays like {!json_obj} does. *)

(** {2:indices Indices} *)

val index : ?absent:'a -> Path.index -> 'a t -> 'a t
(** [index] uses {!nth} or {!mem} on the given index. *)

val set_index : ?allow_absent:bool -> 'a t -> Path.index -> 'a -> json t
(** [set_index] uses {!set_nth} or {!set_mem} on the given index. *)

val update_index : ?stub:json -> ?absent:json -> Path.index -> 'a t -> json t
(** [update_index] uses {!update_nth} or {!update_mem} on the given index. *)

val delete_index : ?allow_absent:bool ->  Path.index -> json t
(** [delete_index] uses {!delete_nth} or {!delete_mem} on the given index. *)

(** {2:paths Paths} *)

val path : ?absent:'a -> Path.t -> 'a t -> 'a t
(** [path p t] {{!index}decodes} with [t] on the last index of [p]. On
    the {!root} path this is [t]. *)

val set_path :
  ?stub:json -> ?allow_absent:bool -> 'a t -> Path.t -> 'a -> json t
(** [set_path t p v] {{!set_index}sets} the last index of [p]. On
    the root paths this results in the encoding of [v]. *)

val update_path : ?stub:json -> ?absent:json -> Path.t -> 'a t -> json t
(** [update_path p t] {{!update_index}updates} the last index of [p] with
    [t]. On the root path this is [t]. *)

val delete_path : ?allow_absent:bool -> Path.t -> json t
(** [delete_path p] {{!delete_index}deletes} the last index of [p]. On
    the root path this results in {!Json.null}[ ()]. *)

(** {1:carets Carets} *)

(** JSON carets (TODO). *)
module Caret : sig

  (** {1:caret Carets} *)

  type pos =
  | Before (** The void before the data indexed by a path. *)
  | Over (** The data indexed by a path. *)
  | After (** The void after the data indexed by a path. *)
  (** The type for caret positions. *)

  type t = Path.t * pos
  (** The type for carets. A path and the caret position. *)

  val of_string : string -> (t, string) result
  (** [of_string s] parses a caret according to
      the {{!path_caret_syntax}caret syntax} .*)

  val pp : t fmt
  (** [pp] formats carets. *)

  (** {1:path_caret_syntax Path & caret end-user syntax}

      Path and carets provide a way for end users to address JSON and
      edit locations.

      A {e path} is a sequence of key and list indexing
      operations. Applying the path to a JSON value leads to a JSON
      construct or nothing if one of the indices does not exist, or an
      error if ones tries to index a non-indexable value.

      A {e caret} is a path and a spatial specification for the JSON
      construct found by the path. The caret indicates either the void
      {e before} that JSON construct, the JSON value itself ({e over}) or
      the void {e after} it.

      Here are a few examples of paths and carets, syntactically the
      charater ['v'] is used to denote the caret's insertion point before or
      after a path. There's no distinction between a path and an over caret.

{@json[
{ "ocaml":
      { "libs": ["jsont", "brr", "cmdliner"] }}
]}

{@shell[
ocaml.libs        # value of member "libs" of member "ocaml"
ocaml.v[libs]     # void before the "libs" member
ocaml.[libs]v     # void after "libs" member

ocaml.libs.[0]    # first element of member "libs" of member "ocaml"
ocaml.libs.v[0]   # void before first element
ocaml.libs.[0]v   # void after first element

ocaml.libs.[-1]   # last element of member "libs" of member "ocaml"
ocaml.libs.v[-1]  # before last element (if any)
ocaml.libs.[-1]v  # after last element (if any)
]}

    More formally a {e path} is a [.] seperated list of indices.

    An {e index} is written [[i]]. [i] can a zero-based list index
    with negative indices counting from the end of the list ([-1] is
    the last element). Or [i] can be an object member name [n]. If there
    is no ambiguity, the surrounding brackets can be dropped.

    A {e caret} is a path whose last index brackets can be prefixed or
    suffixed by an insertion point, represented by the character
    ['v'].  This respectively denote the void before or after the
    JSON construct found by the path.

      {b Note.} FIXME
      {ul
      {- The syntax has no form of quoting at the moment this
         means key names can't contain, [\[], [\]], or start with a number.}
      {- For paths we should consider reusing a subset of the JSONPath syntax}} *)
end

(** {1:fmt Formatting} *)

type format =
| Minify (** Compact. No whitespace, no newlines. *)
| Indent (** Indented output (not necessarily pretty). *)
| Layout (** Follow {!Meta} layout information. *)
(** The type for specifying JSON encoding formatting. See for example
    {!Jsont_bytesrw.val-encode}. *)

type number_format = (float -> unit, Format.formatter, unit) Stdlib.format
(** The type for JSON number formatters. *)

val default_number_format : number_format
(** [default_number_format] is ["%.17g"]. This number formats ensures
    that finite floating point values can be interchanged without loss
    of precision. *)

(** {2:pp Pretty-printing} *)

val pp_null : unit fmt
(** [pp_null] formats a JSON null. *)

val pp_bool : bool fmt
(** [pp_bool] formats a JSON bool. *)

val pp_number : float fmt
(** [pp_number] formats a JSON number of a JSON null if the float
    is not finite. Uses the {!default_number_format}. *)

val pp_number' : number_format -> float fmt
(** [pp_number fmt] is like {!pp_number} but uses [fmt] to format the
    number. *)

val pp_string : string fmt
(** [pp_string] formats a JSON string (quoted and escaped). Assumes
    the string is valid UTF-8. *)

val pp_json : json fmt
(** [pp_json] formats JSON, see {!pp_json'}. *)

val pp_json' : ?number_format:number_format -> unit -> json fmt
(** [pp' ~format ~number_format () ppf j] formats [j] on [ppf]. The output
    is indented but may be more compact than an [Indent] JSON encoder may do.
    For example arrays may be output on one line if they fit etc.
    {ul
    {- [number_format] is used to format JSON numbers. Defaults to
       {!default_number_format}}
    {- Non-finite numbers are output as JSON nulls
       ({{!page-cookbook.numbers_as_nulls}explanation}).}
    {- Strings are assumed to be valid UTF-8.}} *)

val pp_value : ?number_format:number_format -> 'a t -> unit -> 'a fmt
(** [pp_value t ()] formats the JSON representation of values as
    described by [t] by encoding it with {!Json.encode} and formatting
    it with {!pp_json'}. If the encoding of the value errors a JSON
    string with the error message is output. *)

(** {1:low Low-level representation} *)

(** Low level representation (unstable).

    This representation may change even between minor versions of the
    library. It can be used to devise new processors on JSON types.

    Processors should be ready to catch the {!Json.Error} exception
    when they invoke functional members of the representation.

    Processors should make sure they interpret mappings
    correctly. In particular:
    {ul
    {- The [Number] case represents the sets of JSON numbers and nulls.}}

    See the source of {!Json.decode'} and {!Json.encode'}
    for a simple example on how to process this representation. The
    {{:https://github.com/dbuenzli/jsont/tree/master/paper}paper}
    in the project repository may also help to understand this menagerie
    of types. *)
module Repr : sig
  type 'a t' := 'a t

  module String_map : Map.S with type key = string
  (** A [Map.Make(String)] instance. *)

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

  type ('ret, 'f) dec_fun =
  | Dec_fun : 'f -> ('ret, 'f) dec_fun
    (** The function and its return type. *)
  | Dec_app : ('ret, 'a -> 'b) dec_fun * 'a Type.Id.t -> ('ret, 'b) dec_fun
    (** Application of an argument to a function witnessed by a type
        identifier. The type identifier can be used to lookup a value
        of the right type in an heterogenous dictionary. *)
  (** The type for decoding functions. *)

  (** {1:base Base value maps} *)

  type ('a, 'b) base_map =
  { kind : string;
    (** The kind of JSON value that are mapped (documentation) *)
    doc : string;
    (** A doc string for the kind of JSON value. *)
    dec : Context.t -> Meta.t -> 'a -> 'b;
    (** [dec] decodes a base value represented by its metadata and ['a] to
        ['b]. *)
    enc : Context.t -> 'b -> 'a;
    (** [enc] encodes a value of type ['b] to a base JSON value represented
        by ['a]. *)
    enc_meta : Context.t -> 'b -> Meta.t;
    (** [enc_meta] recovers the base JSON value metadata from ['b] (if any). *)
  }
  (** The type for mapping JSON base values represented in OCaml by
      ['a] (these values are fixed by the cases in {!t}) to a value of
      type ['b]. *)

  (** {1:types JSON types} *)

  type 'a t =
  | Null : (unit, 'a) base_map -> 'a t (** Null maps. *)
  | Bool : (bool, 'a) base_map -> 'a t (** Boolean maps. *)
  | Number : (float, 'a) base_map -> 'a t (** Number maps. *)
  | String : (string, 'a) base_map -> 'a t (** String maps. *)
  | Array : ('a, 'elt, 'builder) array_map -> 'a t (** Array maps. *)
  | Obj : ('o, 'o) obj_map -> 'o t (** Object maps. *)
  | Any : 'a any_map -> 'a t (** Map for different sorts of JSON values. *)
  | Map : ('b, 'a) map -> 'a t (** Map from JSON type ['b] to JSON type ['a]. *)
  | Rec : 'a t Lazy.t -> 'a t (** Recursive definition. *)
  (** The type for JSON types. *)

  (** {1:array Array maps} *)

  and ('array, 'elt, 'builder) array_map =
  { kind : string;
    (** The kind of JSON array mapped (documentation). *)
    doc : string;
    (** Documentation string for the JSON array. *)
    elt : 'elt t;
    (** The type for the array elements. *)
    dec_empty : Context.t -> 'builder;
    (** [dec_empty ctx] creates a new empty array builder. *)
    dec_skip : Context.t -> int -> 'builder -> bool;
    (** [dec_skip ctx i b] determines if the [i]th index of the JSON
        array can be skipped. *)
    dec_add : Context.t -> int -> 'elt -> 'builder -> 'builder;
    (** [dec_add] adds the [i]th index value of the JSON array
        as decoded by [elt] to the builder. *)
    dec_finish : Context.t -> Meta.t -> int -> 'builder -> 'array;
    (** [dec_finish] turns the builder into an array given its
        metadata and length. *)
    enc :
      'acc. Context.t -> ('acc -> int -> 'elt -> 'acc) -> 'acc -> 'array ->
      'acc;
    (** [enc] folds over the elements of the array for encoding. *)
    enc_meta : Context.t -> 'array -> Meta.t;
    (** [enc_meta] recovers the metadata of an array (if any). *) }
  (** The type for mapping JSON arrays to values of type ['array]
      with array elements mapped to type ['elt] and using a ['builder]
      value to construct the array. The context values given to any of the
      function should always be the context of the array; an index should
      be {{!Context.push_nth}pushed} on this context when using [elt] to
      process a given array element. *)

  (** {1:obj_map Object maps} *)

  and ('o, 'dec) obj_map =
  { kind : string;
    (** The kind of JSON object (documentation). *)
    doc : string;
    (** A doc string for the JSON member. *)
    dec : ('o, 'dec) dec_fun;
    (** The object decoding function to construct an ['o] value. *)
    mem_decs : mem_dec String_map.t;
    (** [mem_decs] are the member decoders sorted by member name. *)
    mem_encs : 'o mem_enc list;
    (** [mem_encs] is the list of member encoders. *)
    enc_meta : Context.t -> 'o -> Meta.t;
    (** [enc_meta] recovers the metadata of an object (if any). *)
    shape : 'o obj_shape;
    (** [shape] is the {{!obj_shape}shape} of the object. *) }
  (** The type for mapping a JSON object to values of type ['o] using
      a decoding function of type ['dec]. [mem_decs] and [mem_encs]
      have the same {!mem_map} values they are just sorted
      differently for decoding and encoding purposes. *)

  and mem_dec = Mem_dec : ('o, 'a) mem_map -> mem_dec
  (** The type for member maps in decoding position. *)

  and 'o mem_enc = Mem_enc : ('o, 'a) mem_map -> 'o mem_enc
  (** The type for member maps in encoding position. *)

  and ('o, 'a) mem_map =
  { name : string;
    (** The JSON member name. *)
    doc : string;
    (** Documentation for the JSON member. *)
    type' : 'a t;
    (** The type for the member value. *)
    id : 'a Type.Id.t;
    (** A type identifier for the member. This allows to store
        the decode in a {!Dict.t} on decode and give it in time
        to the object decoding function of the object map. *)
    dec_absent : 'a option;
    (** The value to use if absent (if any). *)
    enc : Context.t -> 'o -> 'a;
    (** [enc] recovers the value to encode from ['o]. *)
    enc_meta : Context.t -> 'a -> Meta.t; (* FIXME *)
    enc_omit : Context.t -> 'a -> bool;
    (** [enc_omit] is [true] if the result of [enc] should
        not be encoded. *)
  }
  (** The type for mapping a JSON member to a value of type ['a] in
      an object represented by a value of type ['o]. *)

  and 'o obj_shape =
  | Obj_basic : ('o, 'mems, 'builder) unknown_mems -> 'o obj_shape
    (** A basic object, possibly indicating how to handle unknown members *)
  | Obj_cases :
      ('o, 'mems, 'builder) unknown_mems option *
      ('o, 'cases, 'tag) obj_cases -> 'o obj_shape
    (** An object with a case member each case further describing
        an object map. *)
  (** The type for object shapes. *)

  (** {2:unknown_mems Unknown members} *)

  and ('o, 'mems, 'builder) unknown_mems =
  | Unknown_skip : ('o, unit, unit) unknown_mems
    (** Skip unknown members. *)
  | Unknown_error : ('o, unit, unit) unknown_mems
    (** Error on unknown members. *)
  | Unknown_keep :
      ('mems, 'a, 'builder) mems_map * ('o -> 'mems) ->
      ('o, 'mems, 'builder) unknown_mems
    (** Gather unknown members in a member map. *)
  (** The type for specifying decoding behaviour on unknown JSON object
      members. *)

  and ('mems, 'a, 'builder) mems_map =
  { kind : string; (** The kind for unknown members (documentation). *)
    doc : string; (** Documentation string for the unknown members. *)
    mems_type : 'a t; (** The uniform type according which unknown members
                          are typed. *)
    id : 'mems Type.Id.t; (** A type identifier for the unknown member
                              map. *)
    dec_empty : Context.t -> 'builder;
    (** [dec_empty] create a new empty member map builder. *)
    dec_add : Context.t -> Meta.t -> string -> 'a -> 'builder -> 'builder;
    (** [dec_add] adds a member named [n] with metadata [meta] and
        value parsed by [mems_type] to the builder. *)
    dec_finish : Context.t -> 'builder -> 'mems;
    (** [dec_finish] turns the builder into an unknown member map. *)
    enc :
      'acc. Context.t ->
      (Meta.t -> string -> 'a -> 'acc -> 'acc) -> 'mems -> 'acc -> 'acc;
    (** [enc] folds over the member map for encoding. *)
  }
  (** The type for gathering unknown JSON members uniformly typed
      according to ['a] in a map ['mems] constructed with ['builder]. *)

  (** {2:case_objects Case objects} *)

  and ('o, 'cases, 'tag) obj_cases =
  { tag : ('tag, 'tag) mem_map;
    (** The JSON member used to decide cases. The [enc] field of
        this [mem_map] should be the identity, this allows
        encoders to reuse generic encoding code for members.  We
        don't have [('o, 'tag) mem_map] here because the tag is not
        stored we recover the case via [enc] and [enc_case] below. *)
    tag_compare : 'tag -> 'tag -> int;
    (** The function to compare tags. *)
    tag_to_string : ('tag -> string) option;
    (** The function to stringify tags for error reporting. *)
    id : 'cases Type.Id.t;
    (** A type identifier for the tag. *)
    cases : ('cases, 'tag) case list;
    (** The list of possible cases. *)
    enc : 'o -> 'cases;
    (** [enc] is the function to recover case values from the value
        ['o] the object is mapped to. *)
    enc_case : Context.t -> 'cases -> ('cases, 'tag) case_value;
    (** [enc_case] retrieves the concrete case from the common
        [cases] values. You can see it as preforming a match. *)
  }
  (** The type for object cases mapped to a common type ['cases] stored
      in a vlue of type ['o] and identified by tag values of type ['tag]. *)

  and ('cases, 'case, 'tag) case_map =
  { tag : 'tag;
    (** The tag value for the case. *)
    obj_map : ('case, 'case) obj_map;
    (** The object map for the case. *)
    dec : 'case -> 'cases;
    (** [dec] is the function used on decoding to inject the case
        into the common ['cases] type. *)
  }
  (** The type for an object case with common type ['cases] specific
      type ['case] and tag type ['tag]. *)

  and ('cases, 'tag) case_value =
  | Case_value :
      ('cases, 'case, 'tag) case_map * 'case -> ('cases, 'tag) case_value
  (** The type for case values. This packs a case value and its
      description. *)

  and ('cases, 'tag) case =
  | Case : ('cases, 'case, 'tag) case_map -> ('cases, 'tag) case
  (** The type for hiding the the concrete type of a case . *)

  (** {1:any Any maps} *)

  and 'a any_map =
  { kind : string;
    (** The kind of JSON values mapped (documentation). *)
    doc : string;
    (** Documentation string for the kind of values. *)
    dec_null : 'a t option;
    (** [dec_null], if any, is used for decoding JSON nulls. *)
    dec_bool : 'a t option;
    (** [dec_bool], if any, is used for decoding JSON bools. *)
    dec_number : 'a t option;
    (** [dec_number], if any, is used for decoding JSON numbers. *)
    dec_string : 'a t option;
    (** [dec_string], if any, is used for decoding JSON strings. *)
    dec_array : 'a t option;
    (** [dec_array], if any, is used for decoding JSON arrays. *)
    dec_obj : 'a t option;
    (** [dec_obj], if any, is used for decoding JSON objects. *)
    enc : Context.t -> 'a -> 'a t;
    (** [enc] specifies the encoder to use on a given value. *)
  }
  (** The type for mapping JSON values with multiple sorts to a value
      of type ['a]. If a decoding case is [None], the decoding
      errors on these JSON values. *)

  (** {1:type_map Type maps} *)

  and ('a, 'b) map =
  { kind : string;
    (** The kind of JSON values mapped (documentation). *)
    doc : string;
    (** Documentation string for the kind of values. *)
    dom : 'a t;
    (** The domain of the map. *)
    dec : Context.t -> 'a -> 'b;
    (** [dec] decodes ['a] to ['b]. *)
    enc : Context.t -> 'b -> 'a;
    (** [enc] encodes ['b] to ['a]. *) }
  (** The type for mapping JSON types of type ['a] to a JSON type of
      type ['b]. *)

  (** {1:conv Convert} *)

  val of_t : 'a t' -> 'a t
  (** [of_t] is {!Stdlib.Fun.id}. *)

  val unsafe_to_t : 'a t -> 'a t'
  (** [unsafe_to_t r] converts the representation to a type [r].  It
      is unsafe because constructors of JSON types do maintain some
      invariants. *)

  (** {1:errors Errors and messages} *)

  val value_kind : 'a t -> string
  (** [value_kind m] is the kind of definition of [m]. *)

  val sort_kind : kind:string -> sort:string -> string

  val array_kind : kind:string -> 'a t -> string
  (** [array_kind ~kind elt] is an array of kind [kind] for
      an array with element of type [elt]. *)

  val obj_kind : kind:string -> string

  val obj_map_kind : ('o, 'dec) obj_map  -> string
  (** [obj_map_kind m] is the kind of definition of [m]. *)

  val type_error : Context.t -> Meta.t -> 'a t -> fnd:Sort.t -> 'b
  (** [error_kind p m ~exp ~fnd] errors when kind [exp] was expected
      but sort [fnd] was found. See also {!Jsont.Repr.type_error}. *)

  (** [type_error ctx m t] is like {!Error.kind} but [exp] is derived
      from [t]. *)

  val missing_mems_error :
    Context.t -> Meta.t -> ('o, 'o) obj_map -> exp:mem_dec String_map.t ->
    fnd:string list -> 'a
  (** [missing_mems_error] is like {!Error.missing_mems} but with
      information derived from the given argument map descriptions. In
      [exp] only those member map which do not have a default value
      are reported. *)

  val unexpected_mems_error :
    Context.t -> Meta.t -> ('o, 'o) obj_map -> fnd:(string * Meta.t) list -> 'a
  (** [unexpected_mems ctx m ~obj_kind ~exp fnd] errors when member name
      [n] is found in an object but is not expected. TODO what does
      meta represent. *)

  val unexpected_case_tag_error :
    Context.t -> Meta.t -> ('o, 'o) obj_map -> ('o, 'd, 'tag) obj_cases ->
    'tag -> 'a

  (** {1:context Context}

      Prefer these functions to those of {!Context} we might
      extract information from the given object to enrich the context
      at some point. *)

  val push_mem :
    ('o, 'dec) obj_map -> string -> Meta.t -> Context.t -> Context.t

  val push_mem' :
    ('o, 'dec) obj_map -> string * Meta.t -> Context.t -> Context.t

  val push_nth :
    ('array, 'elt, 'builder) array_map -> int -> Meta.t ->
    Context.t -> Context.t

  (** {1:tool Toolbox} *)

  (** Heterogeneous dictionaries. *)
  module Dict : sig
    type binding = B : 'a Type.Id.t * 'a -> binding
    type t
    val empty : t
    val mem : 'a Type.Id.t -> t -> bool
    val add : 'a Type.Id.t -> 'a -> t -> t
    val remove : 'a Type.Id.t -> t -> t
    val find : 'a Type.Id.t -> t -> 'a option
  end

  val apply_dict : ('ret, 'f) dec_fun -> Dict.t -> 'f

  val obj_context_arg : Context.t Type.Id.t
  val obj_meta_arg : Meta.t Type.Id.t

  val pp_code : string fmt
  val pp_kind : string fmt

  type unknown_mems_option =
  | Unknown_mems :
      ('o, 'mems, 'builder) unknown_mems option -> unknown_mems_option

  val override_unknown_mems :
    Context.t -> by:unknown_mems_option -> unknown_mems_option ->
    Dict.t -> unknown_mems_option * Dict.t
end
