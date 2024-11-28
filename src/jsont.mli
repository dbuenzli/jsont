(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Types for JSON values.

    This module provides a type for describing subsets of JSON values as
    bidirectional maps with arbitrary OCaml values. We call these
    values {e JSON types}.

    In these maps the {e decoding} direction maps from JSON values to
    OCaml values and the {e encoding} direction maps from OCaml values
    to JSON values. Depending on your needs, one direction or the
    other can be left unspecified. Some of the decoding maps may be
    lossy or creative which leads to JSON queries and transforms.

    Read the {{!page-index.quick_start}quick start} and the
    {{!page-cookbook}cookbook}. *)

(** {1:preliminaries Preliminaries} *)

type 'a fmt = Format.formatter -> 'a -> unit
(** The type for formatters of values of type ['a]. *)

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
      {- Indexes the first byte of the next {e newline} sequence if the line
         is empty.}
      {- Is out of bounds and equal to the text's length for a last empty
         line. This is also the case on empty text.}} *)

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

  val set_file : t -> fpath -> t
  (** [set_file l file] is [l] with {!file} set to [file]. *)

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
  (** [pp] is {!pp_ocaml}. *)

  val pp_dump : Format.formatter -> t -> unit
  (** [pp_dump] formats raw data for debugging. *)
end

(** Abstract syntax tree node metadata.

    This type keeps information about source text locations
    and whitespace. *)
module Meta : sig
  type t
  (** The type for node metadata. *)

  val make : ?ws_before:string -> ?ws_after:string -> Textloc.t -> t
  (** [make textloc ~ws_before ~ws_after] is metadata with text location
      [textloc] whitespace [ws_before] before the node and [ws_after] after
      the node. Both default to [""]. *)

  val none : t
  (** [none] is metadata for when there is none. Its {!textloc}
      is {!Textloc.none} and its whitespace is empty. *)

  val is_none : t -> bool
  (** [is_none m] is [true] iff [m] is {!none}. *)

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

  val clear_textloc : t -> t
  (** [clear_textloc m] is [m] with {!textloc} set to {!Textloc.none}. *)

  val copy_ws : t -> dst:t -> t
  (** [copy_ws src ~dst] copies {!ws_before} and {!ws_after} of [src]
      to [dst]. *)
end

type 'a node = 'a * Meta.t
(** The type for abstract syntax tree nodes.
    The node data of type ['a] and its metadata. *)

(** JSON paths.

    Paths are used for keeping track of erroring
    {{!Error.Context.t}contexts} and for specifying {{!Jsont.queries}
    query and update}
    locations. *)
module Path : sig

  (** {1:indices Indices} *)

  type index =
  | Mem of string node (** Indexes the value of the member [n] of an object. *)
  | Nth of int node (** Indexes the value of the [n]th element of an array. *)
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
      {{!Path.path_syntax}path syntax}. *)

  val pp : t fmt
  (** [pp] formats paths. *)

  val pp_trace : t fmt
  (** [pp_trace] formats paths as a stack trace, if not empty. *)

  (** {1:path_syntax Path syntax}

      Path provide a way for end users to address JSON and edit locations.

      A {e path} is a sequence of member and list indexing
      operations. Applying the path to a JSON value leads to either a
      JSON value, or nothing if one of the indices does not exist, or
      an error if ones tries to index a non-indexable value.

      Here are a few examples of paths.

      {@json[
        {
          "ocaml": {
            "libs": ["jsont", "brr", "cmdliner"]
          }
        }
      ]}

      {@shell[
      ocaml.libs        # value of member "libs" of member "ocaml"
      ocaml.libs.[0]    # first element of member "libs" of member "ocaml"
      ]}

      More formally a {e path} is a [.] seperated list of indices. An
      {e index} is written [[i]]. [i] can a zero-based list index. Or
      [i] can be an object member name [n]. If there is no ambiguity,
      the surrounding brackets can be dropped.

      {b Notes.}
      {ul
      {- The syntax has no form of quoting at the moment this
         means key names can't contain, [\[], [\]], or start with a number.}
      {- It would be nice to be able to drop the dots in order
         to be compatible with {{:https://www.rfc-editor.org/rfc/rfc9535}
         JSONPath} syntax.}
      {- Reintroduce and implement negative indices (they are parsed).}} *)
end

(** Sorts of JSON values. *)
module Sort : sig
  type t =
  | Null (** Nulls *)
  | Bool (** Booleans *)
  | Number (** Numbers *)
  | String (** Strings *)
  | Array (** Arrays *)
  | Object (** Objects *)
  (** The type for sorts of JSON values. *)

  val to_string : t -> string
  (** [to_string sort] is a string for sort [sort]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats sorts. *)

  (** {1:kinds Kinds}

      For formatting error messages. *)

  val or_kind : kind:string -> t -> string
  (** [or_kind ~kind sort] is [to_string sort] if [kind] is [""] and
      [kind] otherwise. *)

  val kinded : kind:string -> t -> string
  (** [kinded ~kind sort] is [to_string sort] if [kind] is [""]
      and [String.concat " " [kind; to_string sort]] otherwise. *)

  val kinded' : kind:string -> string -> string
  (** [kinded' ~kind sort] is [sort] if [kind] is [""]
      and [String.concat " " [kind; sort]] otherwise. *)
end

(** Encoding, decoding and query errors. *)
module Error : sig

  (** {1:kinds Kinds of errors} *)

  type kind
  (** The type for kind of errors. *)

  val kind_to_string : kind -> string
  (** [kind_to_string kind] is [kind] as a string. *)

  (** {1:errors Errors} *)

  (** JSON error contexts. *)
  module Context : sig

    type index = string node * Path.index
    (** The type for context indices. The {{!Jsont.kinded_sort}kinded sort} of
        an array or object and its index. *)

    type t = index list
    (** The type for erroring contexts. The first element indexes the
        root JSON value. *)

    val empty : t
    (** [empty] is the empty context. *)

    val is_empty : t -> bool
    (** [is_empty ctx] is [true] iff [ctx] is {!empty}. *)

    val push_array : string node -> int node -> t -> t
    (** [push_array kinded_sort n ctx] wraps [ctx] as the [n]th element of an
        array of {{!Jsont.kinded_sort}kinded sort} [kinded_sort]. *)

    val push_object : string node -> string node -> t -> t
    (** [push_object kinded_sort n ctx] wraps [ctx] as the member named [n] of
        an object of {{!Jsont.kinded_sort}kinded sort} [kinded_sort]. *)
  end

  type t = Context.t * Meta.t * kind
  (** The type for errors. The context, the error localisation and the
      kind of error. *)

  val make_msg : Context.t -> Meta.t -> string -> t
  (** [make_msg ctx meta msg] is an error with message [msg] for meta [meta]
      in context [ctx]. *)

  val msg : Meta.t -> string -> 'a
  (** [msg meta msg] raises an error with message [msg] for meta
      [meta] in an empty context. *)

  val msgf : Meta.t -> ('a, Stdlib.Format.formatter, unit, 'b) format4 -> 'a
  (** [msgf meta fmt …] is like {!val-msg} but formats an error message. *)

  val push_array : string node -> int node -> t -> 'a
  (** [push_array kinded_sort n e] contextualises [e] as an error in the
      [n]th element of an array of {{!Jsont.kinded_sort}kinded sort}
      [kinded_sort]. *)

  val push_object : string node -> string node -> t -> 'a
  (** [push_object kinded_sort n e] contextualises [e] as an error in
      the member [n] of an object of {{!Jsont.kinded_sort}kinded sort}
      [kinded_sort]. *)

  val adjust_context :
    first_byte:Textloc.byte_pos -> first_line:Textloc.line_pos -> t -> 'a
  (** [adjust_context ~first_byte ~first_line] adjusts the error's
      context's meta to encompass the given positions. *)

  (** {1:fmt Formatting} *)

  val to_string : t -> string
  (** [error_to_string e] formats [e] using {!val-pp} to a string. *)

  val pp : t fmt
  (** [pp_error] formats errors. *)

  val puterr : unit fmt
  (** [puterr] formats [Error:] in red. *)

  (**/**)
  val disable_ansi_styler : unit -> unit
  (**/**)
end

exception Error of Error.t
(** The exception raised on map errors. In general codec and query
    functions turn that for you into a {!result} value. *)

(** {1:types Types} *)

type 'a t
(** The type for JSON types.

    A value of this type represents a subset of JSON values mapped to
    a subset of values of type ['a] and vice versa. *)

val kinded_sort : 'a t -> string
(** [kinded_sort t] is a human readable string describing the JSON
    values typed by [t]. This combines the kind of the map with the
    {{!Sort}sort}(s) of JSON value mapped by [t]. For example if [t]
    is an object map and the kind specified for the
    {{!Object.val-map}map} is ["T"] then this is ["T object"], if the
    kind is empty this is simply ["object"]. See also
    {!Sort.kinded}. *)

val kind : 'a t -> string
(** [kind t] is the [kind] of the underlying map. If the kind is an
    empty string this falls back to mention the {{!Sort}sort}. For
    example if [t] is an object map and the kind specified for the
    {{!Object.val-map}map} is ["T"] then this is ["T"], if the kind is
    empty then this is ["object"]. See also {!Sort.or_kind}. *)

val doc : 'a t -> string
(** [doc t] is a documentation string for the JSON values typed by [t]. *)

(** {1:base Base types}

    Read the {{!page-cookbook.base_types}cookbook} on base types. *)

(** Mapping JSON base types. *)
module Base : sig

  (** {1:maps Maps} *)

  type ('a, 'b) map
  (** The type for mapping JSON values of type ['a] to values of type ['b]. *)

  val map :
    ?kind:string -> ?doc:string -> ?dec:(Meta.t -> 'a -> 'b) ->
    ?enc:('b -> 'a) -> ?enc_meta:('b -> Meta.t) ->
    unit -> ('a, 'b) map
  (** [map ~kind ~doc ~dec ~enc ~enc_meta ()] maps JSON base types
      represented by value of type ['a] to values of type ['b] with:
      {ul
      {- [kind] names the entities represented by the map and [doc]
         documents them. Both default to [""].}
      {- [dec] is used to decode values of type ['a] to values of
         type ['b]. Can be omitted if the map is only used for
         encoding, the default unconditionally errors.}
      {- [enc] is used to encode values of type ['b] to values of
         type ['a]. Can be omitted if the map is only used for
         decoding, the default unconditionally errors.}
      {- [enc_meta] is used to recover JSON metadata (source text
         layout information) from a value to encode. The default
         unconditionnaly returns {!Jsont.Meta.none}.}}

      {{!decenc}These functions} can be used to quickly devise
      [dec] and [enc] functions from standard OCaml conversion
      interfaces. *)

  val id : ('a, 'a) map
  (** [id] is the identity map. *)

  val ignore : ('a, unit) map
  (** [ignore] is the ignoring map. It ignores decodes and errors on
      encodes. *)

  (** {2:types JSON types} *)

  val null : (unit, 'a) map -> 'a t
  (** [null map] maps with [map] JSON nulls represented by [()] to
      values of type ['a]. See also {!Jsont.null}. *)

  val bool : (bool, 'a) map -> 'a t
  (** [bool map] maps with [map] JSON booleans represented by [bool]
      values to values of type ['a]. See also {!Jsont.bool}. *)

  val number : (float, 'a) map -> 'a t
  (** [number map] maps with [map] JSON nulls or numbers represented by
      [float] values to values of type ['a]. The [float]
      representation decodes JSON nulls to {!Float.nan} and lossily
      encodes any {{!Float.is_finite}non-finite} to JSON null
      ({{!page-cookbook.non_finite_numbers}explanation}). See also
      {!Jsont.number}. *)

  val string : (string, 'a) map -> 'a t
  (** [string map] maps with [map] {e unescaped} JSON strings represented
      by UTF-8 encoded [string] values to values of type ['a]. See
      also {!Jsont.string}. *)

  (** {1:decenc Decoding and encoding functions}

      These function create suitable [dec] and [enc] functions
      to give to {!val-map} from standard OCaml conversion interfaces.
      See also {!Jsont.of_of_string}. *)

  val dec : ('a -> 'b) -> (Meta.t -> 'a -> 'b)
  (** [dec f] is a decoding function from [f]. This assumes [f] never fails. *)

  val dec_result :
    ?kind:string -> ('a -> ('b, string) result) -> (Meta.t -> 'a -> 'b)
  (** [dec f] is a decoding function from [f]. [Error _] values are given to
      {!Error.msg}, prefixed by [kind:] (if specified). *)

  val dec_failure : ?kind:string -> ('a -> 'b) -> (Meta.t -> 'a -> 'b)
  (** [dec f] is a decoding function from [f]. [Failure _] exceptions
      are catched and given to {!Error.msg}, prefixed by [kind:] (if
      specified). *)

  val enc : ('b -> 'a) -> ('b -> 'a)
  (** [enc f] is an encoding function from [f]. This assumes [f] never fails. *)

  val enc_result : ?kind:string -> ('b -> ('a, string) result) -> ('b -> 'a)
  (** [enc_result f] is an encoding function from [f]. [Error _] values are
      given to {!Error.msg}, prefixed by [kind:] (if specified). *)

  val enc_failure : ?kind:string -> ('b -> 'a) -> ('b -> 'a)
  (** [enc_failure f] is an encoding function from [f]. [Failure _]
      exceptions are catched and given to {!Error.msg}, prefixed by [kind:]
      (if specified). *)
end

(** {2:option Nulls and options}

    Read the {{!page-cookbook.dealing_with_null}cookbook} on [null]s. *)

val null : ?kind:string -> ?doc:string -> 'a -> 'a t
(** [null v] maps JSON nulls to [v]. On encodes any value of type ['a]
    is encoded by null. [doc] and [kind] are given to the underlying
    {!Base.type-map}. See also {!Base.null}. *)

val none : 'a option t
(** [none] maps JSON nulls to [None]. *)

val some : 'a t -> 'a option t
(** [some t] maps JSON like [t] does but wraps results in [Some].
    Encoding fails if the value is [None]. *)

val option : ?kind:string -> ?doc:string -> 'a t -> 'a option t
(** [option t] maps JSON nulls to [None] and other values by [t].
    [doc] and [kind] are given to the underlying {!val-any} map. *)

(** {2:booleans Booleans} *)

val bool : bool t
(** [bool] maps JSON booleans to [bool] values. See also {!Base.bool}. *)

(** {2:numbers Numbers}

    Read the {{!page-cookbook.dealing_with_numbers}cookbook} on JSON
    numbers and their many pitfalls. *)

val number : float t
(** [number] maps JSON nulls or numbers to [float] values.  On decodes
    JSON null is mapped to {!Float.nan}. On encodes any
    {{!Float.is_finite}non-finite} float is lossily mapped to JSON
    null ({{!page-cookbook.non_finite_numbers}explanation}). See also
    {!Base.number}, {!any_float} and the integer combinators below. *)

val any_float : float t
(** [any_float] is a lossless representation for IEEE 754 doubles. It
    maps {{!Float.is_finite}non-finite} floats by the JSON strings
    defined by {!Float.to_string}. This contrasts with {!val-number}
    which maps them to JSON null values
    ({{!page-cookbook.non_finite_numbers}explanation}). Note that on
    decodes this still maps JSON nulls to {!Float.nan} and any
    successful string decode of {!Float.of_string_opt} (so numbers can
    also be written as strings). See also {!val-number}.

    {b Warning.} [any_float] should only be used between parties that
    have agreed on such an encoding. To maximize interoperability you
    should use the lossy {!val-number} map. *)

val float_as_hex_string : float t
(** [float_as_hex_string] maps JSON strings made of IEEE 754 doubles in hex
    notation to float values. On encodes strings this uses the ["%h"]
    format string. On decodes it accepts anything sucessfully decoded
    by {!Float.of_string_opt}. *)

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

val int64_as_string : int64 t
(** [int64_as_string] maps JSON strings to 64-bit integers. On decodes
    this uses {!Int64.of_string_opt} which allows binary, octal,
    decimal and hex syntaxes and errors on overflow and syntax
    errors. On encodes uses {!Int64.to_string}. *)

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
       \[-2{^53};2{^53}\] range}}

    {b Warning.} The behaviour of this function is platform
    dependent, it depends on the value of {!Sys.int_size}. *)

val int_as_string : int t
(** [int_as_string] maps JSON strings to [int] values. On
    decodes this uses {!int_of_string_opt} which allows binary,
    octal, decimal and hex syntaxes and errors on overflow and
    syntax errors. On encodes uses {!Int.to_string}.

    {b Warning.} The behaviour of this function is platform
    dependent, it depends on the value of {!Sys.int_size}. *)

(** {2:enums Strings and enums}

    Read the {{!page-cookbook.transform_strings}cookbook} on
    transforming strings. *)

val string : string t
(** [string] maps unescaped JSON strings to UTF-8 encoded [string]
    values. See also {!Base.string}.

    {b Warning.} Encoders assume OCaml [string]s have been checked for
    UTF-8 validity.  *)

val of_of_string : ?kind:string -> ?doc:string ->
  ?enc:('a -> string) -> (string -> ('a, string) result) -> 'a t
(** [of_of_string of_string] maps JSON string with a
    {{!Base.type-map}base map} using [of_string] for decoding and [enc] for
    encoding. See the {{!page-cookbook.transform_strings}cookbook}. *)

val enum :
  ?cmp:('a -> 'a -> int) -> ?kind:string -> ?doc:string ->
  (string * 'a) list -> 'a t
(** [enum assoc] maps JSON strings member of the [assoc] list to the
    corresponding OCaml value and vice versa in log(n).
    [cmp] is used to compare the OCaml values, it defaults to {!Stdlib.compare}.
    Decoding and encoding errors on strings or values not part of
    [assoc] *)

val binary_string : string t
(** [binary_string] maps JSON strings made of an even number of
    hexdecimal US-ASCII upper or lower case digits to the corresponding
    byte sequence. On encoding uses only lower case hexadecimal
    digits to encode the byte sequence. *)

(** {1:arrays Arrays and tuples}

    Read the {{!page-cookbook.dealing_with_arrays}cookbok} on arrays
    and see also {{!array_queries}array queries and updates}. *)

(** Mapping JSON arrays. *)
module Array : sig

  (** {1:maps Maps} *)

  type ('array, 'elt) enc =
    { enc : 'acc. ('acc -> int -> 'elt -> 'acc) -> 'acc -> 'array -> 'acc }
  (** The type for specifying array encoding functions. A function to fold
      over the elements of type ['elt] of the array of type ['array]. *)

  type ('array, 'elt, 'builder) map
  (** The type for mapping JSON arrays with elements of type ['elt] to arrays
      of type ['array] using values of type ['builder] to build them. *)

  val map :
    ?kind:string -> ?doc:string ->
    dec_empty:(unit -> 'builder) ->
    ?dec_skip:(int -> 'builder -> bool) ->
    dec_add:(int -> 'elt -> 'builder -> 'builder) ->
    dec_finish:(Meta.t -> int -> 'builder -> 'array) ->
    enc:('array, 'elt) enc ->
    ?enc_meta:('array -> Meta.t) -> 'elt t ->
    ('array, 'elt, 'builder) map
  (** [map elt] maps JSON arrays of type ['elt] to arrays of
      type ['array] built with type ['builder].
      {ul
      {- [kind] names the entities represented by the map and [doc]
         documents them. Both default to [""].}
      {- [dec_empty ()] is used to create a builder for the empty array.}
      {- [dec_skip i b] is used to skip the [i]th index of the JSON array.
         If [true], the element is not decoded with [elt] and not added with
         [dec_add] but skipped. The default always returns [false].}
      {- [dec_add i v] is used to add the [i]th JSON element [v] $
         decoded by [elt] to the builder [b].}
      {- [dec_finish b] converts the builder to the final array.}
      {- [enc.enc f acc a] folds over the elements of array [a] in
         increasing order with [f] and starting with [acc]. This function
         is used to encode [a] to a JSON array.}
      {- [enc_meta a] is the metadata to use for encoding [v] to a JSON
         array.}} *)

  val list_map :
    ?kind:string -> ?doc:string ->
    ?dec_skip:(int -> 'a list -> bool) -> 'a t ->
    ('a list, 'a, 'a list) map
  (** [list_map elt] maps JSON arrays with elements of type [elt]
      to [list] values. See also {!Jsont.list}. *)

  type 'a array_builder
  (** The type for array builders. *)

  val array_map :
    ?kind:string -> ?doc:string ->
    ?dec_skip:(int -> 'a array_builder -> bool) -> 'a t ->
    ('a array, 'a, 'a array_builder) map
  (** [array_map elt] maps JSON arrays with elements of type [elt]
      to [array] values. See also {!Jsont.array}. *)

  type ('a, 'b, 'c) bigarray_builder
  (** The type for bigarray_builders. *)

  val bigarray_map :
    ?kind:string -> ?doc:string ->
    ?dec_skip:(int -> ('a, 'b, 'c) bigarray_builder -> bool) ->
    ('a, 'b) Bigarray.kind -> 'c Bigarray.layout -> 'a t ->
    (('a, 'b, 'c) Bigarray.Array1.t, 'a, ('a, 'b, 'c) bigarray_builder) map
  (** [bigarray k l elt] maps JSON arrays with elements of
      type [elt] to bigarray values of kind [k] and layout [l].  See
      also {!Jsont.bigarray}. *)

  (** {1:types JSON types} *)

  val array : ('a, _, _) map -> 'a t
  (** [array map] maps with [map] JSON arrays to values of type ['a].
      See the the {{!section-arrays}array combinators}.  *)

  val ignore : unit t
  (** [ignore] ignores JSON arrays on decoding and errors on encoding. *)

  val zero : unit t
  (** [zero] ignores JSON arrays on decoding and encodes an empty array. *)
end

val list : ?kind:string -> ?doc:string -> 'a t -> 'a list t
(** [list t] maps JSON arrays of type [t] to [list] values. See also
    {!Array.list_map}. *)

val array : ?kind:string -> ?doc:string -> 'a t -> 'a array t
(** [array t] maps JSON arrays of type [t] to [array] values. See
    also {!Array.array_map}. *)

val array_as_string_map :
  ?kind:string -> ?doc:string -> key:('a -> string) -> 'a t ->
  'a Map.Make(String).t t
(** [array_as_string_map ~key t] maps JSON array elements of type [t] to
    string maps by indexing them with [key]. If two elements have
    the same [key] the element with the greatest index takes over.
    Elements of the map are encoded to a JSON array in (binary) key order. *)

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

(** {1:objects Objects}

    Read the {{!page-cookbook.dealing_with_objects}cookbook} on
    objects. See a {{!page-cookbook.objects_as_records}simple
    example}. See also {{!object_queries}object queries and
    updates}. *)

(** Mapping JSON objects.  *)
module Object : sig

  (** {1:maps Maps} *)

  type ('o, 'dec) map
  (** The type for mapping JSON objects to values of type ['o]. The
      ['dec] type is used to construct ['o] from members see {!val-mem}. *)

  val map : ?kind:string -> ?doc:string -> 'dec -> ('o, 'dec) map
  (** [map dec] is an empty JSON object decoded by function [dec].
      {ul
      {- [kind] names the entities represented by the map and [doc]
         documents them. Both default to [""].}
      {- [dec] is a constructor eventually returning a value of
         type ['o] to be saturated with calls to {!val-mem}, {!val-case_mem}
         or {!val-keep_unknown}. This is needed for decoding. Use {!enc_only}
         if the result is only used for encoding.}} *)

  val map' :
    ?kind:string -> ?doc:string -> ?enc_meta:('o -> Meta.t) ->
    (Meta.t -> 'dec) -> ('o, 'dec) map
  (** [map' dec] is like {!val-map} except you get the object's
      decoding metdata in [dec] and [enc_meta] is used to recover it
      on encoding. *)

  val enc_only :
    ?kind:string -> ?doc:string -> ?enc_meta:('o -> Meta.t) -> unit ->
    ('o, 'a) map
  (** [enc_only ()] is like {!val-map'} but can only be used for
      encoding. *)

  val finish : ('o, 'o) map -> 'o t
  (** [finish map] is a JSON type for objects mapped by [map].  Raises
      [Invalid_argument] if [map] describes a member name more than
      once. *)

  (** {1:mems Members} *)

  (** Member maps.

      Usually it's better to use {!Jsont.Object.mem} or {!Jsont.Object.opt_mem}
      directly. But this may be useful in certain abstraction contexts. *)
  module Mem : sig

    type ('o, 'dec) object_map := ('o, 'dec) map

    type ('o, 'a) map
    (** The type for mapping a member object to a value ['a] stored
        in an OCaml value of type ['o]. *)

    val map :
      ?doc:string -> ?dec_absent:'a -> ?enc:('o -> 'a) ->
      ?enc_omit:('a -> bool) -> string -> 'a t -> ('o, 'a) map
    (** See {!Jsont.Object.mem}. *)

    val app : ('o, 'a -> 'b) object_map -> ('o, 'a) map -> ('o, 'b) object_map
    (** [app map mmap] applies the member map [mmap] to the contructor of
        the object map [map]. In turn this adds the [mmap] member definition
        to the object described by [map]. *)
  end

  val mem :
    ?doc:string -> ?dec_absent:'a -> ?enc:('o -> 'a) ->
    ?enc_omit:('a -> bool) -> string -> 'a t -> ('o, 'a -> 'b) map ->
    ('o, 'b) map
  (** [mem name t map] is a member named [name] of type
      [t] for an object of type ['o] being constructed by [map].
      {ul
      {- [doc] is a documentation string for the member. Defaults to [""].}
      {- [dec_absent], if specified, is the value used for the decoding
         direction when the member named [name] is missing. If unspecified
         decoding errors when the member is absent. See also {!opt_mem}
         and {{!page-cookbook.optional_members}this example}.}
      {- [enc] is used to project the member's value from the object
         representation ['o] for encoding to JSON with [t]. It can be omitted
         if the result is only used for decoding.}
      {- [enc_omit] is for the encoding direction. If the member value returned
         by [enc] returns [true] on [enc_omit], the member is omited in the
         encoded JSON object. Defaults to [Fun.const false].
         See also {!opt_mem} and
         {{!page-cookbook.optional_members}this example}.}} *)

  val opt_mem :
    ?doc:string -> ?enc:('o -> 'a option) -> string -> 'a t ->
    ('o, 'a option -> 'b) map -> ('o, 'b) map
  (** [opt_mem name t map] is:
  {[
    let dec_absent = None and enc_omit = Option.is_none in
    Jsont.Object.mem name (Jsont.some t) map ~dec_absent ~enc_omit
  ]}
      A shortcut to represent optional members of type ['a] with ['a option]
      values. *)

  (** {1:cases Case objects}

      Read the {{!page-cookbook.cases}cookbook} on case objects. *)

  (** Case objects.

      Case objects are used to describe objects whose members depend
      on the tag value of a distinguished case member. See an
      {{!page-cookbook.cases}example}. *)
  module Case : sig

    (** {1:maps Maps} *)

    type 'a jsont := 'a t

    type ('cases, 'case, 'tag) map
    (** The type for mapping a case object represented by ['case] belonging to
        a common type represented by ['cases] depending on the value
        of a case member of type ['tag]. *)

    val map :
      ?dec:('case -> 'cases) -> 'tag -> 'case jsont ->
      ('cases, 'case, 'tag) map
    (** [map ~dec v obj] defines the object map [obj] as being the
        case for the tag value [v] of the case member. [dec] indicates how to
        inject the object case into the type common to all cases.

        Raises [Invalid_argument] if [obj] is not a direct result of
        {!finish}, that is if [obj] does not describe an object. *)

    (** {1:cases Cases} *)

    type ('cases, 'tag) t
    (** The type for a case of the type ['cases]. This is
        {!type-map} with its ['case] representation hidden. *)

    val make : ('cases, 'case, 'tag) map -> ('cases, 'tag) t
    (** [make map] is [map] as a case. *)

    (** {1:case Case values} *)

    type ('cases, 'tag) value
    (** The type for case values. This holds a case value and
        its case map {!type-map}. Use {!val-value} to construct them. *)

    val value : ('cases, 'case, 'tag) map -> 'case -> ('cases, 'tag) value
    (** [value map v] is a case value [v] described by [map]. *)
  end

  val case_mem :
    ?doc:string -> ?tag_compare:('tag -> 'tag -> int) ->
    ?tag_to_string:('tag -> string) -> ?dec_absent:'tag ->
    ?enc:('o -> 'cases) -> ?enc_omit:('tag -> bool) ->
    ?enc_case:('cases -> ('cases, 'tag) Case.value) -> string -> 'tag t ->
    ('cases, 'tag) Case.t list -> ('o, 'cases -> 'a) map -> ('o, 'a) map
  (** [case_mem name t cases map] is mostly like {!val-mem} except the member
      [name] selects an object representation according to the member value of
      type [t]:
      {ul
      {- [doc] is a documentation string for the member. Defaults to [""].}
      {- [tag_compare] is used to compare tags. Defaults to {!Stdlib.compare}}
      {- [tag_to_string] is used to stringify tags for improving
         error reporting.}
      {- [dec_absent], if specified, is the case value used for the decoding
         direction when the case member named [name] is missing. If unspecified
         decoding errors when the member is absent.}
      {- [enc] is used to project the value in which cases are stored
         from the object representation ['o] for encoding to JSON. It
         can be omitted if the result is only used for decoding.}
      {- [enc_case] determines the actual case value from the value returned
         by [enc].}
      {- [enc_omit] is used on the tag of the case returned by [enc_case]
         to determine if the case member can be ommited in the encoded JSON
         object}
      {- [cases] enumerates all the cases, it is needed for decoding.}}

      The names of the members of each case must be disjoint from [name]
      or those of [map] otherwise [Invalid_argument] is raised on
      {!finish}. Raises [Invalid_argument] if [case_mem] was already called
      on map. *)

  (** {1:unknown_members Unknown members}

      Read the {{!page-cookbook.unknown_members}cookbook} on unknown object
      members.

      On {{!cases}case objects} each individual case has its own
      behaviour unless the combinators are used on the case object map
      in which case it overrides the behaviour of cases. For those
      cases that use {!keep_unknown} they will get the result of an
      empty builder in their decoding function and the encoder is
      ignored on encode. *)

  (** Uniform members. *)
  module Mems : sig

    (** {1:maps Maps} *)

    type 'a jsont := 'a t

    type ('mems, 'a) enc =
      { enc :
          'acc. (Meta.t -> string -> 'a -> 'acc -> 'acc) ->
          'mems -> 'acc -> 'acc }
    (** The type for specifying unknown members encoding function.
        A function to fold over unknown members of uniform type ['a]
        stored in a value of type ['mems]. *)

    type ('mems, 'a, 'builder) map
    (** The type for mapping members of uniform type ['a] to values of
        type ['mems] using a builder of type ['builder]. *)

    val map :
      ?kind:string -> ?doc:string -> 'a jsont ->
      dec_empty:(unit -> 'builder) ->
      dec_add:(Meta.t -> string -> 'a -> 'builder -> 'builder) ->
      dec_finish:(Meta.t -> 'builder -> 'mems) ->
      enc:('mems, 'a) enc -> ('mems, 'a, 'builder) map
    (** [map type'] maps unknown members of uniform type ['a]
        to values of type ['mems] built with type ['builder].
        {ul
        {- [kind] names the entities represented by the map and [doc]
           documents them. Both default to [""].}
        {- [dec_empty] is used to create a builder for the members}
        {- [dec_add meta name v b] is used to add a member named [name]
           with meta [meta] with member value [v] to builder [b]}
        {- [dec_finish meta b]  converts the builder to the final members
           value. [meta] is the metadata of the object in which they were
           found.}
        {- [enc f mems acc] folds over the elements of [mems] starting
           with [acc]. This function is used to encode the members}}
        See {!keep_unknown}. *)

    val string_map :
      ?kind:string -> ?doc:string -> 'a jsont ->
      ('a Stdlib.Map.Make(String).t, 'a, 'a Stdlib.Map.Make(String).t) map
      (** [string_map t] collects unknown member by name and types their
          values with [t]. See {!keep_unknown} and {!as_string_map}. *)
  end

  val skip_unknown : ('o, 'dec) map -> ('o, 'dec) map
  (** [skip_unknown map] makes [map] skip unknown members. This is the
      default, no need to specify it. Raises [Invalid_argument] if
      {!keep_unknown} was already specified on [map]. *)

  val error_unknown : ('o, 'dec) map -> ('o, 'dec) map
  (** [error_unknown map] makes [map] error on unknown members. Raises
      [Invalid_argument] if {!keep_unknown} was already specified on
      [map]. See {{!page-cookbook.erroring}this example}. *)

  val keep_unknown :
    ?enc:('o -> 'mems) -> ('mems, _, _) Mems.map ->
    ('o, 'mems -> 'a) map -> ('o, 'a) map
  (** [keep_unknown mems map] makes [map] keep unknown member with [mems].
      Raises [Invalid_argument] if {!keep_unknown} was already
      specified on [map]. See this {{!page-cookbook.keeping}this
      example}, {!Mems.string_map} and {!Jsont.json_mems}. *)

  (** {1:types JSON types } *)

  val as_string_map :
    ?kind:string -> ?doc:string -> 'a t -> 'a Stdlib.Map.Make(String).t t
  (** [as_string_map t] maps object to key-value maps of type [t].
      See also {!Mems.string_map} and {!Jsont.json_mems}. *)

  val zero : unit t
  (** [zero] ignores JSON objects on decoding and encodes an empty object. *)
end

(** {1:any Any} *)

val any :
  ?kind:string -> ?doc:string -> ?dec_null:'a t -> ?dec_bool:'a t ->
  ?dec_number:'a t -> ?dec_string:'a t -> ?dec_array:'a t ->
  ?dec_object:'a t -> ?enc:('a -> 'a t) -> unit -> 'a t
(** [any ()] maps subsets of JSON value of different sorts to values
    of type ['a]. The unspecified cases are not part of the subset and
    error on decoding. [enc] selects the type to use on encoding and errors
    if omitted. [kind] names the entities represented by the type and [doc]
    documents them, both defaults to [""]. *)

(** {1:maps Maps & recursion} *)

val map :
  ?kind:string -> ?doc:string -> ?dec:('a -> 'b) ->
  ?enc:('b -> 'a) -> 'a t -> 'b t
(** [map t] changes the type of [t] from ['a] to ['b].
    {ul
    {- [kind] names the entities represented by the type and [doc]
       documents them, both default to [""].}
    {- [dec] decodes values of type ['a] to values of type ['b].
       Can be omitted if the result is only used for
       encoding. The default errors.}
    {- [enc] encodes values of type ['b] to values of type ['a].
       Can be omitted if the result is only used for
       decoding. The default errors.}} *)

val iter :
  ?kind:string -> ?doc:string -> ?dec:('a -> unit) -> ?enc:('a -> unit) ->
  'a t -> 'a t
(** [iter ?enc dec t] applies [dec] on decoding and [enc] on encoding
    but otherwise behaves like [t] does. Typically [dec] can be used
    to further assert the shape of the decoded value and {!Error.msgf}
    if it hasn't the right shape. [iter] can also be used as a tracing
    facility for debugging. *)

val rec' : 'a t Lazy.t -> 'a t
(** [rec'] maps recursive JSON values. See the {{!page-cookbook.recursion}
    cookbook}. *)

(** {1:ignoring Ignoring} *)

val ignore : unit t
(** [ignore] lossily maps all JSON values to [()] on decoding and
    errors on encoding. See also {!const}. *)

val zero : unit t
(** [zero] lossily maps all JSON values to [()] on decoding and
    encodes JSON nulls. *)

val todo : ?kind:string -> ?doc:string -> ?dec_stub:'a -> unit -> 'a t
(** [todo ?dec_stub ()]  maps all JSON values to [dec_stub] if
    specified (errors otherwise) and errors on encoding. *)

(** {1:generic_json Generic JSON} *)

type name = string node
(** The type for JSON member names. *)

type mem = name * json
(** The type for generic JSON object members. *)

and object' = mem list
(** The type for generic JSON objects. *)

and json =
| Null of unit node
| Bool of bool node
| Number of float node
(** Encoders must use [Null] if float is {{!Float.is_finite}not finite}. *)
| String of string node
| Array of json list node
| Object of object' node (** *)
(** The type for generic JSON values. *)

(** Generic JSON values. *)
module Json : sig

  (** {1:json JSON values} *)

  type 'a jsont := 'a t

  type 'a cons = ?meta:Meta.t -> 'a -> json
  (** The type for constructing JSON values from an OCaml value of type ['a].
      [meta] defaults to {!Meta.none}. *)

  type t = json
  (** See {!Jsont.val-json}. *)

  val meta : json -> Meta.t
  (** [meta v] is the metadata of value [v]. *)

  val set_meta : Meta.t -> json -> json
  (** [set_meta m v] replaces [v]'s meta with [m]. *)

  val copy_layout : json -> dst:json -> json
  (** [copy_layout src ~dst] copies the layout of [src] and sets
      it on [dst] using {!Meta.copy_ws}. *)

  val sort : json -> Sort.t
  (** [sort v] is the sort of value [v]. *)

  val zero : json cons
  (** [zero j] is a stub value of the sort value of [j]. The stub
      value is the “natural” zero: null, false, 0, empty string,
      empty array, empty object. *)

  val equal : json -> json -> bool
  (** [equal j0 j1] is {!compare}[ j0 j1 = 0]. *)

  val compare : json -> json -> int
  (** [compare j0 j1] is a total order on JSON values:
      {ul
      {- Floating point values are compared with {!Float.compare},
         this means NaN values are equal.}
      {- Strings are compared byte wise.}
      {- Objects members are sorted before being compared.}
      {- {!Meta.t} values are ignored.}} *)

  val pp : t fmt
  (** See {!Jsont.pp_json}. *)

  (** {2:null Nulls and options} *)

  val null : unit cons
  (** [null] is [Null (unit, meta)]. *)

  val option : 'a cons -> 'a option cons
  (** [option c] constructs [Some v] values with [c v] and [None] ones
      with {!val-null}. *)

  (** {2:bool Booleans} *)

  val bool : bool cons
  (** [bool b] is [Bool (b, meta)]. *)

  (** {2:numbers Numbers} *)

  val number : float cons
  (** [number n] is [Number (n, meta)]. *)

  val any_float : float cons
  (** [any_float v] is [number v] if {!Float.is_finite}[ v] is [true]
      and [string (Float.to_string v)] otherwise. See {!Jsont.any_float}. *)

  val int32 : int32 cons
  (** [int32] is [i] as a JSON number. *)

  val int64 : int64 cons
  (** [int64 i] is [i] as a JSON number or a JSON string if
      not in the range \[-2{^53};2{^53}\]. See also {!int64_as_string}. *)

  val int64_as_string : int64 cons
  (** [int64_as_string i] is [i] as a JSON string. See also {!int64}. *)

  val int : int cons
  (** [int] is [i] as a JSON number or a JSON string if not
      in the range \[-2{^53};2{^53}\]. See also {!int_as_string}. *)

  val int_as_string : int cons
  (** [int_as_string i] is [i] as a JSON string. See also {!int}. *)

  (** {2:strings Strings} *)

  val string : string cons
  (** [string s] is [String (s, meta)]. *)

  (** {2:arrays Arrays} *)

  val list : json list cons
  (** [list l] is [Array (l, meta)]. *)

  val array : json array cons
  (** [array l] is [Array (Array.to_list a, meta)]. See also {!list}. *)

  (** {2:objects Objects} *)

  val name : ?meta:Meta.t -> string -> name
  (** [name ?meta n] is [(n, meta)]. [meta] defaults to {!Meta.none}. *)

  val mem : name -> json -> mem
  (** [mem n v] is [(n, v)]. [meta] defaults to {!Meta.none}. *)

  val object' : object' cons
  (** [object o] is [Object (o, meta)]. *)

  val find_mem : string -> object' -> mem option
  (** [find_mem n ms] find the first member whose name matches [n] in [ms]. *)

  val find_mem' : name -> object' -> mem option
  (** [find_mem n ms] is [find_mem (fst n) ms]. *)

  val object_names : object' -> string list
  (** [object_names ms] are the names of [ms]. *)

  val object_names' : object' -> name list
  (** [object_names ms] are the names of [ms]. *)

  (** {1:decode Decode} *)

  val decode : 'a jsont -> json -> ('a, string) result
  (** [decode t j] decodes a value from the generic JSON [j] according
      to type [t]. *)

  val decode' : 'a jsont -> json -> ('a, Error.t) result
  (** [decode'] is like {!val-decode} but preserves the error structure. *)

  (** {1:encode Encode} *)

  val encode : 'a jsont -> 'a -> (json, string) result
  (** [encode t v] encodes a generic JSON value for [v] according
      to type [t]. *)

  val encode' : 'a jsont -> 'a -> (json, Error.t) result
  (** [encode'] is like {!val-encode} but preserves the error structure. *)

  (** {1:recode Recode} *)

  val recode : 'a jsont -> json -> (json, string) result
  (** [recode t v] decodes [v] with [t] and encodes it with [t]. *)

  val recode' : 'a jsont -> json -> (json, Error.t) result
  (** [recode'] is like {!val-recode} but preserves the error structure. *)

  val update : 'a jsont -> json -> json
  (** [update] is like {!val-recode} but raises {!Jsont.exception-Error}. *)

  (** {1:errors Errors} *)

  val error_sort : exp:Sort.t -> json -> 'a
  (** [error_sort ~exp fnd] errors when sort [exp] was expected but
      generic JSON [fnd] was found. *)

  val error_type : 'a jsont -> json -> 'a
  (** [error_type t fnd] errors when the type expected by [t]
      does not match [fnd]. *)
end

val json : json t
(** [json] maps any JSON value to its generic representation. *)

val json_null : json t
(** [json_null] maps JSON nulls to their generic representation. *)

val json_bool : json t
(** [json_bool] maps JSON booleans to their generic representation. *)

val json_number : json t
(** [json_number] maps JSON nulls or numbers
    ({{!page-cookbook.non_finite_numbers}explanation}) to their generic
    representation. *)

val json_string : json t
(** [json_string] represents JSON strings by their generic representation. *)

val json_array : json t
(** [json_array] represents JSON arrays by their generic representation. *)

val json_object : json t
(** [json_object] represents JSON objects by their generic representation. *)

val json_mems : (json, json, mem list) Object.Mems.map
(** [json_mems] is a members map collecting unknown members into a
    generic JSON object. See {{!page-cookbook.keeping}this example}. *)

(** {1:queries Queries and updates}

    Queries are lossy or aggregating decodes. Updates decode to
    {!type-json} values but transform the data along the way. They allow to
    process JSON data without having to fully model it
    (see the update example in the {{!page-index.quick_start}quick start}). *)

val const : 'a t -> 'a -> 'a t
(** [const t v] maps any JSON value to [v] on decodes and
    unconditionally encodes [v] with [t]. *)

val recode : dec:'a t -> ('a -> 'b) -> enc:'b t -> 'b t
(** [recode ~dec f ~enc] maps on decodes like [dec] does followed by
    [f] and on encodes uses [enc]. This can be used to change the JSON
    sort of value. For example:
{[
recode ~dec:int (fun _ i -> string_of_int s) ~enc:string
]}
    decodes an integer but encodes the integer as a string. *)

val update : 'a t -> json t
(** [update t] decodes any JSON with [t] and directly encodes it back
    with [t] to yield the decode result. Encodes any JSON like {!val-json}
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
    indices as needed. [stub] defaults to {!Json.zero} applied to the
    value [v] encoded by [t] (i.e. the "natural zero" of [v]'s encoding sort).
    Encodes like {!json_array} does. *)

val update_nth : ?stub:json -> ?absent:'a -> int -> 'a t -> json t
(** [update_nth n t] on decode recodes the [n]th value of a JSON array
    with [t]. Errors if there is no such index unless [absent] is
    specified in which case the index is created with [absent],
    encoded with [t] and preceeded by as many [stub] values as
    needed. [stub] defaults to {!Json.zero} applied to the recode.
    Encodes like {!json_array} does. *)

val delete_nth : ?allow_absent:bool -> int -> json t
(** [delete_nth n] drops the [n]th index of a JSON array on both
    decode and encodes. Other indices are left untouched.  Errors if
    there is no such index unless [~allow_absent:true] is specified in
    which case the data is left untouched. *)

val filter_map_array : 'a t -> 'b t -> (int -> 'a -> 'b option) -> json t
(** [filter_map_array a b f] maps the [a] elements of a JSON array
    with [f] to [b] elements or deletes them on [None]. Encodes
    generic JSON arrays like {!json_array} does. *)

val fold_array : 'a t -> (int -> 'a -> 'b -> 'b) -> 'b -> 'b t
(** [fold_array t f acc] fold [f] over the [t] elements of a JSON
    array starting with [acc]. Encodes an empty JSON array. *)

(** {2:object_queries Objects} *)

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

val update_mem : ?absent:'a -> string -> 'a t -> json t
(** [update_mem name t] recodes the member value of [name] of a JSON
    object with [t]. This happens both on decodes and encodes.  Errors
    if there is no such member unless [absent] is specified in which
    case a member with this value encoded with [t] is added to the
    object. *)

val delete_mem : ?allow_absent:bool -> string -> json t
(** [delete_mem name] deletes the member named [name] of a JSON object
    on decode. Other members are left untouched. The decode errors if
    there is no such member unless [~allow_absent:true] is specified
    in which case the data is left untouched. Encodes generic JSON
    objects like {!json_object} does. *)

val filter_map_object :
  'a t -> 'b t -> (Meta.t -> string -> 'a -> (name * 'b) option) -> json t
(** [filter_map_object a b f] maps the [a] members of a JSON object
    with [f] to [(n, b)] members or deletes them on [None]. The meta
    given to [f] is the meta of the member name. Encodes generic JSON
    arrays like {!json_object} does. *)

val fold_object : 'a t -> (Meta.t -> string -> 'a -> 'b -> 'b) -> 'b -> 'b t
(** [fold_object t f acc] folds [f] over the [t] members of a JSON object
    starting with [acc]. Encodes an empty JSON object. *)

(** {2:index_queries Indices} *)

val index : ?absent:'a -> Path.index -> 'a t -> 'a t
(** [index] uses {!val-nth} or {!val-mem} on the given index. *)

val set_index : ?allow_absent:bool -> 'a t -> Path.index -> 'a -> json t
(** [set_index] uses {!set_nth} or {!set_mem} on the given index. *)

val update_index : ?stub:json -> ?absent:'a -> Path.index -> 'a t -> json t
(** [update_index] uses {!update_nth} or {!update_mem} on the given index. *)

val delete_index : ?allow_absent:bool ->  Path.index -> json t
(** [delete_index] uses {!delete_nth} or {!delete_mem} on the given index. *)

(** {2:path_queries Paths} *)

val path : ?absent:'a -> Path.t -> 'a t -> 'a t
(** [path p t] {{!index}decodes} with [t] on the last index of [p]. If
    [p] is {!Path.root} this is [t]. *)

val set_path :
  ?stub:json -> ?allow_absent:bool -> 'a t -> Path.t -> 'a -> json t
(** [set_path t p v] {{!set_index}sets} the last index of [p]. If [p]
    is {!Path.root} this encodes [v] with [t]. *)

val update_path : ?stub:json -> ?absent:'a -> Path.t -> 'a t -> json t
(** [update_path p t] {{!update_index}updates} the last index of [p] with
    [t]. On the root path this is [t]. *)

val delete_path : ?allow_absent:bool -> Path.t -> json t
(** [delete_path p] {{!delete_index}deletes} the last index of [p]. If
    [p] is {!Path.root} this is {!Json.val-null}. *)

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
       ({{!page-cookbook.non_finite_numbers}explanation}).}
    {- Strings are assumed to be valid UTF-8.}} *)

val pp_value : ?number_format:number_format -> 'a t -> unit -> 'a fmt
(** [pp_value t ()] formats the JSON representation of values as
    described by [t] by encoding it with {!Json.val-encode} and formatting
    it with {!pp_json'}. If the encoding of the value errors a JSON
    string with the error message is formatted. This means that {!pp_value}
    should always format valid JSON text. *)

(** {1:low Low-level representation} *)

(** Low level representation (unstable).

    This representation may change even between minor versions of the
    library. It can be used to devise new processors on JSON types.

    Processors should be ready to catch the {!Jsont.exception-Error} exception
    when they invoke functional members of the representation.

    Processors should make sure they interpret mappings
    correctly. In particular:
    {ul
    {- The [Number] case represents the sets of JSON numbers and nulls.}}

    See the source of {!Json.decode'} and {!Json.encode'}
    for a simple example on how to process this representation. The
    {{:https://erratique.ch/repos/jsont/tree/paper}paper}
    in the Jsont source repository may also help to understand this menagerie
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
    dec : Meta.t -> 'a -> 'b;
    (** [dec] decodes a base value represented by its metadata and ['a] to
        ['b]. *)
    enc : 'b -> 'a;
    (** [enc] encodes a value of type ['b] to a base JSON value represented
        by ['a]. *)
    enc_meta : 'b -> Meta.t;
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
  | Object : ('o, 'o) object_map -> 'o t (** Object maps. *)
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
    dec_empty : unit -> 'builder;
    (** [dec_empty ()] creates a new empty array builder. *)
    dec_skip : int -> 'builder -> bool;
    (** [dec_skip i b] determines if the [i]th index of the JSON array can be
        skipped. *)
    dec_add : int -> 'elt -> 'builder -> 'builder;
    (** [dec_add] adds the [i]th index value of the JSON array
        as decoded by [elt] to the builder. *)
    dec_finish : Meta.t -> int -> 'builder -> 'array;
    (** [dec_finish] turns the builder into an array given its
        metadata and length. *)
    enc : 'acc. ('acc -> int -> 'elt -> 'acc) -> 'acc -> 'array -> 'acc;
    (** [enc] folds over the elements of the array for encoding. *)
    enc_meta : 'array -> Meta.t;
    (** [enc_meta] recovers the metadata of an array (if any). *) }
  (** The type for mapping JSON arrays to values of type ['array]
      with array elements mapped to type ['elt] and using a ['builder]
      value to construct the array. *)

  (** {1:object_map Object maps} *)

  and ('o, 'dec) object_map =
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
    enc_meta : 'o -> Meta.t;
    (** [enc_meta] recovers the metadata of an object (if any). *)
    shape : 'o object_shape;
    (** [shape] is the {{!object_shape}shape} of the object. *) }
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
    enc : 'o -> 'a;
    (** [enc] recovers the value to encode from ['o]. *)
    (*    enc_name_meta : 'a -> Meta.t;
          XXX This should have been the meta found for the name, but
          that does not fit so well in the member combinators, it's
          not impossible to fit it in but likely increases the cost
          for decoding objects. The layout preserving updates occur
          via generic JSON which uses [mems_map] in which the meta
          is available in [dec_add]. Let's leave it that way for now. *)
    enc_omit : 'a -> bool;
    (** [enc_omit] is [true] if the result of [enc] should
        not be encoded. *)
  }
  (** The type for mapping a JSON member to a value of type ['a] in
      an object represented by a value of type ['o]. *)

  and 'o object_shape =
  | Object_basic : ('o, 'mems, 'builder) unknown_mems -> 'o object_shape
    (** A basic object, possibly indicating how to handle unknown members *)
  | Object_cases :
      ('o, 'mems, 'builder) unknown_mems option *
      ('o, 'cases, 'tag) object_cases -> 'o object_shape
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
    dec_empty : unit -> 'builder;
    (** [dec_empty] create a new empty member map builder. *)
    dec_add : Meta.t -> string -> 'a -> 'builder -> 'builder;
    (** [dec_add] adds a member named [n] with metadata [meta] and
        value parsed by [mems_type] to the builder. *)
    dec_finish : Meta.t -> 'builder -> 'mems;
    (** [dec_finish] turns the builder into an unknown member map.
        The [meta] is the meta data of the object in which they were found. *)
    enc :
      'acc. (Meta.t -> string -> 'a -> 'acc -> 'acc) -> 'mems -> 'acc -> 'acc;
    (** [enc] folds over the member map for encoding. *)
  }
  (** The type for gathering unknown JSON members uniformly typed
      according to ['a] in a map ['mems] constructed with ['builder]. *)

  (** {2:case_objects Case objects} *)

  and ('o, 'cases, 'tag) object_cases =
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
    enc_case : 'cases -> ('cases, 'tag) case_value;
    (** [enc_case] retrieves the concrete case from the common
        [cases] values. You can see it as preforming a match. *)
  }
  (** The type for object cases mapped to a common type ['cases] stored
      in a vlue of type ['o] and identified by tag values of type ['tag]. *)

  and ('cases, 'case, 'tag) case_map =
  { tag : 'tag;
    (** The tag value for the case. *)
    object_map : ('case, 'case) object_map;
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
    dec_object : 'a t option;
    (** [dec_object], if any, is used for decoding JSON objects. *)
    enc : 'a -> 'a t;
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
    dec : 'a -> 'b;
    (** [dec] decodes ['a] to ['b]. *)
    enc : 'b -> 'a;
    (** [enc] encodes ['b] to ['a]. *) }
  (** The type for mapping JSON types of type ['a] to a JSON type of
      type ['b]. *)

  (** {1:conv Convert} *)

  val of_t : 'a t' -> 'a t
  (** [of_t] is {!Stdlib.Fun.id}. *)

  val unsafe_to_t : 'a t -> 'a t'
  (** [unsafe_to_t r] converts the representation to a type [r].  It
      is unsafe because constructors of the {!Jsont} module do
      maintain some invariants. *)

  (** {1:kinds Kinds and doc} *)

  val kinded_sort : 'a t -> string
  (** [kinded_sort t] is kinded sort of [t], see {!Jsont.kinded_sort}. *)

  val array_map_kinded_sort : ('a, 'elt, 'builder) array_map  -> string
  (** [array_map_kinded_sort map] is like {!kinded_sort} but
      acts directly on the array [map]. *)

  val object_map_kinded_sort : ('o, 'dec) object_map  -> string
  (** [object_map_kind map] is like {!kinded_sort} but acts directly
      on the object [map]. *)

  val pp_kind : string fmt
  (** [pp_kind] formats kinds. *)

  val doc : 'a t -> string
  (** See {!Jsont.doc}. *)

  (** {1:errors Errors} *)

  val error_push_array :
    Meta.t -> ('array, 'elt, 'builder) array_map -> int node -> Error.t -> 'a
  (** [error_push_array] is like {!Error.push_array} but uses the
      given array [meta] and array map to caracterize the context.  *)

  val error_push_object :
    Meta.t -> ('o, 'dec) object_map -> string node -> Error.t -> 'a
  (** [error_push_object] is like {!Error.push_object} but uses the
      given object [meta] and object map to caracterize the context. *)

  val type_error : Meta.t -> 'a t -> fnd:Sort.t -> 'b
  (** [type_error meta ~exp ~fnd] errors when kind [exp] was expected
      but sort [fnd] was found. *)

  val missing_mems_error :
    Meta.t -> ('o, 'o) object_map -> exp:mem_dec String_map.t ->
    fnd:string list -> 'a
  (** [missing_mems_error m map exp fnd] errors when [exp] cannot
      be found, [fnd] can list a few members that were found. *)

  val unexpected_mems_error :
    Meta.t -> ('o, 'o) object_map -> fnd:(string * Meta.t) list -> 'a
  (** [unexpected_mems_error meta map ~fnd] errors when [fnd] are
      unexpected members for object [map]. *)

  val unexpected_case_tag_error :
    Meta.t -> ('o, 'o) object_map -> ('o, 'd, 'tag) object_cases ->
    'tag -> 'a
  (** [unexpected_case_tag_error meta map cases tag] is when a [tag]
      of a case member has no corresponding case. *)

  (** {1:toolbox Processor toolbox} *)

  val object_meta_arg : Meta.t Type.Id.t
  (** [object_meta_arg] holds the {!Jsont.Object.mem} to *)

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
  (** [apply_dict dec dict] applies [dict] to [f] in order to get the
      value ['f]. Raises [Invalid_argument] if [dict] has not all the
      type identifiers that [dec] needs. *)

  type unknown_mems_option =
  | Unknown_mems :
      ('o, 'mems, 'builder) unknown_mems option -> unknown_mems_option
  (** A type for hiding an optional {!type-unknown_mems} values. *)

  val override_unknown_mems :
    by:unknown_mems_option -> unknown_mems_option ->
    Dict.t -> unknown_mems_option * Dict.t
  (** [override_unknown_mems ~by current dict] preforms the unknown member
      overriding logic for {!Jsont.Object.Case} objects. In particular if
      [current] is a {!Jsont.Object.Mems.val-map} it adds an empty one in [dict]
      so that the associated decoding function does not fail. *)

  val finish_object_decode :
    ('o, 'o) object_map -> Meta.t -> ('p, 'mems, 'builder) unknown_mems ->
    'builder -> mem_dec String_map.t -> Dict.t -> Dict.t
  (** [finish_object_decode map meta unknown_mems umap rem_mems dict] finishes
      an object map [map] decode. It adds the [umap] (if needed) to [dict],
      it adds [meta] to [dict] under {!object_meta_arg} and tries to find
      andd default values to [dict] for [rem_mems] (and errors if it can't). *)

  val pp_code : string fmt
  (** [pp_code] formats strings like code (in bold). *)
end
