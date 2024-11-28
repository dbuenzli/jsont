(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** JSON codec.

    According to {{:https://www.rfc-editor.org/rfc/rfc8259}RFC 8259}.

    See notes about {{!layout}layout preservation} and behaviour
    on {{!duplicate}duplicate members}.

    {b Tip.} For maximal performance decode with [~layout:false] and
    [~locs:false], this is the default. Howver using [~locs:true] improves
    some error reports. *)

open Bytesrw

(** {1:decode Decode} *)

val decode :
  ?layout:bool -> ?locs:bool -> ?file:Jsont.Textloc.fpath -> 'a Jsont.t ->
  Bytes.Reader.t -> ('a, string) result
(** [decode t r] decodes a value from [r] according to [t].
    {ul
    {- If [layout] is [true] whitespace is preserved in {!Jsont.Meta.t}
       values. Defaults to [false].}
    {- If [locs] is [true] locations are preserved in {!Jsont.Meta.t}
       values and error messages are precisely located. Defaults to [false].}
    {- [file] is the file path from which [r] is assumed to read.
       Defaults to {!Jsont.Textloc.file_none}}} *)

val decode' :
  ?layout:bool -> ?locs:bool -> ?file:Jsont.Textloc.fpath -> 'a Jsont.t ->
  Bytes.Reader.t -> ('a, Jsont.Error.t) result
(** [decode'] is like {!val-decode} but preserves the error structure. *)

val decode_string :
  ?layout:bool -> ?locs:bool -> ?file:Jsont.Textloc.fpath -> 'a Jsont.t ->
  string -> ('a, string) result
(** [decode_string] is like {!val-decode} but decodes directly from a string. *)

val decode_string' :
  ?layout:bool -> ?locs:bool -> ?file:Jsont.Textloc.fpath -> 'a Jsont.t ->
  string -> ('a, Jsont.Error.t) result
(** [decode_string'] is like {!val-decode'} but decodes directly from a
    string. *)

(** {1:encode Encode} *)

val encode :
  ?buf:Bytes.t -> ?format:Jsont.format -> ?number_format:Jsont.number_format ->
  'a Jsont.t -> 'a -> eod:bool -> Bytes.Writer.t -> (unit, string) result
(** [encode t v w] encodes value [v] according to [t] on [w].
    {ul
    {- If [buf] is specified it is used as a buffer for the slices written
       on [w]. Defaults to a buffer of length {!Bytes.Slice.io_buffer_size}.}
    {- [format] specifies how the JSON should be formatted.
       Defaults to {!Jsont.Minify}.}
    {- [number_format] specifies the format string to format numbers. Defaults
       to {!Jsont.default_number_format}.}
    {- [eod] indicates whether {!Bytesrw.Bytes.Slice.eod} should
       be written on [w].}} *)

val encode' :
  ?buf:Bytes.t -> ?format:Jsont.format -> ?number_format:Jsont.number_format ->
  'a Jsont.t -> 'a -> eod:bool -> Bytes.Writer.t -> (unit, Jsont.Error.t) result
(** [encode'] is like {!val-encode} but preserves the error structure. *)

val encode_string :
  ?buf:Bytes.t -> ?format:Jsont.format -> ?number_format:Jsont.number_format ->
  'a Jsont.t -> 'a -> (string, string) result
(** [encode_string] is like {!val-encode} but writes to a string. *)

val encode_string' :
  ?buf:Bytes.t -> ?format:Jsont.format -> ?number_format:Jsont.number_format ->
  'a Jsont.t -> 'a -> (string, Jsont.Error.t) result
(** [encode_string'] is like {!val-encode'} but writes to a string. *)

(** {1:recode Recode}

    The defaults in these functions are those of {!val-decode} and
    {!val-encode}, except if [layout] is [true], [format] defaults to
    [Jsont.Layout] and vice-versa.  *)

val recode :
  ?layout:bool -> ?locs:bool -> ?file:Jsont.Textloc.fpath -> ?buf:Bytes.t ->
  ?format:Jsont.format -> ?number_format:Jsont.number_format -> 'a Jsont.t ->
  Bytes.Reader.t -> Bytes.Writer.t -> eod:bool -> (unit, string) result
(** [recode] is {!val-decode} followed by {!val-recode}.  *)

val recode' :
  ?layout:bool -> ?locs:bool -> ?file:Jsont.Textloc.fpath -> ?buf:Bytes.t ->
  ?format:Jsont.format -> ?number_format:Jsont.number_format -> 'a Jsont.t ->
  Bytes.Reader.t -> Bytes.Writer.t -> eod:bool -> (unit, Jsont.Error.t) result
(** [recode'] is like {!val-recode} but preserves the error structure. *)

val recode_string :
  ?layout:bool -> ?locs:bool -> ?file:Jsont.Textloc.fpath -> ?buf:Bytes.t ->
  ?format:Jsont.format -> ?number_format:Jsont.number_format -> 'a Jsont.t ->
  string -> (string, string) result
(** [recode] is {!decode_string} followed by {!recode_string}. *)

val recode_string' :
  ?layout:bool -> ?locs:bool -> ?file:Jsont.Textloc.fpath -> ?buf:Bytes.t ->
  ?format:Jsont.format -> ?number_format:Jsont.number_format -> 'a Jsont.t ->
  string -> (string, Jsont.Error.t) result
(** [recode_string'] is like {!val-recode_string} but preserves the error
    structure. *)

(** {1:layout Layout preservation}

    In order to simplify the implementation not all layout is preserved.
    In particular:
    {ul
    {- White space in empty arrays and objects is dropped.}
    {- Unicode escapes are replaced by their UTF-8 encoding.}
    {- The format of numbers is not preserved.}} *)

(** {1:duplicate Duplicate object members}

    Duplicate object members are undefined behaviour in JSON. We
    follow the behaviour of
    {{:https://262.ecma-international.org/6.0/#sec-internalizejsonproperty}
    [JSON.parse]} and the last one takes over, however duplicate
    members all have to parse with the specified type as we error as soon
    as possible. Also
    {{!Jsont.Object.case_mem}case members} are not allowed to duplicate. *)
