(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** JavaScript support.

    {b Note.} These functions incur a bit of overhead but should work
    fast enough for medium sized structures. Get in touch if you run
    into problems, some improvements may be possible.

    The JSON functions use JavaScript's
    {{:https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Global_Objects/JSON/parse}[JSON.parse]} and
    {{:https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Global_Objects/JSON/stringify}[JSON.stringify]} to convert to JavaScript values
    which are then converted with {!decode_jv} and {!encode_jv}. *)

(** {1:decode Decode} *)

val decode :
  ?ctx:Jsont.Context.t -> 'a Jsont.t -> Jstr.t -> ('a, Jv.Error.t) result
(** [decode t s] decodes the JSON data [s] according to [t]. *)

val decode' :
  ?ctx:Jsont.Context.t -> 'a Jsont.t -> Jstr.t -> ('a, Jsont.Error.t) result
(** [decode' t s] is like {!val-decode} but preserves the error structure. *)

val decode_jv :
  ?ctx:Jsont.Context.t ->  'a Jsont.t -> Jv.t -> ('a, Jv.Error.t) result
(** [decode_jv t v] decodes the JavaScript value [v] according to [t]. *)

val decode_jv' :
  ?ctx:Jsont.Context.t -> 'a Jsont.t -> Jv.t -> ('a, Jsont.Error.t) result
(** [decode_jv'] is like {!decode_jv'} but preserves the error structure. *)

(** {1:encode Encode} *)

val encode :
  ?ctx:Jsont.Context.t -> ?format:Jsont.format -> 'a Jsont.t -> 'a ->
  (Jstr.t, Jv.Error.t) result
(** [encode t v] encodes [v] to JSON according to [t]. [format]
    specifies how the JSON is formatted, defaults to
    {!Jsont.Minify}. The {!Jsont.Layout} format is unsupported,
    {!Jsont.Indent} is used instead. *)

val encode' :
  ?ctx:Jsont.Context.t -> ?format:Jsont.format -> 'a Jsont.t -> 'a ->
  (Jstr.t, Jsont.Error.t) result
(** [encode'] is like {!val-encode} but preserves the error structure.
    [format] specifies how the JSON is formatted, defaults to
    {!Jsont.Minify}. The {!Jsont.Layout} format is unsupported,
    {!Jsont.Indent} is used instead. *)

val encode_jv :
  ?ctx:Jsont.Context.t -> 'a Jsont.t -> 'a -> (Jv.t, Jv.Error.t) result
(** [encode_jv t v] encodes [v] to a JavaScript value according to [t]. *)

val encode_jv' :
  ?ctx:Jsont.Context.t -> 'a Jsont.t -> 'a -> (Jv.t, Jsont.Error.t) result
(** [encode_jv'] is like {!val-encode_jv} but preserves the error structure. *)

(** {1:recode Recode} *)

val recode :
  ?ctx:Jsont.Context.t -> ?format:Jsont.format -> 'a Jsont.t -> Jstr.t ->
  (Jstr.t, Jv.Error.t) result
(** [recode] is {!val-decode} followed by {!val-encode}. *)

val recode' :
  ?ctx:Jsont.Context.t -> ?format:Jsont.format -> 'a Jsont.t -> Jstr.t ->
  (Jstr.t, Jsont.Error.t) result
(** [recode] is {!val-decode'} followed by {!val-encode'}. *)

val recode_jv :
  ?ctx:Jsont.Context.t -> 'a Jsont.t -> Jv.t -> (Jv.t, Jv.Error.t) result
(** [recode] is {!val-decode} followed by {!val-encode}. *)

val recode_jv' :
  ?ctx:Jsont.Context.t -> 'a Jsont.t -> Jv.t -> (Jv.t, Jsont.Error.t) result
(** [recode] is {!val-decode_jv'} followed by {!encode_jv'}. *)
