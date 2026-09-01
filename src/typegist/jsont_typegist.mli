(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Derive JSON types from OCaml type gists.

    The derivation assumes that the JSON type is used to serialize OCaml values
    to be read back by the derivation itself. Do not use (or perhaps only
    partially) for devising a nice JSON format or as a proxy for parsing
    a JSON format into OCaml values. *)

open Typegist

(** {1:deriving Deriving} *)

val to_jsont : versioned:bool -> 'a Type.Gist.t -> 'a Jsont.t
(** [to_jsont ~versioned g] is a JSON type derived fom [g]. In general the
    result uses:

    {ul
    {- {!Typegist.Type.Gist.val-name} for {!Jsont.kind}.}
    {- {!Typegist.Type.Gist.val-doc} for {!Jsont.doc}.}}

    If [versioned] is [true] the derivation is wrapped in a top-level case
    object that holds the current version of the derivation. This makes your
    serializations robust to potential future changes in the derivation.
    This does not however version your own data, use
    {!Typegist.Type.Gist.abstract} for that.

    {b Overriding.} The JSON type can be selectively overriden on
    a gist or field with the {!Jsont_typegist.Jsont} and
    {!Jsont_typegist.Mem_map} metadata keys.
    This happens before ignoring (see below).

    {b Ignoring} Gists or fields whose {!Typegist.Type.Gist.Meta.Ignore}
    metadata key is [true] are ignored. However in this case they {b must}
    define a {!Typegist.Type.Gist.Meta.Default} value otherwise
    the derivation is undefined. Ignored values or fields are serialized
    as [null] and deserialized as [null] only and replaced by the default
    value. If you want a gist or field to
    be ignored only for the purpose of JSON type derivation you can override
    [to_json] with {!Jsont_typegist.Jsont.ignore} or {!Mem_map.ignore}.

    {b Warning.} Do not assume that the name of an ignored field is
    secret. While the serializer does not output it by using the [enc_omit]
    argument of {!Jsont.Object.mem}, it's presence in JSON will parse
    (and thus signal its existence) not error. However it will only
    parse as a [null] value, no data can be injected.

    {b Undefined.} Unless ignored (see above), the function raises
    [Invalid_argument] in these cases:
    {ul
    {- On functional values.}
    {- On {{!Typegist.Type.Abstract.is_opaque}opaque} abstract types.}
    {- {!Typegist.Type.Gist.Meta.Ignore} is specified on a field
       but the field has no {!Typegist.Type.Gist.Meta.Default}
       value.}}
*)

(** {1:metadata Metadata} *)

(** Custom member map field metadata key.

    If a field defines a value for this key, the value
    is used for the JSON member map of the field. *)
module Mem_map : sig
  type ('a, 'b) t = ('a, 'b) Jsont.Object.Mem.map
  (** See {!Jsont.Object.Mem.type-map}. *)

  val ignore : string -> 'b -> ('a, 'b) t
  (** [ignore n v] is {!Jsont.Object.Mem.map} named [n]
      which always (de)serializes [null] with [v]. *)

  include Type.Gist.Meta.KEY
    with type ('a, 'b) value := ('a, 'b) t (** @inline *)
end

(** Custom {!Jsont.t} value metadata key.

    If a gist or field defines a value for this key, the
    {!Jsont_typegist.to_jsont} function uses it as the derived
    {!Jsont.t} value.

    Using a JSON type that always (de)serizalizes [null] maps with a constant
    value like {!Jsont.ignore} does can also be used to indicate that a
    value should not be serialized. *)
module Jsont : sig
  type 'a t = 'a Jsont.t
  (** See {!Jsont.type-t}. *)

  val ignore : 'a -> 'a t
  (** [ignore] is {!Jsont.null}. *)

  include Type.Gist.Meta.KEY
    with type ('a, 'b) value := 'b t (** @inline *)
end
