(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* This syntax/idea does not work well with JSON it worked well with
   s-expressions because of their uniform nature e.g. to insert
   bindings.

   That would still work on arrays though. Maybe we could add
   something at some point. *)

module Path : sig


  (** {1:carets Carets} *)

  (** Carets.

      A path and a spatial localisation. *)
  module Caret : sig

    (** {1:caret Carets} *)

    type path := t

    type pos =
    | Before (** The void before the data indexed by a path. *)
    | Over (** The data indexed by a path. *)
    | After (** The void after the data indexed by a path. *)
    (** The type for caret positions. *)

    type t = pos * path
    (** The type for carets. A path and a caret position. *)

    val of_string : string -> (t, string) result
    (** [of_string s] parses a caret according to
        the {{!path_caret_syntax}caret syntax} .*)

    val pp : t fmt
    (** [pp] formats carets. *)
  end

  val over : t -> Caret.t
  (** [over p] is the data at the path [p]. *)

  val before : t -> Caret.t
  (** [before p] is the void before the path [p]. *)

  val after : t -> Caret.t
  (** [after p] is the void after the path [p]. *)

  (** {1:path_caret_syntax Path & caret syntax}

      Path and carets provide a way for end users to address JSON and
      edit locations.

      A {e path} is a sequence of member and list indexing
      operations. Applying the path to a JSON value leads to either a
      JSON value or nothing if one of the indices does not exist, or
      an error if ones tries to index a non-indexable value.

      A {e caret} is a path and a spatial specification for the JSON
      construct found by the path. The caret indicates either the void
      {e before} that JSON construct, the JSON value itself ({e over}) or
      the void {e after} it.

      Here are a few examples of paths and carets, syntactically the
      charater ['v'] is used to denote the caret's insertion point before or
      after a path. There's no distinction between a path and an over caret.

      {@json[
        {
          "ocaml": {
            "libs": ["jsont", "brr", "cmdliner"]
          }
        }
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
      the last element). Or [i] can be an object member name [n]. If
      there is no ambiguity, the surrounding brackets can be dropped.

      A {e caret} is a path whose last index brackets can be prefixed or
      suffixed by an insertion point, represented by the character
      ['v'].  This respectively denote the void before or after the
      JSON construct found by the path.

      {b Notes.}
      {ul
      {- The syntax has no form of quoting at the moment this
         means key names can't contain, [\[], [\]], or start with a number.}
      {- It would be nice to be able to drop the dots in order
         to be compatible with {{:https://www.rfc-editor.org/rfc/rfc9535}
         JSONPath} syntax.}} *)
end = struct


  (* Carets *)

  module Caret = struct
    type path = t
    type pos = Before | Over | After
    type t = pos * path
    let pp ppf = function
    | Over, p -> pp ppf p
    | Before, (c :: p)->
        pp ppf p;
        (if p <> [] then Fmt.char ppf '.');
        Fmt.char ppf 'v'; pp_bracketed_index ppf c
    | After, (c :: p) ->
        pp ppf p;
        (if p <> [] then Fmt.char ppf '.');
        pp_bracketed_index ppf c; Fmt.char ppf 'v'
    | _ -> ()

    (* Parsing *)

    let of_string s =
      let rec loop p s i max =
        if i > max then Over, p else
        let next = i + 1 in
        match s.[i] with
        | 'v' when next <= max && s.[next] = '[' ->
            let next, p = parse_index p s next max in
            parse_eoi s next max; Before, p
        | c ->
            let next, p = parse_index p s i max in
            if next > max then Over, p else
            if s.[next] = 'v'
            then (parse_eoi s (next + 1) max; After, p) else
            if s.[next] <> '.' then err_unexp_char next s else
            if next + 1 <= max then loop p s (next + 1) max else
            err_unexp_eoi next
      in
      try
        if s = "" then Ok (Over, []) else
        let start = if s.[0] = '.' then 1 else 0 in
        Ok (loop [] s start (String.length s - 1))
      with Failure e -> Error e
  end

  let over p = Caret.Over, p
  let after p = Caret.After, p
  let before p = Caret.Before, p
end
