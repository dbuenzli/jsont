(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* These three things should really belong to String. *)

let string_subrange ?(first = 0) ?last s =
  let max = String.length s - 1 in
  let last = match last with
  | None -> max
  | Some l when l > max -> max
  | Some l -> l
  in
  let first = if first < 0 then 0 else first in
  if first > last then "" else
  String.sub s first (last - first + 1)

let edit_distance s0 s1 =
  let min_by f a b = if f a <= f b then a else b in
  let max_by f a b = if f a <= f b then b else a in
  let minimum a b c = min a (min b c) in
  let s0 = min_by String.length s0 s1     (* row *)
  and s1 = max_by String.length s0 s1 in  (* column *)
  let m = String.length s0 and n = String.length s1 in
  let rec rows row0 row i =
    if i > n then row0.(m) else begin
      row.(0) <- i;
      for j = 1 to m do
        if s0.[j - 1] = s1.[i - 1] then row.(j) <- row0.(j - 1) else
        row.(j) <- minimum (row0.(j - 1) + 1) (row0.(j) + 1) (row.(j - 1) + 1)
      done;
      rows row row0 (i + 1)
    end in
  rows (Array.init (m + 1) (fun x -> x)) (Array.make (m + 1) 0) 1

let suggest ?(dist = 2) candidates s =
  let add (min, acc) name =
    let d = edit_distance s name in
    if d = min then min, (name :: acc) else
    if d < min then d, [name] else
    min, acc
  in
  let d, suggs = List.fold_left add (max_int, []) candidates in
  if d <= dist (* suggest only if not too far *) then List.rev suggs else []

(* Hex converters *)

let lower_hex_digit n =
  let n = n land 0xF in
  Char.unsafe_chr (if n < 10 then 0x30 + n else 0x57 + n)

let binary_string_to_hex s =
  let rec loop max s i h k =
    if i > max then Bytes.unsafe_to_string h else
    let byte = Char.code s.[i] in
    Bytes.set h k (lower_hex_digit (byte lsr 4));
    Bytes.set h (k + 1) (lower_hex_digit byte);
    loop max s (i + 1) h (k + 2)
  in
  let len = String.length s in
  let h = Bytes.create (2 * len) in
  loop (len - 1) s 0 h 0

exception Illegal_hex of int

let binary_string_of_hex h =
  let hex_value s i = match s.[i] with
  | '0' .. '9' as c -> Char.code c - 0x30
  | 'A' .. 'F' as c -> 10 + (Char.code c - 0x41)
  | 'a' .. 'f' as c -> 10 + (Char.code c - 0x61)
  | _ -> raise_notrace (Illegal_hex i)
  in
  try match String.length h with
  | len when len mod 2 <> 0 -> raise (Illegal_hex len)
  | len ->
      let rec loop max s i h k =
        if i > max then Ok (Bytes.unsafe_to_string s) else
        let hi = hex_value h k and lo = hex_value h (k + 1) in
        Bytes.set s i (Char.chr @@ (hi lsl 4) lor lo);
        loop max s (i + 1) h (k + 2)
      in
      let s_len = len / 2 in
      let s = Bytes.create s_len in
      loop (s_len - 1) s 0 h 0
  with Illegal_hex i ->
    if i = String.length h
    then Error "Missing final hexadecimal digit" else
    let c = String.get_uint8 h i in
    Error (Printf.sprintf "%d: byte x%x not an ASCII hexadecimal digit" i c)

(* Type identifiers. *)

module Type = struct (* Can be removed once we require OCaml 5.1 *)
  type (_, _) eq = Equal : ('a, 'a) eq
  module Id = struct
    type _ id = ..
    module type ID = sig type t type _ id += Id : t id end
    type 'a t = (module ID with type t = 'a)

    let make (type a) () : a t =
      (module struct type t = a type _ id += Id : t id end)

    let provably_equal
        (type a b) ((module A) : a t) ((module B) : b t) : (a, b) eq option
      =
      match A.Id with B.Id -> Some Equal | _ -> None

    let uid (type a) ((module A) : a t) =
      Obj.Extension_constructor.id (Obj.Extension_constructor.of_val A.Id)
  end
end

(* Resizable arrays *)

module Rarray = struct
  type 'a t =
    { mutable els : 'a array;
      mutable max : int; (* index of last element of [els]. *) }

  let get a i = a.els.(i)
  let empty () = { els = [||]; max = -1 }
  let grow a v =
    let len = a.max + 1 in
    let nlen = if len = 0 then 256 else len in
    let els' = Array.make (2 * nlen) v in
    Array.blit a.els 0 els' 0 len; a.els <- els'

  let length a = a.max + 1
  let add_last v a =
    let max = a.max + 1 in
    if max = Array.length a.els then grow a v;
    a.max <- max; a.els.(max) <- v; a

  let to_array a =
    if a.max + 1 = Array.length a.els then a.els else
    let v = Array.make (a.max + 1) a.els.(0) in
    Array.blit a.els 0 v 0 (a.max + 1);
    v
end

(* Resizable bigarrays *)

module Rbigarray1 = struct
  type ('a, 'b, 'c) t =
    { mutable els : ('a, 'b, 'c) Bigarray.Array1.t;
      mutable max : int; (* index of the last element of [els]. *)  }

  let get a i = Bigarray.Array1.get a.els i

  let empty kind layout =
    { els = Bigarray.Array1.create kind layout 0; max = -1 }

  let grow a v =
    let len = a.max + 1 in
    let len = if len = 0 then 256 else len in
    let init i = Bigarray.Array1.(if i <= a.max then get a.els i else v) in
    let k, l = Bigarray.Array1.(kind a.els, layout a.els) in
    let els' = Bigarray.Array1.init k l (2 * len) init in
    a.els <- els'

  let length a = a.max + 1
  let add_last v a =
    let max = a.max + 1 in
    if max = Bigarray.Array1.dim a.els then grow a v;
    a.max <- max; Bigarray.Array1.set a.els max v; a

  let to_bigarray a =
    if a.max + 1 = Bigarray.Array1.dim a.els then a.els else
    let init i = Bigarray.Array1.get a.els i in
    let k, l = Bigarray.Array1.(kind a.els, layout a.els) in
    Bigarray.Array1.init k l (a.max + 1) init
end

(* Mini fmt *)

module Fmt = struct
  type 'a t = Format.formatter -> 'a -> unit
  let pf = Format.fprintf
  let str = Format.asprintf
  let nop _ () = ()
  let sp = Format.pp_print_space
  let comma ppf () = Format.pp_print_char ppf ','; sp ppf ()
  let list = Format.pp_print_list
  let char = Format.pp_print_char
  let string = Format.pp_print_string
  let substring first len ppf s =
    if first = 0 && len = String.length s then string ppf s else
    (* One day use https://github.com/ocaml/ocaml/pull/12133 *)
    for i = first to first + len - 1 do char ppf s.[i] done

  let lines ppf s =
    Format.pp_print_list string ppf (String.split_on_char '\n' s)

  (* ANSI styling

     Note this is the scheme we have in B0_std.Fmt but obviously
     we can't depend on it. For now we decided not to surface it
     at the library level. Ideally something should be provided
     upstream. *)

  type styler = Ansi | Plain
  let styler' =
    ref begin match Sys.getenv_opt "TERM" with
    | None when Sys.backend_type = Other "js_of_ocaml" -> Ansi
    | None | Some "dumb" -> Plain
    | _ -> Ansi
    end

  let set_styler st = styler' := st
  let styler () = !styler'
  let ansi_reset = "\x1B[0m"
  let bold ppf s =
    if !styler' = Plain then string ppf s else
    pf ppf "@<0>%s%s@<0>%s" "\x1B[1m" s ansi_reset

  let bold_red ppf s =
    if !styler' = Plain then string ppf s else
    pf ppf "@<0>%s%s@<0>%s" "\x1B[31;1m" s ansi_reset

  let code = bold
  let puterr ppf () = bold_red ppf "Error"; char ppf ':'

  let disable_ansi_styler () = set_styler Plain

  (* HCI fragments *)

  let op_enum op ?(empty = nop) pp_v ppf = function
  | [] -> empty ppf ()
  | [v] -> pp_v ppf v
  | _ as vs ->
      let rec loop ppf = function
      | [v0; v1] -> pf ppf "%a@ %s@ %a" pp_v v0 op pp_v v1
      | v :: vs -> pf ppf "%a,@ " pp_v v; loop ppf vs
      | [] -> assert false
      in
      loop ppf vs

  let or_enum ?empty pp_v ppf vs = op_enum "or" ?empty pp_v ppf vs

  let should_it_be pp_v ppf = function
  | [] -> () | vs -> pf ppf "Should it be %a ?" (or_enum pp_v) vs

  let must_be pp_v ppf = function
  | [] -> () | vs -> pf ppf "Must be %a." (or_enum pp_v) vs

  let unexpected ~kind pp_v ppf v = pf ppf "Unexpected %a: %a." kind () pp_v v
  let unexpected' ~kind pp_v ~hint ppf (v, hints) = match hints with
  | [] -> unexpected ~kind pp_v ppf v
  | hints -> unexpected ~kind pp_v ppf v; sp ppf (); (hint pp_v) ppf hints

  let out_of_dom ?pp_kind () ppf (s, ss) =
    let kind = match pp_kind with
    | None -> fun ppf () -> string ppf "value" | Some pp_kind -> pp_kind
    in
    let hint, ss = match suggest ss s with
    | [] -> must_be, ss | ss -> should_it_be, ss
    in
    pf ppf "@[%a@]" (unexpected' ~kind code ~hint) (s, ss)

  let similar_mems ppf (exp, fnd) = match suggest fnd exp with
  | [] -> () | ms ->
      pf ppf "@;@[Similar members in object: %a@]" (list ~pp_sep:comma code) ms

  let should_it_be_mem ppf (exp, fnd) = match suggest fnd exp with
  | [] -> () | ms ->  pf ppf "@;@[%a@]" (should_it_be code) ms

  (* JSON formatting *)

  type json_number_format = (float -> unit, Format.formatter, unit) format
  let json_default_number_format : json_number_format = format_of_string "%.17g"

  let json_null ppf () = string ppf "null"
  let json_bool ppf b = string ppf (if b then "true" else "false")
  let json_number' fmt ppf f = (* cf. ECMAScript's JSON.stringify *)
    if Float.is_finite f then pf ppf fmt f else json_null ppf ()

  let json_number ppf v = json_number' json_default_number_format ppf v
  let json_string ppf s =
    let is_control = function '\x00' .. '\x1F' | '\x7F' -> true | _ -> false in
    let len = String.length s in
    let max_idx = len - 1 in
    let flush ppf start i =
      if start < len then substring start (i - start) ppf s
    in
    let rec loop start i =
      if i > max_idx then flush ppf start i else
      let next = i + 1 in
      match String.get s i with
      | '"' -> flush ppf start i; string ppf "\\\""; loop next next
      | '\\' -> flush ppf start i; string ppf "\\\\"; loop next next
      | '\n' -> flush ppf start i; string ppf "\\n"; loop next next
      | '\r' -> flush ppf start i; string ppf "\\r"; loop next next
      | '\t' -> flush ppf start i; string ppf "\\t"; loop next next
      | c when is_control c ->
          flush ppf start i;
          string ppf (Printf.sprintf "\\u%04X" (Char.code c));
          loop next next
      | _c -> loop start next
    in
    char ppf '"'; loop 0 0; char ppf '"'
end

(* Text locations *)

module Textloc = struct

  (* File paths *)

  type fpath = string
  let file_none = "-"
  let pp_path = Format.pp_print_string

  (* Byte positions *)

  type byte_pos = int (* zero-based *)
  let byte_pos_none = -1

  (* Lines *)

  type line_num = int (* one-based *)
  let line_num_none = -1

  (* Line positions

     We keep the byte position of the first element on the line. This
     first element may not exist and be equal to the text length if
     the input ends with a newline. Editors expect tools to compute
     visual columns (not a very good idea). By keeping these byte
     positions we can approximate columns by subtracting the line byte
     position data byte location. This will only be correct on
     US-ASCII data. *)

  type line_pos = line_num * byte_pos
  let line_pos_first = 1, 0
  let line_pos_none = line_num_none, byte_pos_none

  (* Text locations *)

  type t =
    { file : fpath;
      first_byte : byte_pos; last_byte : byte_pos;
      first_line : line_pos; last_line : line_pos }

  let make ~file ~first_byte ~last_byte ~first_line ~last_line =
    { file; first_byte; last_byte; first_line; last_line }

  let file l = l.file
  let first_byte l = l.first_byte
  let last_byte l = l.last_byte
  let first_line l = l.first_line
  let last_line l = l.last_line
  let none =
    let first_byte = byte_pos_none and last_byte = byte_pos_none in
    let first_line = line_pos_none and last_line = line_pos_none in
    make ~file:file_none ~first_byte ~last_byte ~first_line ~last_line

  (* Predicates and comparisons *)

  let is_none l = l.first_byte < 0
  let is_empty l = l.first_byte > l.last_byte
  let equal l0 l1 =
    String.equal l0.file l1.file &&
    Int.equal l0.first_byte l1.first_byte &&
    Int.equal l0.last_byte l1.last_byte

  let compare l0 l1 =
    let c = String.compare l0.file l1.file in
    if c <> 0 then c else
    let c = Int.compare l0.first_byte l1.first_byte in
    if c <> 0 then c else
    Int.compare l0.last_byte l1.last_byte

  (* Shrink and stretch *)

  let set_first l ~first_byte ~first_line = { l with first_byte; first_line }
  let set_last l ~last_byte ~last_line = { l with last_byte; last_line }

  [@@@warning "-6"]
  let to_first l =
    make l.file l.first_byte l.first_byte l.first_line l.first_line

  let to_last l =
    make l.file l.last_byte l.last_byte l.last_line l.last_line

  let before l =
    make l.file l.first_byte byte_pos_none l.first_line line_pos_none

  let after l =
    make l.file (l.first_byte + 1) byte_pos_none l.last_line line_pos_none
  [@@@warning "+6"]

  let span l0 l1 =
    let first_byte, first_line =
      if l0.first_byte < l1.first_byte
      then l0.first_byte, l0.first_line
      else l1.first_byte, l1.first_line
    in
    let last_byte, last_line, file =
      if l0.last_byte < l1.last_byte
      then l1.last_byte, l1.last_line, l1.file
      else l0.last_byte, l0.last_line, l0.file
    in
    make ~file ~first_byte ~first_line ~last_byte ~last_line

  [@@@warning "-6"]
  let reloc ~first ~last =
    make last.file first.first_byte last.last_byte first.first_line
      last.last_line
  [@@@warning "+6"]

  (* Formatters *)

  let pf = Format.fprintf
  let pp_ocaml ppf l = match is_none l with
  | true -> pf ppf "File \"%a\"" pp_path l.file
  | false ->
      let pp_lines ppf l = match fst l.first_line = fst l.last_line with
      | true -> pf ppf "line %d" (fst l.first_line)
      | false -> pf ppf "lines %d-%d" (fst l.first_line) (fst l.last_line)
      in
      (* "characters" represent positions (insertion points) not columns *)
      let pos_s = l.first_byte - snd l.first_line in
      let pos_e = l.last_byte - snd l.last_line + 1 in
      if pos_s = 0 && pos_e = 0
      then pf ppf "File \"%a\", %a" pp_path l.file pp_lines l
      else pf ppf "File \"%a\", %a, characters %d-%d"
          pp_path l.file pp_lines l pos_s pos_e

  let pp_gnu ppf l = match is_none l with
  | true -> pf ppf "%a:" pp_path l.file
  | false ->
      let pp_lines ppf l =
        let col_s = l.first_byte - snd l.first_line + 1 in
        let col_e = l.last_byte - snd l.last_line + 1 in
        match fst l.first_line = fst l.last_line with
        | true ->  pf ppf "%d.%d-%d" (fst l.first_line) col_s col_e
        | false ->
            pf ppf "%d.%d-%d.%d"
              (fst l.first_line) col_s (fst l.last_line) col_e
      in
      pf ppf "%a:%a" pp_path l.file pp_lines l

  let pp = pp_ocaml

  let pp_dump ppf l =
    pf ppf "file:%s bytes:%d-%d lines:%d-%d lines-bytes:%d-%d"
      l.file l.first_byte l.last_byte (fst l.first_line) (fst l.last_line)
      (snd l.first_line) (snd l.last_line)
end

type 'a fmt = Stdlib.Format.formatter -> 'a -> unit

(* Node meta data *)

module Meta = struct
  type t =
    { textloc : Textloc.t;
      ws_before : string;
      ws_after : string; }

  let make ?(ws_before = "") ?(ws_after = "") textloc =
    { textloc; ws_before; ws_after }

  let none = { textloc = Textloc.none; ws_before = ""; ws_after = "" }
  let is_none m = none == m
  let textloc m = m.textloc
  let ws_before m = m.ws_before
  let ws_after m = m.ws_after
  let with_textloc m textloc = { m with textloc }
  let clear_ws m = { m with ws_before = ""; ws_after = "" }
  let clear_textloc m = { m with textloc = Textloc.none }
  let copy_ws src ~dst =
    { dst with ws_before = src.ws_before; ws_after = src.ws_after }
end

type 'a node = 'a * Meta.t

(* JSON numbers *)

module Number = struct
  let number_contains_int = Sys.int_size <= 53
  let min_exact_int = if number_contains_int then Int.min_int else -(1 lsl 53)
  let max_exact_int = if number_contains_int then Int.max_int else 1 lsl 53
  let min_exact_uint8 = 0 let max_exact_uint8 = 255
  let min_exact_uint16 = 0 let max_exact_uint16 = 65535
  let min_exact_int8 = -128 let max_exact_int8 = 127
  let min_exact_int16 = -32768 let max_exact_int16 = 32767
  let min_exact_int32 = Int32.min_int let max_exact_int32 = Int32.max_int
  let max_exact_int64 = Int64.shift_left 1L 53
  let min_exact_int64 = Int64.neg max_exact_int64

  let[@inline] int_is_uint8 v = v land (lnot 0xFF) = 0
  let[@inline] int_is_uint16 v = v land (lnot 0xFFFF) = 0
  let[@inline] int_is_int8 v = min_exact_int8 <= v && v <= max_exact_int8
  let[@inline] int_is_int16 v = min_exact_int16 <= v && v <= max_exact_int16

  let[@inline] can_store_exact_int v =
    min_exact_int <= v && v <= max_exact_int

  let[@inline] can_store_exact_int64 v =
    Int64.(compare min_exact_int64 v <= 0 && compare v max_exact_int64 <= 0)

  let max_exact_int_float = Int.to_float max_exact_int
  let min_exact_int_float = Int.to_float min_exact_int
  let max_exact_uint8_float = Int.to_float max_exact_uint8
  let min_exact_uint8_float = Int.to_float min_exact_uint8
  let max_exact_uint16_float = Int.to_float max_exact_uint16
  let min_exact_uint16_float = Int.to_float min_exact_uint16
  let max_exact_int8_float = Int.to_float max_exact_int8
  let min_exact_int8_float = Int.to_float min_exact_int8
  let min_exact_int16_float = Int.to_float min_exact_int16
  let max_exact_int16_float = Int.to_float max_exact_int16
  let max_exact_int32_float = Int32.to_float max_exact_int32
  let min_exact_int32_float = Int32.to_float min_exact_int32
  let max_exact_int64_float = Int64.to_float max_exact_int64
  let min_exact_int64_float = Int64.to_float min_exact_int64

  let[@inline] in_exact_int_range v =
    min_exact_int_float <= v && v <= max_exact_int_float

  let[@inline] in_exact_uint8_range v =
    min_exact_uint8_float <= v && v <= max_exact_uint8_float

  let[@inline] in_exact_uint16_range v =
    min_exact_uint16_float <= v && v <= max_exact_uint16_float

  let[@inline] in_exact_int8_range v =
    min_exact_int8_float <= v && v <= max_exact_int8_float

  let[@inline] in_exact_int16_range v =
    min_exact_int16_float <= v && v <= max_exact_int16_float

  let[@inline] in_exact_int32_range v =
    min_exact_int32_float <= v && v <= max_exact_int32_float

  let[@inline] in_exact_int64_range v =
    min_exact_int64_float <= v && v <= max_exact_int64_float
end

(* JSON Paths *)

module Path = struct

  (* Indices *)

  type index = Mem of string node | Nth of int node

  let pp_name ppf n = Fmt.code ppf n
  let pp_index_num ppf n = Fmt.code ppf (Int.to_string n)

  let pp_index ppf = function
  | Mem (n, _) -> pp_name ppf n
  | Nth (n, _) -> Fmt.pf ppf "[%a]" pp_index_num n

  let pp_index_trace ppf = function
  | Mem (n, meta) ->
      Fmt.pf ppf "%a: in member %a" Textloc.pp (Meta.textloc meta) pp_name n
  | Nth (n, meta) ->
      Fmt.pf ppf "%a: at index %a" Textloc.pp (Meta.textloc meta) pp_index_num n

  let pp_bracketed_index ppf = function
  | Mem (n, _) -> Fmt.pf ppf "[%a]" pp_name n
  | Nth (n, _) -> Fmt.pf ppf "[%a]" pp_index_num n

  (* Paths *)

  type t = index list
  let root = []
  let is_root = function [] -> true | _ -> false
  let nth ?(meta = Meta.none) n p = Nth (n, meta) :: p
  let mem ?(meta = Meta.none) n p = Mem (n, meta) :: p
  let rev_indices p = p
  let pp ppf is =
    let pp_sep ppf () = Fmt.char ppf '.' in
    Fmt.list ~pp_sep pp_index ppf (List.rev is)

  let pp_trace ppf is =
    if is <> [] then Fmt.pf ppf "@,@[<v>%a@]" (Fmt.list pp_index_trace) is

  let none = []
  let err i fmt = Format.kasprintf failwith ("%d: " ^^ fmt) i
  let err_unexp_eoi i = err i "Unexpected end of input"
  let err_unexp_char i s = err i "Unexpected character: %C" s.[i]
  let err_illegal_char i s = err i "Illegal character here: %C" s.[i]
  let err_unexp i s = err i "Unexpected input: %S" (string_subrange ~first:i s)

  (* Parsing *)

  let parse_eoi s i max = if i > max then () else err_unexp i s
  let parse_index p s i max =
    let first, stop = match s.[i] with '[' -> i + 1, ']' | _ -> i, '.' in
    let last, next =
      let rec loop stop s i max = match i > max with
      | true -> if stop = ']' then err_unexp_eoi i else (i - 1), i
      | false ->
          let illegal = s.[i] = '[' || (s.[i] = ']' && stop = '.') in
          if illegal then err_illegal_char i s else
          if s.[i] <> stop then loop stop s (i + 1) max else
          (i - 1), if stop = ']' then i + 1 else i
      in
      loop stop s first max
    in
    let idx = string_subrange ~first ~last s in
    if idx = "" then err first "illegal empty index" else
    match int_of_string idx with
    | exception Failure _ -> next, (Mem (idx, Meta.none)) :: p
    | idx -> next, (Nth (idx, Meta.none)) :: p

  let of_string s =
    let rec loop p s i max =
      if i > max then p else
      let next, p = parse_index p s i max in
      if next > max then p else
      if s.[next] <> '.' then err_unexp_char next s else
      if next + 1 <= max then loop p s (next + 1) max else
      err_unexp_eoi next
    in
    try
      if s = "" then Ok [] else
      let start = if s.[0] = '.' then 1 else 0 in
      Ok (loop [] s start (String.length s - 1))
    with Failure e -> Error e

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

(* JSON sorts *)

module Sort = struct
  type t = Null | Bool | Number | String | Array | Object
  let to_string = function
  | Null -> "null" | Bool -> "bool" | Number -> "number"
  | String  -> "string" | Array  -> "array" | Object -> "object"

  let kinded' ~kind:k s = if k = "" then s else String.concat " " [k; s]
  let kinded ~kind  sort = kinded' ~kind (to_string sort)
  let or_kind ~kind sort = if kind <> "" then kind else (to_string sort)
  let pp ppf s = Fmt.code ppf (to_string s)
end
