(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw
open Jsont.Repr

(* XXX add these things to Stdlib.Uchar *)

let uchar_max_utf_8_byte_length = 4
let uchar_utf_8_byte_decode_length byte = (* or utf_8_byte_length_of_byte *)
  if byte < 0x80 then 1 else if byte < 0xC2 then 0 else
  if byte < 0xE0 then 2 else if byte < 0xF0 then 3 else
  if byte < 0xF5 then 4 else 0

(* Character classes *)

let[@inline] is_digit u = 0x0030 (* 0 *) <= u && u <= 0x0039 (* 9 *)
let[@inline] is_number_start u = is_digit u || u = 0x002D (* - *)
let[@inline] is_surrogate u = 0xD800 <= u && u <= 0xDFFF
let[@inline] is_hi_surrogate u = 0xD800 <= u && u <= 0xDBFF
let[@inline] is_lo_surrogate u = 0xDC00 <= u && u <= 0xDFFF
let[@inline] is_control u =
  (0x0000 <= u && u <= 0x001F) || (* C0 control characters *)
  u = 0x007F || (* Delete *)
  (0x0080 <= u && u <= 0x009F) || (* C1 control characters *)
  u = 0x2028 (* Line separator *) ||
  u = 0x2029 (* Paragraph separator *) ||
  u = 0x200E (* left-to-right mark *) ||
  u = 0x200F (* right-to-left mark *)

let sot = 0x1A0000  (* start of text U+10FFFF + 1 *)
let eot = 0x1A0001  (*   end of text U+10FFFF + 2 *)

let pp_code = Jsont.Repr.pp_code
let pp_quchar ppf u =
  pp_code ppf @@
  if u = sot then "start of text" else
  if u = eot then "end of text" else
  if is_control u || is_surrogate u then Printf.sprintf "U+%04X" u else
  let u = Uchar.of_int u in
  let b = Stdlib.Bytes.make (Uchar.utf_8_byte_length u) '\x00' in
  Stdlib.(ignore (Bytes.set_utf_8_uchar b 0 u); (Bytes.unsafe_to_string b))

(* Decoder *)

type decoder =
  { file : string;
    locs : bool; (* [true] if text locations should be computed. *)
    layout : bool; (* [true] if text layout should be kept. *)
    reader : Bytes.Reader.t; (* The source of bytes. *)
    mutable i : Stdlib.Bytes.t; (* Current input slice. *)
    mutable i_max : int; (* Maximum byte index in [i]. *)
    mutable i_next : int; (* Next byte index to read in [i]. *)
    mutable overlap : Stdlib.Bytes.t; (* Buffer for overlapping decodes. *)
    mutable u : int; (* Current Unicode scalar value or sot or eot. *)
    mutable byte_count : int; (* Global byte count. *)
    mutable line : int; (* Current line number. *)
    mutable line_start : int; (* Current line global byte position. *)
    token : Buffer.t;
    ws : Buffer.t; (* Bufferizes whitespace when layout is [true]. *) }

let make_decoder ?(locs = false) ?(layout = false) ?(file = "-") reader =
  let overlap = Stdlib.Bytes.create uchar_max_utf_8_byte_length in
  let token = Buffer.create 255 and ws = Buffer.create 255 in
  { file; locs; layout; reader; i = overlap (* overwritten by initial refill *);
    i_max = 0; i_next = 1 (* triggers an initial refill *);
    overlap; u = sot; byte_count = 0; line = 1; line_start = 0; token; ws }

(* Decoder positions *)

let[@inline] get_line_pos d =
  if d.locs then d.line, d.line_start else Jsont.Textloc.line_pos_none

let get_last_byte d =
  if not d.locs then 0 else
  if d.u <= 0x7F || d.u = eot then d.byte_count - 1 else
  if d.u = sot then 0 else
  (* On multi-bytes uchars we want to point on the first byte. *)
  d.byte_count - Uchar.utf_8_byte_length (Uchar.of_int d.u) + 1

let textloc_to_current ~first_byte ~first_line d =
  if not d.locs then Jsont.Textloc.none else
  let last_byte = get_last_byte d and last_line = get_line_pos d in
  Jsont.Textloc.make ~file:d.file ~first_byte ~last_byte ~first_line ~last_line

let textloc_current d =
  if not d.locs then Jsont.Textloc.none else
  let first_byte = get_last_byte d and first_line = get_line_pos d in
  let last_byte = first_byte and last_line = first_line in
  Jsont.Textloc.make ~file:d.file ~first_byte ~last_byte ~first_line ~last_line

let textloc_prev_ascii_char ~first_byte ~first_line d =
  (* N.B. when we call that the line doesn't move and the char was on
     a single byte *)
  if not d.locs then Jsont.Textloc.none else
  let last_byte = get_last_byte d and last_line = get_line_pos d in
  let last_byte = last_byte - 1 in
  Jsont.Textloc.make ~file:d.file ~first_byte ~last_byte ~first_line ~last_line

let meta_make d ?ws_before ?ws_after textloc =
  if not d.locs && not d.layout then Jsont.Meta.none else
  Jsont.Meta.make ?ws_before ?ws_after textloc

(* Decoder errors *)

let err_here d fmt =
  Jsont.Error.msgf (Jsont.Meta.make (textloc_current d)) fmt

let err_to_here ~first_byte ~first_line d fmt =
  let textloc = textloc_to_current ~first_byte ~first_line d  in
  Jsont.Error.msgf (Jsont.Meta.make textloc) fmt

let err_malformed_utf_8 d =
  if d.i_next > d.i_max
  then err_here d "UTF-8 decoding error: unexpected end of bytes"
  else err_here d "UTF-8 decoding error: invalid byte %a"
      pp_code (Printf.sprintf "%x02x" (Bytes.get_uint8 d.i d.i_next))

let err_exp_eot d =
  err_here d "Expected %a but found %a" pp_quchar eot pp_quchar d.u

let err_not_json_value d =
  err_here d "Expected %a but found %a" pp_code "JSON value" pp_quchar d.u

(* Errors for constants *)

let err_exp_in_const ~first_byte ~first_line d ~exp ~fnd ~const =
  err_to_here ~first_byte ~first_line d
    "Expected %a while parsing %a but found: %a"
    pp_quchar exp pp_code const pp_quchar fnd

(* Errors for numbers *)

let err_float_parse meta tok =
  Jsont.Error.msgf meta "Could not parse %S to a %a" tok pp_code "float"

let err_exp_digit d =
  err_here d "Expected %a while parsing %a but found %a"
    pp_code "decimal digit" pp_code "number" pp_quchar d.u

(* Errors for strings *)

let err_exp_hex_digit d =
  err_here d "Expected %a while parsing %a but found %a"
    pp_code "hex digit" pp_code "character escape" pp_quchar d.u

let err_exp_lo_surrogate d u =
  err_here d "Expected %a while parsing %a but found %a"
    pp_code "low surrogate" pp_code "character escape" pp_quchar u

let err_unpaired_lo_surrogate d u =
  err_here d "Unpaired low surrogate %a in %a" pp_quchar u pp_code "string"

let err_unpaired_hi_surrogate d u =
  err_here d "Unpaired high surrogate %a in %a" pp_quchar u pp_code "string"

let err_exp_esc ~first_byte ~first_line d u =
  err_to_here ~first_byte ~first_line d
    "Expected %a while parsing %a found %a"
    pp_code "escape character" pp_code "escape" pp_quchar u

let err_unclosed_string ~first_byte ~first_line d =
  err_to_here ~first_byte ~first_line d "Unclosed %a" pp_code "string"

let err_illegal_ctrl_char ~first_byte ~first_line d =
  err_to_here ~first_byte ~first_line d
    "Illegal control character %a in %a" pp_quchar d.u pp_code "string"

(* Errors for arrays *)

let err_exp_comma_or_eoa d ~fnd =
  err_here d
    "Expected %a or %a after %a but found %a"
    pp_code "," pp_code "]" pp_code "array element" pp_quchar fnd

let err_unclosed_array ~first_byte ~first_line d =
  err_to_here ~first_byte ~first_line d "Unclosed %a" pp_code "array"

let err_exp_comma_or_eoo d =
  err_here d "Expected %a or %a after %a but found: %a"
    pp_code "," pp_code "}" pp_code "object member" pp_quchar d.u

(* Errors for objects *)

let err_exp_mem d =
  err_here d "Expected %a but found %a"
    pp_code "object member" pp_quchar d.u

let err_exp_mem_or_eoo d =
  err_here d "Expected: %a or %a but found %a"
    pp_code "object member" pp_code "}" pp_quchar d.u

let err_exp_colon d =
  err_here d "Expected %a after %a but found %a"
    pp_code ":" pp_code "member name" pp_quchar d.u

let err_unclosed_object
    ~first_byte ~first_line d (map : ('o, 'o) Jsont.Repr.obj_map)
  =
  err_to_here ~first_byte ~first_line d "Unclosed %a"
    Jsont.Repr.pp_kind (Jsont.Repr.obj_map_value_kind map)

(* Decode next character in d.u *)

let[@inline] is_eoslice d = d.i_next > d.i_max
let[@inline] is_eod d = d.i_max = - 1 (* Only happens on Slice.eod *)
let[@inline] available d = d.i_max - d.i_next + 1
let[@inline] next_utf_8_length d =
  uchar_utf_8_byte_decode_length (Stdlib.Bytes.get_uint8 d.i d.i_next)

let set_slice d slice =
  d.i <- Bytes.Slice.bytes slice;
  d.i_next <- Bytes.Slice.first slice;
  d.i_max <- d.i_next + Bytes.Slice.length slice - 1

let rec setup_overlap d start need = match need with
| 0 ->
    let slice = match available d with
    | 0 -> Bytes.Reader.read d.reader
    | length -> Bytes.Slice.make d.i ~first:d.i_next ~length
    in
    d.i <- d.overlap; d.i_next <- 0; d.i_max <- start; slice
| need ->
    if is_eoslice d then set_slice d (Bytes.Reader.read d.reader);
    if is_eod d
    then (d.byte_count <- d.byte_count - start; err_malformed_utf_8 d);
    let available = available d in
    let take = Int.min need available in
    for i = 0 to take - 1 do
      Bytes.set d.overlap (start + i) (Bytes.get d.i (d.i_next + i))
    done;
    d.i_next <- d.i_next + take; d.byte_count <- d.byte_count + take;
    setup_overlap d (start + take) (need - take)

let rec nextc d = match available d with
| a when a <= 0 ->
    if is_eod d
    then d.u <- eot
    else (set_slice d (Bytes.Reader.read d.reader); nextc d)
| a when a < uchar_max_utf_8_byte_length && a < next_utf_8_length d ->
    let s = setup_overlap d 0 (next_utf_8_length d) in
    nextc d; set_slice d s
| _ ->
    let udec = Bytes.get_utf_8_uchar d.i d.i_next in
    if not (Uchar.utf_decode_is_valid udec) then err_malformed_utf_8 d else
    let u = Uchar.to_int (Uchar.utf_decode_uchar udec) in
    let ulen = Uchar.utf_decode_length udec in
    d.i_next <- d.i_next + ulen; d.byte_count <- d.byte_count + ulen;
    begin match u with
    | 0x000D (* CR *) -> d.line_start <- d.byte_count; d.line <- d.line + 1;
    | 0x000A (* LF *) ->
        d.line_start <- d.byte_count;
        if d.u <> 0x000D then d.line <- d.line + 1;
    | _ -> ()
    end;
    d.u <- u

(* Decoder layout tracking and tokenizer *)

let[@inline] ws_clear d = if d.layout then Buffer.clear d.ws
let[@inline] ws_pop d =
  if d.layout then (let t = Buffer.contents d.ws in ws_clear d; t) else ""

let[@inline] ws_add d =
  if d.layout then (Buffer.add_utf_8_uchar d.ws (Uchar.unsafe_of_int d.u))

let[@inline] token_clear d = Buffer.clear d.token
let[@inline] token_pop d = let t = Buffer.contents d.token in (token_clear d; t)
let[@inline] token_add d u =
  Buffer.add_utf_8_uchar d.token (Uchar.unsafe_of_int u)

let[@inline] accept d = token_add d d.u; nextc d

let token_pop_float d ~meta =
  let token = token_pop d in
  match float_of_string_opt token with
  | None -> err_float_parse meta token (* likely [assert false] *)
  | Some f -> f

(* Decoding *)

let current_json_sort d = match d.u with
| 0x0066 (* f *) | 0x0074 (* t *) -> Jsont.Sort.Bool
| 0x006E (* n *) -> Jsont.Sort.Null
| 0x007B (* { *) -> Jsont.Sort.Obj
| 0x005B (* [ *) -> Jsont.Sort.Array
| 0x0022 (* DQUOTE *) -> Jsont.Sort.String
| u when is_number_start u -> Jsont.Sort.Number
| _ -> err_not_json_value d

let type_error d t =
  let meta = Jsont.Meta.make (textloc_current d) in
  Jsont.Repr.type_error meta t ~fnd:(current_json_sort d)

let false_uchars = [| 0x0066; 0x0061; 0x006C; 0x0073; 0x0065 |]
let true_uchars  = [| 0x0074; 0x0072; 0x0075; 0x0065 |]
let null_uchars  = [| 0x006E; 0x0075; 0x006C; 0x006C |]
let ascii_str us = String.init (Array.length us) (fun i -> Char.chr us.(i))

let rec read_ws d = match d.u with
| 0x0020 (* SP  *) | 0x0009 (* TAB *) | 0x000A (* LF *) | 0x000D (* CR *) ->
    ws_add d; nextc d; read_ws d
| _ -> ()

let read_json_const d const = (* First character was checked. *)
  let ws_before = ws_pop d in
  let first_byte = get_last_byte d and first_line = get_line_pos d in
  for i = 1 to Array.length const - 1 do
    nextc d;
    if not (Int.equal d.u const.(i)) then
      err_exp_in_const ~first_byte ~first_line d ~exp:const.(i) ~fnd:d.u
        ~const:(ascii_str const)
  done;
  let ws_after = (nextc d; read_ws d; ws_pop d) in
  let textloc = textloc_to_current d ~first_byte ~first_line in
  meta_make d ~ws_before ~ws_after textloc

let read_json_false d = read_json_const d false_uchars
let read_json_true d = read_json_const d true_uchars
let read_json_null d = read_json_const d null_uchars
let read_json_number d = (* [is_number_start d.u] = true *)
  let rec read_digits d =
    if is_digit d.u then (accept d; read_digits d) else ()
  in
  let read_int d = match d.u with
  | 0x0030 (* 0 *) -> accept d
  | u when is_digit u -> accept d; read_digits d
  | u -> err_exp_digit d
  in
  let read_opt_frac d = match d.u with
  | 0x002E (* . *) ->
      accept d;
      if not (is_digit d.u) then err_exp_digit d else read_digits d;
  | _ -> ()
  in
  let read_opt_exp d = match d.u with
  | 0x0065 (* e *) | 0x0045 (* E *) ->
      token_add d d.u; nextc d;
      (match d.u with
      | 0x002D (* - *) | 0x002B (* + *) -> token_add d d.u; nextc d
      | _ -> ());
      if not (is_digit d.u) then err_exp_digit d;
      read_digits d;
  | _ -> ()
  in
  let first_byte = get_last_byte d and first_line = get_line_pos d in
  let ws_before = ws_pop d in
  token_clear d;
  if d.u = 0x002D (* - *) then accept d;
  read_int d;
  read_opt_frac d;
  read_opt_exp d;
  let textloc = textloc_prev_ascii_char d ~first_byte ~first_line in
  let ws_after = read_ws d; ws_pop d in
  meta_make d ~ws_before ~ws_after textloc

let read_json_string d = (* d.u is 0x0022 *)
  let first_byte = get_last_byte d and first_line = get_line_pos d in
  let rec read_uescape d hi uc count =
    if count > 0 then match d.u with
    | u when 0x0030 <= u && u <= 0x0039 ->
        nextc d; read_uescape d hi (uc * 16 + u - 0x30) (count - 1)
    | u when 0x0041 <= u && u <= 0x0046 ->
        nextc d; read_uescape d hi (uc * 16 + u - 0x37) (count - 1)
    | u when 0x0061 <= u && u <= 0x0066 ->
        nextc d; read_uescape d hi (uc * 16 + u - 0x57) (count - 1)
    | u -> err_exp_hex_digit d
    else match hi with
    | Some hi -> (* combine high and low surrogate. *)
        if not (is_lo_surrogate uc) then err_exp_lo_surrogate d uc else
        let u = (((hi land 0x3FF) lsl 10) lor (uc land 0x3FF)) + 0x10000 in
        token_add d u
    | None ->
        if not (is_surrogate uc) then token_add d uc else
        if uc > 0xDBFF then err_unpaired_lo_surrogate d uc else
        if d.u <> 0x005C (* \ *) then err_unpaired_hi_surrogate d uc else
        (nextc d;
         if d.u <> 0x0075 (* u *) then err_unpaired_hi_surrogate d uc else
         (nextc d; read_uescape d (Some uc) 0 4))
  in
  let read_escape d = match d.u with
  | 0x0022 (* DQUOTE *) | 0x005C (* \ *) | 0x002F (* / *) -> accept d
  | 0x0062 (* b *) -> token_add d 0x0008 (* backspace *); nextc d
  | 0x0066 (* f *) -> token_add d 0x000C (* form feed *); nextc d
  | 0x006E (* n *) -> token_add d 0x000A (* line feed *); nextc d
  | 0x0072 (* r *) -> token_add d 0x000D (* carriage return *); nextc d
  | 0x0074 (* t *) -> token_add d 0x0009 (* tab *); nextc d
  | 0x0075 (* u *) -> nextc d; read_uescape d None 0 4
  | u -> err_exp_esc ~first_byte ~first_line d u
  in
  let rec loop d = match d.u with
  | 0x005C (* \ *) -> nextc d; read_escape d; loop d
  | 0x0022 (* DQUOTE *) -> ()
  | u when u = eot -> err_unclosed_string ~first_byte ~first_line d
  | u when 0x0000 <= u && u <= 0x001F ->
      err_illegal_ctrl_char ~first_byte ~first_line d
  | u -> accept d; loop d
  in
  let ws_before = ws_pop d in
  nextc d; token_clear d; loop d;
  let textloc = textloc_to_current d ~first_byte ~first_line in
  let ws_after = nextc d; read_ws d; ws_pop d in
  meta_make d ~ws_before ~ws_after textloc

let read_json_name d =
  let meta (* FIXME *) = read_json_string d in
  (if d.u = 0x003A (* : *) then nextc d else err_exp_colon d);
  meta

let read_json_mem_sep d =
  if d.u = 0x007D (* } *) then () else
  if d.u = 0x002C (* , *)
  then (nextc d; read_ws d; if d.u <> 0x0022 then err_exp_mem d)
  else err_exp_comma_or_eoo d

let rec decode : type a. decoder -> a t -> a =
fun d t -> match (read_ws d; t) with
| Null map ->
    (match d.u with
    | 0x006E (* n *) -> map.dec (read_json_null d) ()
    | _ -> type_error d t)
| Bool map ->
    (match d.u with
    | 0x0066 (* f *) -> map.dec (read_json_false d) false
    | 0x0074 (* t *) -> map.dec (read_json_true d) true
    | _ -> type_error d t)
| Number map ->
    (match d.u with
    | u when is_number_start u ->
        let meta = read_json_number d in
        map.dec meta (token_pop_float d ~meta)
    | 0x006E (* n *) -> map.dec (read_json_null d) Float.nan
    | _ -> type_error d t)
| String map ->
    (match d.u with
    | 0x0022 (* DQUOTE *) ->
        let meta = read_json_string d in
        map.dec meta (token_pop d)
    | _ -> type_error d t)
| Array map ->
    (match d.u with
    | 0x005B (* [ *) -> decode_array d map
    | _ -> type_error d t)
| Obj map ->
    (match d.u with
    | 0x007B (* { *) -> decode_obj d map
    | _ -> type_error d t)
| Map map -> map.dec (decode d map.dom)
| Any map -> decode_any d t map
| Rec t -> decode d (Lazy.force t)

and decode_array : type a elt b. decoder -> (a, elt, b) array_map -> a =
fun d map ->
  let ws_before = ws_pop d in
  let first_byte = get_last_byte d and first_line = get_line_pos d in
  let b, len = match (nextc d; read_ws d; d.u) with
  | 0x005D (* ] *) -> map.dec_empty (), 0
  | _ ->
      let rec next_element d map b i =
        let b =
          try
            if map.dec_skip i b
            then (decode d (of_t Jsont.ignore); b)
            else map.dec_add i (decode d map.elt) b
          with
          | Jsont.Error e ->
              let meta = Jsont.Meta.none (* FIXME *)in
              let imeta = meta_make d (textloc_current d) in
              Jsont.Repr.error_push_array meta map (i, imeta) e
        in
        match read_ws d; d.u with
        | 0x005D (* ] *) -> b, i + 1
        | 0x002C (* , *) -> nextc d; next_element d map b (i + 1)
        | u when u = eot -> err_unclosed_array ~first_byte ~first_line d
        | fnd -> err_exp_comma_or_eoa d ~fnd
      in
      next_element d map (map.dec_empty ()) 0
  in
  let textloc = textloc_to_current d ~first_byte ~first_line in
  let ws_after = nextc d; read_ws d; ws_pop d in
  let meta = meta_make d ~ws_before ~ws_after textloc in
  map.dec_finish meta len b

and decode_obj :
  type a dec. decoder -> (a, a) obj_map -> a
=
fun d map ->
  let dict = Dict.empty in
  let dict = Dict.add Jsont.Repr.obj_meta_arg Jsont.Meta.none dict in
  let ws_before = ws_pop d in
  let first_byte = get_last_byte d and first_line = get_line_pos d in
  let dict =
    nextc d; read_ws d;
    try
      decode_obj_map
        d map (Unknown_mems None) String_map.empty String_map.empty []
        dict
    with
    | Exit -> err_unclosed_object d ~first_byte ~first_line map
  in
  let textloc = textloc_to_current d ~first_byte ~first_line in
  let ws_after = nextc d; read_ws d; ws_pop d in
  let meta = meta_make d ~ws_before ~ws_after textloc in
  let dict = Dict.add Jsont.Repr.obj_meta_arg meta dict in
  Jsont.Repr.apply_dict map.dec dict

and decode_obj_delayed : type o.
  decoder -> (o, o) obj_map -> mem_dec String_map.t ->
  mem_dec String_map.t -> Jsont.obj -> Dict.t ->
  mem_dec String_map.t * Jsont.obj * Dict.t
=
fun d map mem_miss mem_decs delay dict ->
  let rec loop d map mem_miss mem_decs rem_delay dict = function
  | [] -> mem_miss, rem_delay, dict
  | ((name, meta as nm), v as mem) :: delay ->
      match String_map.find_opt name mem_decs with
      | Some (Mem_dec m) ->
          let dict =
            try
              let t = Jsont.Repr.unsafe_to_t m.type' in
              let v = match Jsont.Json.decode' t v with
              | Ok v -> v
              | Error e -> raise_notrace (Jsont.Error e)
              in
              Dict.add m.id v dict
            with
            | Jsont.Error e ->
                let meta = Jsont.Meta.none (* FIXME *) in
                Jsont.Repr.error_push_object meta map nm e
          in
          let mem_miss = String_map.remove name mem_miss in
          loop d map mem_miss mem_decs rem_delay dict delay
      | None ->
          (* TODO I think we could handle unknown here, rather than having
             returning the rem_delay *)
          loop d map mem_miss mem_decs (mem :: rem_delay) dict delay
  in
  loop d map mem_miss mem_decs [] dict delay

and decode_obj_map : type o.
  decoder -> (o, o) obj_map -> unknown_mems_option ->
  mem_dec String_map.t -> mem_dec String_map.t -> Jsont.obj -> Dict.t -> Dict.t
=
fun d map umems mem_miss mem_decs delay dict ->
  let u n _ _ = assert false in
  let mem_miss = String_map.union u mem_miss map.mem_decs in
  let mem_decs = String_map.union u mem_decs map.mem_decs in
  match map.shape with
  | Obj_cases (umems', cases) ->
      let umems' = (Unknown_mems umems') in
      let umems,dict = Jsont.Repr.override_unknown_mems ~by:umems umems' dict in
      decode_obj_case d map umems cases mem_miss mem_decs delay dict
  | Obj_basic umems' ->
      let mem_miss, delay, dict =
        decode_obj_delayed d map mem_miss mem_decs delay dict
      in
      let umems' = Unknown_mems (Some umems') in
      let umems,dict = Jsont.Repr.override_unknown_mems ~by:umems umems' dict in
      match umems with
      | Unknown_mems (Some Unknown_skip | None) ->
          decode_obj_basic d map Unknown_skip () mem_miss mem_decs dict
      | Unknown_mems (Some (Unknown_error as u)) ->
          if delay = []
          then decode_obj_basic d map u () mem_miss mem_decs dict else
          let meta = (* FIXME *) Jsont.Meta.none in
          let fnd = List.map fst delay in
          Jsont.Repr.unexpected_mems_error meta map ~fnd
      | Unknown_mems (Some (Unknown_keep (umap, _) as u)) ->
          let add_delay umems ((n, meta as nm), v) =
            try
              let t = Jsont.Repr.unsafe_to_t umap.mems_type in
              let v = match Jsont.Json.decode' t v with
              | Ok v -> v
              | Error e -> raise_notrace (Jsont.Error e)
              in
              umap.dec_add meta n v umems
            with
            | Jsont.Error e ->
                let meta = Jsont.Meta.none (* FIXME *) in
                Jsont.Repr.error_push_object meta map nm e
          in
          let umems = List.fold_left add_delay (umap.dec_empty ()) delay in
          decode_obj_basic d map u umems mem_miss mem_decs dict

and decode_obj_basic : type o p mems builder.
  decoder -> (o, o) obj_map -> (p, mems, builder) unknown_mems -> builder ->
  mem_dec String_map.t -> mem_dec String_map.t -> Dict.t -> Dict.t
=
fun d map u umap mem_miss mem_decs dict -> match d.u with
| 0x007D (* } *) ->
    let meta = Jsont.Meta.none (* FIXME *) in
    Jsont.Repr.finish_object_decode map meta u umap mem_miss dict
| 0x0022 ->
    let meta = read_json_name d in
    let name = token_pop d in
    begin match String_map.find_opt name mem_decs with
    | Some (Mem_dec mem) ->
        let mem_miss = String_map.remove name mem_miss in
        let dict = try Dict.add mem.id (decode d mem.type') dict with
        | Jsont.Error e ->
            let ometa = Jsont.Meta.none (* FIXME *) in
            Jsont.Repr.error_push_object ometa map (name, meta) e
        in
        read_json_mem_sep d;
        decode_obj_basic d map u umap mem_miss mem_decs dict
    | None ->
        match u with
        | Unknown_skip ->
            let () = try decode d (Jsont.Repr.of_t Jsont.ignore) with
            | Jsont.Error e ->
                let ometa = Jsont.Meta.none (* FIXME *) in
                Jsont.Repr.error_push_object ometa map (name, meta) e
            in
            read_json_mem_sep d;
            decode_obj_basic d map u umap mem_miss mem_decs dict
        | Unknown_error ->
            let name = (* FIXME *) name, Jsont.Meta.none in
            let meta = (* FIXME *) Jsont.Meta.none in
            Jsont.Repr.unexpected_mems_error meta map ~fnd:[name]
        | Unknown_keep (umap', _) ->
            let umap =
              try umap'.dec_add meta name (decode d umap'.mems_type) umap with
              | Jsont.Error e ->
                  let ometa = Jsont.Meta.none (* FIXME *) in
                  Jsont.Repr.error_push_object ometa map (name, meta) e
            in
            read_json_mem_sep d;
            decode_obj_basic d map u umap mem_miss mem_decs dict
    end
| u when u = eot -> raise Exit (* Ugly *)
| fnd -> err_exp_mem_or_eoo d

and decode_obj_case : type o cases tag.
  decoder -> (o, o) obj_map -> unknown_mems_option ->
  (o, cases, tag) obj_cases -> mem_dec String_map.t -> mem_dec String_map.t ->
  Jsont.obj -> Dict.t -> Dict.t
=
fun d map umems cases mem_miss mem_decs delay dict ->
  let decode_case_tag map umems cases mem_miss mem_decs tag delay =
    let eq_tag (Case c) = cases.tag_compare c.tag tag = 0 in
    match List.find_opt eq_tag cases.cases with
    | None ->
        let meta = Jsont.Meta.none in (* FIXME *)
        Jsont.Repr.unexpected_case_tag_error meta map cases tag
    | Some (Case case) ->
        let dict =
          decode_obj_map d case.obj_map umems mem_miss mem_decs delay dict
        in
        Dict.add cases.id (case.dec (apply_dict case.obj_map.dec dict)) dict
  in
  match d.u with
  | 0x007D (* } *) ->
      (match cases.tag.dec_absent with
      | Some tag -> decode_case_tag map umems cases mem_miss mem_decs tag delay
      | None ->
          let meta = Jsont.Meta.none in (* FIXME *)
          let obj_kind = Jsont.Repr.obj_map_value_kind map in
          Jsont.Error.missing_mems meta ~obj_kind
            ~exp:[cases.tag.name]
            ~fnd:(List.map (fun ((n, _), _) -> n) delay))
  | 0x0022 ->
      let meta = read_json_name d in
      let name = token_pop d in
      if String.equal name cases.tag.name then
        let tag = try decode d cases.tag.type' with
        | Jsont.Error e ->
            let ometa = Jsont.Meta.none in (* FIXME *)
            Jsont.Repr.error_push_object ometa map (name, meta) e
        in
        read_json_mem_sep d;
        decode_case_tag map umems cases mem_miss mem_decs tag delay
      else
      begin match String_map.find_opt name mem_decs with
      | Some (Mem_dec mem) ->
          let mem_miss = String_map.remove name mem_miss in
          let dict = try Dict.add mem.id (decode d mem.type') dict with
          | Jsont.Error e ->
              let ometa = Jsont.Meta.none in (* FIXME *)
              Jsont.Repr.error_push_object ometa map (name, meta) e
          in
          read_json_mem_sep d;
          decode_obj_case d map umems cases mem_miss mem_decs delay dict
      | None ->
          (* Because JSON can be out of orer we don't know how to decode
             this yet. Generic decode *)
          let v =
            try decode d (Jsont.Repr.of_t Jsont.json) with
            | Jsont.Error e ->
                let ometa = Jsont.Meta.none in
                Jsont.Repr.error_push_object ometa map (name, meta) e
          in
          let delay = ((name, meta), v) :: delay in
          read_json_mem_sep d;
          decode_obj_case d map umems cases mem_miss mem_decs delay dict
      end
  | u when u = eot -> raise Exit (* Ugly *)
  | fnd -> err_exp_mem_or_eoo d

and decode_any : type a. decoder -> a t -> a any_map -> a =
fun d t map ->
  let case d t map = match map with
  | None -> type_error d t | Some t -> decode d t
  in
  match d.u with
  | 0x006E (* n *) -> case d t map.dec_null
  | 0x0066 (* f *)
  | 0x0074 (* t *) -> case d t map.dec_bool
  | 0x0022 (* DQUOTE *) -> case d t map.dec_string
  | 0x005B (* [ *) -> case d t map.dec_array
  | 0x007B (* { *) -> case d t map.dec_obj
  | u when is_number_start u -> case d t map.dec_number
  | _ -> err_not_json_value d

let decode' ?layout ?locs ?file t reader =
  try
    let d = make_decoder ?layout ?locs ?file reader in
    nextc d;
    let v = decode d (Jsont.Repr.of_t t) in
    if d.u <> eot then err_exp_eot d else Ok v
  with Jsont.Error e -> Error e

let decode ?layout ?locs ?file t reader =
  Result.map_error Jsont.Error.to_string (decode' ?layout ?locs ?file t reader)

let decode_string' ?layout ?locs ?file t s =
  decode' ?layout ?locs ?file t (Bytes.Reader.of_string s)

let decode_string ?layout ?locs ?file t s =
  decode ?layout ?locs ?file t (Bytes.Reader.of_string s)

(* Encoding *)

type encoder =
  { writer : Bytes.Writer.t; (* Destination of bytes. *)
    o : Bytes.t; (* Buffer for slices. *)
    o_max : int; (* Max index in [o]. *)
    mutable o_next : int; (* Next writable index in [o]. *)
    format : Jsont.format;
    number_format : (float -> unit, unit, string, unit) format4; }

let make_encoder
    ?buf:(o = Bytes.create Bytes.Slice.io_buffer_size)
    ?(format = Jsont.Minify) ?(number_format = Jsont.default_number_format)
    writer
  =
  let len = Bytes.length o in
  let number_format = string_of_format number_format in
  let number_format = Scanf.format_from_string number_format "%.17g" in
  let o_max = len - 1 and o_next = 0 in
  { writer; o; o_max; o_next; format; number_format }

let[@inline] rem_len e = e.o_max - e.o_next + 1

let flush e =
  Bytes.Writer.write e.writer (Bytes.Slice.make e.o ~first:0 ~length:e.o_next);
  e.o_next <- 0

let write_eot e = flush e; Bytes.Writer.write_eod e.writer
let write_char e c =
  if e.o_next > e.o_max then flush e;
  Stdlib.Bytes.set e.o e.o_next c; e.o_next <- e.o_next + 1

let rec write_substring e s first length =
  if length = 0 then () else
  let len = Int.min (rem_len e) length in
  if len = 0 then (flush e; write_substring e s first length) else
  begin
    Bytes.blit_string s first e.o e.o_next len;
    e.o_next <- e.o_next + len;
    write_substring e s (first + len) (length - len)
  end

let write_bytes e s = write_substring e s 0 (String.length s)
let write_sep e = write_char e ','
let write_indent e ~nest =
  for i = 1 to nest do write_char e ' '; write_char e ' ' done

let write_ws_before e m = write_bytes e (Jsont.Meta.ws_before m)
let write_ws_after e m = write_bytes e (Jsont.Meta.ws_after m)
let write_json_null e = write_bytes e "null"
let write_json_bool e b = write_bytes e (if b then "true" else "false")
let write_json_number e f =
  if Float.is_finite f
  then Printf.ksprintf (write_bytes e) e.number_format f
  else write_json_null e

let write_json_string e s =
  let is_control = function '\x00' .. '\x1F' | '\x7F' -> true | _ -> false in
  let len = String.length s in
  let flush e start i max =
    if start <= max then write_substring e s start (i - start);
  in
  let rec loop start i max =
    if i > max then flush e start i max else
    let next = i + 1 in
    match String.get s i with
    | '\"' -> flush e start i max; write_bytes e "\\\""; loop next next max
    | '\\' -> flush e start i max; write_bytes e "\\\\"; loop next next max
    | '\n' -> flush e start i max; write_bytes e "\\n"; loop next next max
    | '\r' -> flush e start i max; write_bytes e "\\r"; loop next next max
    | '\t' -> flush e start i max; write_bytes e "\\t"; loop next next max
    | c when is_control c ->
        flush e start i max;
        write_bytes e "\\u";
        write_bytes e (Printf.sprintf "%04X" (Char.code c));
        loop next next max
    | c -> loop start next max
  in
  write_char e '"'; loop 0 0 (len - 1); write_char e '"'

let encode_null (map : ('a, 'b) Jsont.Repr.base_map) e v =
  let () = map.enc v in
  match e.format with
  | Jsont.Minify | Jsont.Indent -> write_json_null e
  | Jsont.Layout ->
      let meta = map.enc_meta v in
      write_ws_before e meta;
      write_json_null e;
      write_ws_after e meta

let encode_bool (map : ('a, 'b) Jsont.Repr.base_map) e v =
  let b = map.enc v in
  match e.format with
  | Jsont.Minify | Jsont.Indent -> write_json_bool e b
  | Jsont.Layout ->
      let meta = map.enc_meta v in
      write_ws_before e meta;
      write_json_bool e b;
      write_ws_after e meta

let encode_number (map : ('a, 'b) Jsont.Repr.base_map) e v =
  let n = map.enc v in
  match e.format with
  | Jsont.Minify | Jsont.Indent -> write_json_number e n
  | Jsont.Layout ->
      let meta = map.enc_meta v in
      write_ws_before e meta;
      write_json_number e n;
      write_ws_after e meta

let encode_string (map : ('a, 'b) Jsont.Repr.base_map) e v =
  let s = map.enc v in
  match e.format with
  | Jsont.Minify | Jsont.Indent -> write_json_string e s
  | Jsont.Layout ->
      let meta = map.enc_meta v in
      write_ws_before e meta;
      write_json_string e s;
      write_ws_after e meta

let encode_mem_indent ~nest e = write_char e '\n'; write_indent e ~nest
let encode_mem_name e meta n = match e.format with
  | Jsont.Minify -> write_json_string e n; write_char e ':'
  | Jsont.Indent -> write_json_string e n; write_bytes e ": "
  | Jsont.Layout ->
      write_ws_before e meta;
      write_json_string e n;
      write_ws_after e meta;
      write_char e ':'

let rec encode : type a. nest:int -> a Jsont.Repr.t -> encoder -> a -> unit =
fun ~nest t e v -> match t with
| Null map -> encode_null map e v
| Bool map -> encode_bool map e v
| Number map -> encode_number map e v
| String map -> encode_string map e v
| Array map -> encode_array ~nest map e v
| Obj map -> encode_obj ~nest map e v
| Any map -> encode ~nest (map.enc v) e v
| Map map -> encode ~nest map.dom e (map.enc v)
| Rec t -> encode ~nest (Lazy.force t) e v

and encode_array : type a elt b.
  nest:int -> (a, elt, b) Jsont.Repr.array_map -> encoder -> a -> unit
=
fun ~nest map e v ->
   let encode_element ~nest map e i v =
     if i <> 0 then write_sep e;
     try encode ~nest map.elt e v; e with
     | Jsont.Error e ->
         Jsont.Repr.error_push_array Jsont.Meta.none map (i, Jsont.Meta.none) e
  in
  match e.format with
  | Jsont.Minify ->
      write_char e '[';
      ignore (map.enc (encode_element ~nest:(nest + 1) map) e v);
      write_char e ']'
  | Jsont.Layout ->
      let meta = map.enc_meta v in
      write_ws_before e meta;
      write_char e '[';
      ignore (map.enc (encode_element ~nest:(nest + 1) map) e v);
      write_char e ']';
      write_ws_after e meta
  | Jsont.Indent ->
      let encode_element ~nest map e i v =
        if i <> 0 then write_sep e;
        write_char e '\n';
        write_indent e ~nest;
        try encode ~nest map.elt e v; e with
        | Jsont.Error e ->
            Jsont.Repr.error_push_array
              Jsont.Meta.none map (i, Jsont.Meta.none) e
      in
      let array_not_empty e =
        e.o_next = 0 || not (Bytes.get e.o (e.o_next - 1) = '[')
      in
      write_char e '[';
      ignore (map.enc (encode_element ~nest:(nest + 1) map) e v);
      if array_not_empty e then (write_char e '\n'; write_indent e ~nest);
      write_char e ']'

and encode_obj : type o enc.
  nest:int -> (o, o) Jsont.Repr.obj_map -> encoder -> o -> unit
 =
 fun ~nest map e o -> match e.format with
 | Jsont.Minify ->
     write_char e '{';
     ignore @@
     encode_obj_map ~nest:(nest + 1) map ~do_unknown:true e ~start:true o;
     write_char e '}';
 | Jsont.Layout ->
     let meta = map.enc_meta o in
     write_ws_before e meta;
     write_char e '{';
     ignore @@
     encode_obj_map ~nest:(nest + 1) map ~do_unknown:true e ~start:true o;
     write_char e '}';
     write_ws_after e meta;
 | Jsont.Indent ->
     write_char e '{';
     let start =
       encode_obj_map ~nest:(nest + 1) map ~do_unknown:true e ~start:true o
     in
     if not start then (write_char e '\n'; write_indent e ~nest);
     write_char e '}'

and encode_obj_map : type o enc.
  nest:int -> (o, o) Jsont.Repr.obj_map -> do_unknown:bool -> encoder ->
  start:bool -> o -> bool
=
fun ~nest map ~do_unknown e ~start o ->
  let encode_mem ~nest map e o start (Mem_enc mmap) =
    try
      let v = mmap.enc o in
      if mmap.enc_omit v then start else
      begin
        if not start then write_char e ',';
        if e.format = Jsont.Indent then encode_mem_indent ~nest e;
        let meta =
          (* FIXME I think [mem_map] is a bit wrang it should
                  an enc_name_meta : Context.t -> 'o -> Meta.t
                  or simply rename enc_meta to enc_name_meta
          *)
          if e.format = Jsont.Layout then mmap.enc_meta v else Jsont.Meta.none
        in
        encode_mem_name e meta mmap.name;
        encode ~nest mmap.type' e v;
      false
      end
    with
    | Jsont.Error e ->
        Jsont.Repr.error_push_object Jsont.Meta.none map
          (mmap.name, Jsont.Meta.none) e
  in
  match map.shape with
  | Obj_basic u ->
      let start =
        List.fold_left (encode_mem ~nest map e o) start map.mem_encs
      in
      begin match u with
      | Unknown_keep (umap, enc) when do_unknown ->
          encode_unknown_mems ~nest map umap e ~start (enc o)
      | _ -> start
      end
  | Obj_cases (umap, cases) ->
      let Case_value (case, c) = cases.enc_case (cases.enc o) in
      let start =
        if cases.tag.enc_omit case.tag
        then start
        else encode_mem ~nest map e case.tag start (Mem_enc cases.tag)
      in
      let start =
        List.fold_left (encode_mem ~nest map e o) start map.mem_encs
      in
      match umap with
      | Some (Unknown_keep (umap, enc)) ->
          let start =
            encode_obj_map ~nest case.obj_map ~do_unknown:false e ~start c
          in
          encode_unknown_mems ~nest map umap e ~start (enc o)
      | _ ->
          encode_obj_map ~nest case.obj_map ~do_unknown e ~start c

and encode_unknown_mems : type o dec mems a builder.
  nest:int -> (o,o) obj_map -> (mems, a, builder) mems_map ->
  encoder -> start:bool -> mems -> bool
=
fun ~nest map umap e ~start mems ->
  let encode_unknown_mem ~nest map umap e meta n v start =
    try
      if not start then write_char e ',';
      if e.format = Jsont.Indent then encode_mem_indent ~nest e;
      encode_mem_name e meta n;
      encode ~nest umap.mems_type e v; false
    with
    | Jsont.Error e ->
        Jsont.Repr.error_push_object Jsont.Meta.none map (n, Jsont.Meta.none) e
  in
  umap.enc (encode_unknown_mem ~nest map umap e) mems start

let encode' ?buf ?format ?number_format t v ~eod w =
  let e = make_encoder ?buf ?format ?number_format w in
  let t = Jsont.Repr.of_t t in
  try Ok (encode ~nest:0 t e v; write_eot e) with
  | Jsont.Error e -> Error e

let encode ?buf ?format ?number_format t v ~eod w =
  Result.map_error Jsont.Error.to_string @@
  encode' ?buf ?format ?number_format ~eod t v w

let encode_string' ?buf ?format ?number_format t v =
  let b = Buffer.create 255 in
  let w = Bytes.Writer.of_buffer b in
  match encode' ?buf ?format ?number_format ~eod:true t v w with
  | Ok () -> Ok (Buffer.contents b) | Error _ as e -> e

let encode_string ?buf ?format ?number_format t v =
  Result.map_error Jsont.Error.to_string @@
  encode_string' ?buf ?format ?number_format t v

(* Recode *)

let unsurprising_defaults layout format = match layout, format with
| Some true, None -> Some true, Some Jsont.Layout
| None, (Some Jsont.Layout as l) -> Some true, l
| l, f -> l, f

let recode' ?layout ?locs ?file ?buf ?format ?number_format t r w ~eod =
  let layout, format = unsurprising_defaults layout format in
  match decode' ?layout ?locs ?file t r with
  | Error _ as e -> e
  | Ok v -> encode' ?buf ?format ?number_format t v ~eod w

let recode ?layout ?locs ?file ?buf ?format ?number_format t r w ~eod =
  Result.map_error Jsont.Error.to_string @@
  recode' ?layout ?locs ?file ?buf ?format ?number_format t r w ~eod

let recode_string' ?layout ?locs ?file ?buf ?format ?number_format t s =
  let layout, format = unsurprising_defaults layout format in
  match decode_string' ?layout ?locs ?file t s with
  | Error _ as e -> e
  | Ok v -> encode_string' ?buf ?format ?number_format t v

let recode_string ?layout ?locs ?file ?buf ?format ?number_format t s =
  Result.map_error Jsont.Error.to_string @@
  recode_string' ?layout ?locs ?file ?buf ?format ?number_format t s
