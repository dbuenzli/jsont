(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_testing
open Test_common_samples

let ( let* ) = Result.bind

(* This abstracts over codecs Jsont_brr, Jsont_bytesrw and Jsont.Json *)

type test_funs =
  { supports_layout : bool;
    decode : 'a. ?layout:bool -> 'a Jsont.t -> string -> ('a, string) result;
    encode :
      'a. ?format:Jsont.format -> 'a Jsont.t -> 'a -> (string, string) result; }

let test_funs : test_funs ref =
  ref { supports_layout = false;
        decode = (fun ?layout _ _ -> assert false);
        encode = (fun ?format _ _ -> assert false); }

let supports_layout () = !test_funs.supports_layout
let decode ?layout t json = !test_funs.decode ?layout t json
let encode ?format t v = !test_funs.encode ?format t v

(* Test combinators

   Note that the part of the test combinators rely on the library to
   be correct. If something really feels fishy you may have to
   investigate here too. *)

let decode_ok ?__POS__:pos ?value ?(eq = Test.Eq.any) t json =
  Test.block ?__POS__:pos @@ fun () ->
  match decode t json with
  | Error e -> Test.fail "%a" Fmt.lines e ~__POS__
  | Ok v' ->
      match value with
      | None -> ()
      | Some value  -> Test.eq eq v' value ~__POS__

let encode_ok ?__POS__:pos ?format t ~value json =
  Test.block ?__POS__:pos @@ fun () ->
  match encode ?format t value with
  | Error e -> Test.fail "%a" Fmt.lines e ~__POS__
  | Ok json' -> Test.string json' json ~__POS__

let decode_error ?__POS__:pos ?layout ?msg t json =
  Test.block ?__POS__:pos @@ fun () ->
  match decode ?layout t json with
  | Ok _ -> Test.fail "Decode did not error" ~__POS__
  | Error e ->
      match msg with None -> () | Some msg -> Test.styled_string msg e ~__POS__

let encode_error ?__POS__:pos ?msg t v =
  Test.block ?__POS__:pos @@ fun () ->
  match encode t v with
  | Ok _ -> Test.fail "Encode did not error" ~__POS__
  | Error e ->
      match msg with None -> () | Some msg -> Test.styled_string msg e ~__POS__

let update ?__POS__:pos ?(format = Jsont.Minify) q j j' =
  let layout = format = Jsont.Layout in
  Test.block ?__POS__:pos @@ fun () ->
  match decode ~layout q j with
  | Error e -> Test.fail "%a" Fmt.lines e ~__POS__
  | Ok v when supports_layout () || not (format = Jsont.Layout) ->
      encode_ok ~format Jsont.json ~value:v j' ~__POS__
  | Ok v ->
      let j' =
        encode ~format:Jsont.Indent Jsont.json
          (decode Jsont.json j' |> Result.get_ok)
        |> Result.get_ok
      in
      encode_ok ~format:Jsont.Indent Jsont.json ~value:v j' ~__POS__

(* [trip t src] is the über testing combinator.

   It rounds trips a decode of [src] according to [t] and verifies
   that the generated JSON [trip] has the same data unless [lossy] is
   specified. If [value] is provided both decodes of [src] and [trip]
   are tested against [value]. If [format] is specified with
   [Jsont.Indent] or [Jsont.Layout] it assumes that [src] and [trip]
   must be equal *)

let trip
    ?(format = Jsont.Minify) ?(lossy = false) ?value ?(eq = Test.Eq.any)
    ?__POS__:pos t src
  =
  Test.block ?__POS__:pos @@ fun () ->
  let layout = format = Jsont.Layout in
  let v = decode ~layout t src |> Test.get_ok ~__POS__ in
  let trip = encode ~format t v |> Test.get_ok ~__POS__ in
  let v' = decode t trip |> Test.get_ok ~__POS__ in
  begin match value with
  | None -> Test.eq eq v v' ~__POS__;
  | Some value ->
      Test.eq eq v value ~__POS__;
      Test.eq eq v' value ~__POS__;
  end;
  if not lossy then begin
    let json = decode Jsont.json src |> Test.get_ok ~__POS__ in
    let trip = decode Jsont.json trip |> Test.get_ok ~__POS__ in
    Test.eq (module Jsont.Json) json trip ~__POS__
  end;
  if format <> Jsont.Minify then begin
    if format = Jsont.Layout && not (supports_layout ()) then () else
    (* Test that src is a representation of the requested encoding format *)
    Test.string src trip ~__POS__
  end

let eq : (module Test.Eq.T with type t = 'a)  = (module Jsont.Json)

(* Tests *)

let test_basic_invalid () =
  Test.test "basic invalid JSON" @@ fun () ->
  decode_error Jsont.json "" ~__POS__;
  decode_error (Jsont.null ()) "" ~__POS__;
  decode_error Jsont.bool "" ~__POS__;
  decode_error Jsont.json "ha" ~__POS__;
  decode_error (Jsont.null ()) "ha" ~__POS__;
  decode_error Jsont.bool "ha" ~__POS__;
  decode_error Jsont.json "  ha" ~__POS__;
  decode_error Jsont.json "  r6  " ~__POS__;
  decode_error Jsont.json " {  " ~__POS__;
  decode_error Jsont.json " [  " ~__POS__;
  decode_error Jsont.json " ][  " ~__POS__;
  ()

let test_indent () =
  Test.test "Encode with indentation" @@ fun () ->
  ()

let test_null () =
  Test.test "Jsont.null" @@ fun () ->
  trip ~eq ~format:Layout Jsont.json " null \r\n" ~__POS__;
  trip ~eq ~format:Layout Jsont.json "\n null " ~__POS__;
  trip ~eq ~format:Layout Jsont.json "null" ~__POS__;
  trip ~eq ~format:Indent Jsont.json "null" ~__POS__;
  decode_error Jsont.json " nu " ~__POS__;
  decode_error Jsont.json " nul " ~__POS__;
  decode_error Jsont.json " n " ~__POS__;
  trip (Jsont.null ()) " \n null \n " ~value:() ~__POS__;
  trip (Jsont.null ()) " null  " ~value:() ~__POS__;
  decode_error (Jsont.null ()) " true  " ~__POS__;
  ()

let test_bool () =
  Test.test "Jsont.bool" @@ fun () ->
  trip ~eq ~format:Layout Jsont.json " true \r\n" ~__POS__;
  trip ~eq ~format:Layout Jsont.json "\n false " ~__POS__;
  trip ~eq ~format:Layout Jsont.json "false" ~__POS__;
  trip ~eq ~format:Indent Jsont.json "true" ~__POS__;
  trip ~eq ~format:Indent Jsont.json "false" ~__POS__;
  decode_error Jsont.json " fals " ~__POS__;
  decode_error Jsont.json " falsee " ~__POS__;
  decode_error Jsont.json " f " ~__POS__;
  trip ~eq:Test.Eq.bool Jsont.bool " true \n " ~value:true ~__POS__;
  trip ~eq:Test.Eq.bool Jsont.bool " false  " ~value:false ~__POS__;
  decode_error Jsont.bool " fals  " ~__POS__;
  ()

let test_numbers () =
  Test.test "Jsont.number" @@ fun () ->
  trip ~eq ~format:Layout Jsont.json " 1 " ~__POS__;
  trip ~eq ~format:Layout Jsont.json " 0 \n " ~__POS__;
  trip ~eq ~format:Layout Jsont.json "\n 2.5 " ~__POS__;
  trip ~eq ~format:Indent Jsont.json "0";
  trip ~eq ~format:Indent Jsont.json "0.5";
  decode_error Jsont.json " 01 " ~__POS__;
  decode_error Jsont.json " -a " ~__POS__;
  decode_error Jsont.json " 1. " ~__POS__;
  decode_error Jsont.json " 1.0e+  " ~__POS__;
  decode_error Jsont.json " inf  " ~__POS__;
  decode_error Jsont.json " infinity  " ~__POS__;
  decode_error Jsont.json " nan  " ~__POS__;
  let eq = Test.Eq.float in
  trip ~eq Jsont.number " -0  " ~value:(-0.) ~__POS__;
  trip ~eq Jsont.number " 0  " ~value:(0.) ~__POS__;
  trip ~eq Jsont.number " 0E1  " ~value:0. ~__POS__;
  trip ~eq Jsont.number " 0e+1  " ~value:0. ~__POS__;
  trip ~eq Jsont.number " null  " ~value:Float.nan ~__POS__;
  encode_ok  Jsont.number "null" ~value:Float.infinity ~__POS__;
  encode_ok  Jsont.number "null" ~value:Float.neg_infinity ~__POS__;
  trip ~eq Jsont.number " 1e300  " ~value:1.e300 ~__POS__;
  decode_error Jsont.number " fals  " ~__POS__;
  decode_error Jsont.number " 1.  " ~__POS__;
  decode_error Jsont.number " 1.0e+  " ~__POS__;
  decode_error Jsont.number " 0E  " ~__POS__;
  decode_error Jsont.number " 1eE2  " ~__POS__;
  ()

let test_strings () =
  Test.test "Jsont.string" @@ fun () ->
  trip ~eq ~format:Layout Jsont.json {| "" |} ~__POS__;
  trip ~eq ~format:Layout Jsont.json " \"\\\"\" " ~__POS__;
  trip ~eq ~format:Layout Jsont.json " \"\\\\\" " ~__POS__;
  trip ~eq ~format:Layout Jsont.json " \"hihi\" \n  " ~__POS__;
  trip ~eq ~format:Layout Jsont.json " \"hi\\nhi\" \n  " ~__POS__;
  if Sys.backend_type <> Sys.Other "js_of_ocaml" then begin
    decode_error Jsont.json "\"\\uDC01\"" ~__POS__;
    decode_error Jsont.json "\"\\uDBFF\"" ~__POS__;
    decode_error Jsont.json "\"\\uDBFF\\uDBFF\"" ~__POS__;
  end;
  trip ~format:Indent Jsont.json {|""|};
  trip ~format:Indent Jsont.json {|"blablabla"|};
  decode_error Jsont.json "\"hi\nhi\"" ~__POS__;
  decode_error Jsont.json "\n \"abla\" hi " ~__POS__;
  decode_error Jsont.json "\n \"unclosed hi " ~__POS__;
  trip ~eq:Test.Eq.string
    Jsont.string "\"\\ud83D\\uDc2B\"" ~value:"🐫" ~__POS__;
  trip ~eq:Test.Eq.string Jsont.string "\"🐫 a\"" ~value:"🐫 a" ~__POS__;
  decode_error Jsont.string " false  " ~__POS__;
  decode_error Jsont.string "1.0" ~__POS__;
  ()

let test_option () =
  Test.test "Jsont.{none,some,option}" @@ fun () ->
  (* none *)
  decode_error Jsont.none "2" ~__POS__;
  decode_error Jsont.none "true" ~__POS__;
  trip Jsont.none "null" ~value:None ~__POS__;
  (* some *)
  decode_error Jsont.(some bool) "null" ~__POS__;
  decode_error Jsont.(some bool) "1.0" ~__POS__;
  trip Jsont.(some bool) "true" ~value:(Some true) ~__POS__;
  (* option *)
  decode_error Jsont.(option bool) "1.0" ~__POS__;
  decode_error Jsont.(option bool) "{}" ~__POS__;
  trip Jsont.(option bool) "true" ~value:(Some true) ~__POS__;
  trip Jsont.(option bool) "false" ~value:(Some false) ~__POS__;
  trip Jsont.(option bool) "null" ~value:None ~__POS__;
  ()

let test_ints () =
  Test.test "Jsont.{int…,uint…}" @@ fun () ->
  (* uint8 *)
  decode_error Jsont.uint8 "null" ~__POS__;
  decode_error Jsont.uint8 "true" ~__POS__;
  decode_error Jsont.uint8 "-1" ~__POS__;
  decode_error Jsont.uint8 "256" ~__POS__;
  trip Jsont.uint8 "0" ~value:0 ~__POS__;
  trip Jsont.uint8 "255" ~value:255 ~__POS__;
  (* uint16 *)
  decode_error Jsont.uint16 "null" ~__POS__;
  decode_error Jsont.uint16 "true" ~__POS__;
  decode_error Jsont.uint16 "-1" ~__POS__;
  decode_error Jsont.uint16 "65536" ~__POS__;
  trip Jsont.uint16 "0" ~value:0 ~__POS__;
  trip Jsont.uint16 "65535" ~value:65535 ~__POS__;
  (* int8 *)
  decode_error Jsont.int8 "null" ~__POS__;
  decode_error Jsont.int8 "true" ~__POS__;
  decode_error Jsont.int8 "-129" ~__POS__;
  decode_error Jsont.int8 "128" ~__POS__;
  trip Jsont.int8 "-128" ~value:(-128) ~__POS__;
  trip Jsont.int8 "127" ~value:127 ~__POS__;
  (* int32 *)
  decode_error Jsont.int32 "null" ~__POS__;
  decode_error Jsont.int32 "true" ~__POS__;
  decode_error Jsont.int32 "-2147483649" ~__POS__;
  decode_error Jsont.int32 "2147483648" ~__POS__;
  trip Jsont.int32 "-2147483648" ~value:Int32.min_int ~__POS__;
  trip Jsont.int32 "2147483647" ~value:Int32.max_int ~__POS__;
  (* int64 *)
  let max_exact = Int64.shift_left 1L 53 in
  let max_exact_next = Int64.(add max_exact 1L) in
  let min_exact = Int64.shift_left 1L 53 in
  let min_exact_prev = Int64.(add max_exact 1L) in
  decode_error Jsont.int64 "null" ~__POS__;
  decode_error Jsont.int64 "true" ~__POS__;
  trip Jsont.int64 (Fmt.str "%Ld" max_exact)  ~value:max_exact ~__POS__;
  trip Jsont.int64 (Fmt.str "%Ld" min_exact)  ~value:min_exact ~__POS__;
  trip Jsont.int64
    (Fmt.str {|"%Ld"|} max_exact_next)  ~value:max_exact_next ~__POS__;
  trip Jsont.int64
    (Fmt.str {|"%Ld"|} min_exact_prev)  ~value:min_exact_prev ~__POS__;
  (* int_as_string *)
  trip Jsont.int_as_string {|"2"|} ~value:2 ~__POS__;
  trip Jsont.int_as_string
    (Fmt.str {|"%d"|} Int.max_int) ~value:Int.max_int ~__POS__;
  trip Jsont.int_as_string
    (Fmt.str {|"%d"|} Int.min_int) ~value:Int.min_int ~__POS__;
  (* int64_as_string *)
  trip Jsont.int64_as_string
    (Fmt.str {|"%Ld"|} Int64.max_int) ~value:Int64.max_int ~__POS__;
  trip Jsont.int64_as_string
    (Fmt.str {|"%Ld"|} Int64.min_int) ~value:Int64.min_int ~__POS__;
  ()

let test_floats () =
  Test.test "Jsont.{any_float,float_as_hex_string}" @@ fun () ->
  (* any_float *)
  let jsonstr f = Fmt.str {|"%s"|} (Float.to_string f) in
  let eq = Test.Eq.float in
  decode_ok ~eq Jsont.any_float "null" ~value:Float.nan ~__POS__;
  trip ~eq Jsont.any_float " -0  " ~value:(-0.) ~__POS__;
  trip ~eq Jsont.any_float " 0  " ~value:(0.) ~__POS__;
  trip ~eq Jsont.any_float " 0.5  " ~value:0.5 ~__POS__;
  decode_ok ~eq Jsont.any_float (jsonstr 0.5) ~value:0.5 ~__POS__;
  trip ~eq Jsont.any_float
    (jsonstr Float.nan) ~value:Float.nan ~__POS__;
  trip ~eq Jsont.any_float
    (jsonstr Float.infinity) ~value:Float.infinity ~__POS__;
  trip ~eq Jsont.any_float
    (jsonstr Float.neg_infinity) ~value:Float.neg_infinity ~__POS__;

  (* float_as_hex_string *)
  let jsonstr f = Fmt.str {|"%h"|} f in
  let t = Jsont.float_as_hex_string in
  decode_error t "null" ~__POS__;
  decode_error t "1.0" ~__POS__;
  trip ~eq t (jsonstr 0.5) ~value:0.5 ~__POS__;
  trip ~eq t (jsonstr Float.nan) ~value:Float.nan ~__POS__;
  trip ~eq t (jsonstr Float.infinity) ~value:Float.infinity ~__POS__;
  trip ~eq t (jsonstr Float.neg_infinity) ~value:Float.neg_infinity ~__POS__;
  ()

let test_enum_and_binary_string () =
  Test.test "Jsont.{of_of_string,enum,binary_string}" @@ fun () ->
  (* of_string *)
  let int_of_string s = match int_of_string_opt s with
  | None -> Error "Not an integer" | Some i -> Ok i
  in
  let t = Jsont.of_of_string ~kind:"int" int_of_string ~enc:Int.to_string in
  trip ~eq:(Test.Eq.int) t {|"1"|} ~value:1 ~__POS__;
  decode_error t {|"bla"|} ~__POS__;
  (* enum *)
  let enum = Jsont.enum ~kind:"heyho" ["hey", `Hey; "ho", `Ho ] in
  decode_error enum {|null|} ~__POS__;
  decode_error enum {|"ha"|} ~__POS__;
  decode_error enum {|"farfarfar"|} ~__POS__;
  trip enum {|"hey"|} ~value:`Hey ~__POS__;
  trip enum {|"ho"|} ~value:`Ho ~__POS__;
  (* binary_string *)
  decode_error Jsont.binary_string {|null|};
  decode_error Jsont.binary_string {|"00gabb"|} ~__POS__;
  decode_error Jsont.binary_string {|"00aab"|} ~__POS__;
  trip Jsont.binary_string {|"00a1bb"|} ~__POS__;
  trip Jsont.binary_string {|"00a1ff"|} ~value:"\x00\xa1\xff" ~__POS__;
  ()

let test_arrays () =
  Test.test "Jsont.{list,array,bigarray,t2,t3,t4,tn}" @@ fun () ->
  let barr arr = Bigarray.Array1.of_array Int C_layout arr in
  trip ~eq ~format:Layout Jsont.json " [] \n" ~__POS__;
  trip ~eq ~format:Layout Jsont.json " [1, 3] \n\n" ~__POS__;
  trip ~eq ~format:Layout Jsont.json " [1\n,3] \n\n" ~__POS__;
  trip ~eq ~format:Layout Jsont.json " [1\n, \"a\",\n3 ] \n\n" ~__POS__;
  trip ~eq ~format:Indent Jsont.json "[]" ~__POS__;
  trip ~eq ~format:Indent Jsont.json "[\n  1\n]" ~__POS__;
  trip ~eq ~format:Indent Jsont.json "[\n  1,\n  \"bla\",\n  2\n]" ~__POS__;
  decode_error Jsont.json "[1 ~__POS__;3]" ~__POS__;
  decode_error Jsont.json " [1,3 " ~__POS__;
  decode_error (Jsont.(list number)) "[1,true,3]" ~__POS__;
  trip Jsont.(list int) " [ ] \n" ~value:[] ~__POS__;
  trip Jsont.(list int) "[1,2,3]" ~value:[1;2;3] ~__POS__;
  trip Jsont.(array int) " [ ] \n" ~value:[||] ~__POS__;
  trip Jsont.(array int) "[1,2,3]" ~value:[|1;2;3|] ~__POS__;
  trip Jsont.(bigarray Int int) " [ ] \n" ~value:(barr [||]) ~__POS__;
  trip Jsont.(bigarray Int int) " [1,2,3] \n" ~value:(barr [|1;2;3;|]) ~__POS__;
  let enc = Array.get in
  let t2_int = Jsont.t2 ~dec:(fun x y -> [|x;y|]) ~enc Jsont.int in
  decode_error t2_int "[]" ~__POS__;
  decode_error t2_int "[1]" ~__POS__;
  trip t2_int "[1,2]" ~value:[|1;2|] ~__POS__;
  decode_error t2_int "[1,2,3]" ~__POS__;
  let t3_int = Jsont.t3 ~dec:(fun x y z -> [|x;y;z|]) ~enc Jsont.int in
  decode_error t3_int "[]" ~__POS__;
  decode_error t3_int "[1]" ~__POS__;
  decode_error t3_int "[1,2]" ~__POS__;
  trip t3_int "[1,2,3]" ~value:[|1;2;3|] ~__POS__;
  decode_error t3_int "[1,2,3,4]" ~__POS__;
  let t4_int = Jsont.t4 ~dec:(fun x y z w -> [|x;y;z;w|]) ~enc Jsont.int in
  decode_error t4_int "[]" ~__POS__;
  decode_error t4_int "[1]" ~__POS__;
  decode_error t4_int "[1,2]" ~__POS__;
  decode_error t4_int "[1,2,3]" ~__POS__;
  trip t4_int "[1,2,3,4]" ~value:[|1;2;3;4|] ~__POS__;
  decode_error t4_int "[1,2,3,4,5]" ~__POS__;
  let t0_int = Jsont.(tn ~n:0 int) in
  let t2_int = Jsont.(tn ~n:2 int) in
  trip t0_int "[]" ~value:[||] ~__POS__;
  decode_error t0_int "[1]" ~__POS__;
  decode_error t0_int "[1;2]" ~__POS__;
  decode_error t2_int "[]" ~__POS__;
  decode_error t2_int "[1]" ~__POS__;
  trip t2_int "[1,2]" ~value:[|1;2|] ~__POS__;
  decode_error t2_int "[1,2,3]" ~__POS__;
  ()

let test_objects () =
  Test.test "Jsont.Obj.map" @@ fun () ->
  trip ~eq ~format:Layout Jsont.json " {} \n" ~__POS__;
  trip ~eq ~format:Layout Jsont.json {| {"a": 1}  |} ~__POS__;
  trip ~eq ~format:Layout Jsont.json {| {"a": 1,  "b":2}  |} ~__POS__;
  trip ~eq ~format:Indent Jsont.json "{}" ~__POS__;
  trip ~eq ~format:Indent Jsont.json "{\n  \"bla\": 1\n}";
  trip ~format:Indent Item.jsont Item_data.i0_json ~value:Item_data.i0 ~__POS__;
  trip ~format:Indent Item.jsont Item_data.i1_json ~value:Item_data.i1 ~__POS__;
  ()

let test_unknown_mems () =
  Test.test "Jsont.Obj.*_unknown" @@ fun () ->
  (* Skip unknowns *)
  trip Unknown.skip_jsont Unknown_data.u0 ~__POS__;
  trip ~lossy:true Unknown.skip_jsont Unknown_data.u1 ~__POS__;
  trip ~lossy:true Unknown.skip_jsont Unknown_data.u2 ~__POS__;
  (* Error on unknown *)
  trip Unknown.error_jsont Unknown_data.u0 ~__POS__;
  decode_error Unknown.error_jsont Unknown_data.u1 ~__POS__;
  decode_error Unknown.error_jsont Unknown_data.u2 ~__POS__;
  (* Keep unknowns *)
  trip Unknown.keep_jsont Unknown_data.u0 ~__POS__;
  trip Unknown.keep_jsont Unknown_data.u1 ~__POS__;
  trip Unknown.keep_jsont Unknown_data.u2 ~__POS__;
  ()

let test_cases () =
  Test.test "Jsont.Obj.Case" @@ fun () ->
  decode_error Cases.Person_top.jsont Cases_data.invalid_miss ~__POS__;
  decode_error Cases.Person_top.jsont Cases_data.invalid_case ~__POS__;
  decode_error Cases.Person_field.jsont Cases_data.invalid_miss ~__POS__;
  decode_error Cases.Person_field.jsont Cases_data.invalid_case ~__POS__;
  trip Cases.Person_top.jsont Cases_data.author0
    ~value:Cases_data.author0_top ~__POS__;
  trip Cases.Person_top.jsont Cases_data.author0'
    ~value:Cases_data.author0_top ~__POS__;
  trip Cases.Person_top.jsont Cases_data.editor0
    ~value:Cases_data.editor0_top ~__POS__;
  trip Cases.Person_top.jsont Cases_data.editor0'
    ~value:Cases_data.editor0_top ~__POS__;
  trip Cases.Person_field.jsont Cases_data.author0
    ~value:Cases_data.author0_field ~__POS__;
  trip Cases.Person_field.jsont Cases_data.author0'
    ~value:Cases_data.author0_field ~__POS__;
  trip Cases.Person_field.jsont Cases_data.editor0
    ~value:Cases_data.editor0_field ~__POS__;
  trip Cases.Person_field.jsont Cases_data.editor0'
    ~value:Cases_data.editor0_field ~__POS__;
  (* Unknown value override *)
  trip Cases.Keep_unknown.jsont ~eq:(module Cases.Keep_unknown)
    Cases_data.unknown_a ~value:Cases_data.unknown_a_value ~__POS__;
  trip Cases.Keep_unknown.jsont ~eq:(module Cases.Keep_unknown)
    Cases_data.unknown_b ~value:Cases_data.unknown_b_value ~__POS__;
  let module M = struct
    type t = string String_map.t
    let equal = String_map.equal String.equal
    let pp ppf v = Fmt.string ppf "<value>"
  end
  in
  trip Cases.Keep_unknown.a_jsont ~eq:(module M)
    Cases_data.unknown_a ~value:Cases_data.unknown_a_a_value ~__POS__;
  encode_ok Cases.Keep_unknown.jsont
    ~format:Indent ~value:Cases_data.unknown_a_no_a_unknown_value
    Cases_data.unknown_a_no_a_unknown;
  ()

let test_rec () =
  Test.test "Jsont.rec" @@ fun () ->
  let tree_null = Tree.jsont_with_null Jsont.int in
  trip tree_null Tree_data.empty_null ~value:Tree_data.empty ~__POS__;
  trip tree_null Tree_data.tree0_null ~value:Tree_data.tree0 ~__POS__;
  let tree_cases = Tree.jsont_with_cases Jsont.int in
  trip tree_cases Tree_data.empty_cases ~value:Tree_data.empty ~__POS__;
  trip tree_cases Tree_data.tree0_cases ~value:Tree_data.tree0 ~__POS__;
  ()

let test_const () =
  Test.test "Jsont.const" @@ fun () ->
  trip ~lossy:true Jsont.(const int 4) " {} " ~value:4 ~__POS__;
  trip ~lossy:true Jsont.(const bool true) ~value:true "false" ~__POS__;
  ()

let recode_int_to_string = Jsont.(recode ~dec:int string_of_int ~enc:string)

let test_array_queries () =
  let a = "[1,[ 1, 2], 3] " in
  Test.test "Jsont.{nth,*_nth,filter_map_array,fold_array}" @@
  fun () ->
  (* Jsont.nth *)
  decode_ok Jsont.(nth 0 @@ int) a ~value:1 ~__POS__;
  decode_ok Jsont.(nth 1 @@ nth 1 int) a ~value:2 ~__POS__;
  decode_ok Jsont.(nth 1 @@ list int) a ~value:[1;2] ~__POS__;
  decode_error Jsont.(nth 3 @@ int) a ~__POS__;
  decode_ok Jsont.(nth ~absent:3 3 @@ int) ~value:3 a ~__POS__;
  decode_ok Jsont.(nth 0 @@ int) ~value:1 a ~__POS__;
  decode_ok Jsont.(nth 1 @@ nth 1 int) a ~value:2 ~__POS__;
  decode_ok Jsont.(nth 1 @@ list int) a ~value:[1;2] ~__POS__;
  (* Jsont.{set,update}_nth} *)
  update ~format:Jsont.Layout
    Jsont.(update_nth 1 @@ update_nth 1 Jsont.(const int 4))
    a "[1,[ 1, 4], 3] " ~__POS__;
  update ~format:Jsont.Layout Jsont.(update_nth 1 @@ set_nth int 0 2) a
    "[1,[ 2, 2], 3] " ~__POS__;
  decode_error Jsont.(update_nth 1 @@ set_nth int 2 3) a;
  decode_error Jsont.(update_nth 3 int) a;
  update ~format:Jsont.Layout Jsont.(update_nth 3 ~absent:5 int) a
    "[1,[ 1, 2], 3,5] ";
  update ~format:Jsont.Layout
    Jsont.(update_nth 1 @@ set_nth ~allow_absent:true int 3 3) a
    "[1,[ 1, 2,0,3], 3] " ~__POS__;
  update ~format:Jsont.Layout
    Jsont.(update_nth 1 @@ set_nth
             ~stub:(Jsont.Json.null ()) ~allow_absent:true int 3 3) a
    "[1,[ 1, 2,null,3], 3] " ~__POS__;
  update ~format:Jsont.Layout
    Jsont.(update_nth 1 @@ update_nth 1 recode_int_to_string) a
    "[1,[ 1, \"2\"], 3] " ~__POS__;
  update Jsont.(update_nth 1 @@ delete_nth 0) a "[1,[2],3]" ~__POS__;
  decode_ok
    Jsont.(nth 1 @@ fold_array int (fun i v acc -> (i, v) :: acc) [])
    a ~value:[(1,2); (0,1)] ~__POS__;
  update Jsont.(update_nth 1 @@ filter_map_array int int
                  (fun _ v -> if v mod 2 = 0 then None else Some (v - 1)))
    a "[1,[0],3]" ~__POS__;
  (* Jsont.delete_nth *)
  update ~format:Jsont.Layout Jsont.(delete_nth 1) a "[1, 3] " ~__POS__;
  decode_error Jsont.(delete_nth 3) a ~__POS__;
  update ~format:Jsont.Layout Jsont.(delete_nth ~allow_absent:true 3) a a
    ~__POS__;
  (* Jsont.filter_map_array *)
  update ~format:Jsont.Layout
    Jsont.(filter_map_array Jsont.json Jsont.json
             (fun i v -> if i = 1 then None else Some v)) a
    "[1, 3] " ~__POS__;
  (* Jsont.fold_array *)
  decode_ok Jsont.(nth 1 @@ fold_array int (fun i v acc -> i + v + acc) 0) a
    ~value:4 ~__POS__;
  ()

let test_object_queries () =
  Test.test "Jsont.{mem,*_mem,fold_object,filter_map_object}" @@ fun () ->
  let o = {| { "a" : { "b" : 1 }, "c": 2 } |} in
  (* Jsont.mem *)
  decode_ok Jsont.(mem "a" @@ mem "b" int) o ~value:1 ~__POS__;
  decode_error Jsont.(mem "a" @@ mem "c" int) o ~__POS__;
  decode_ok Jsont.(mem "a" @@ mem ~absent:3 "c" int) o ~value:3 ~__POS__;
  (* Jsont.{update,set}_mem *)
  update ~format:Jsont.Layout
    Jsont.(update_mem "a" @@ update_mem "b" (const int 3))
    o {| { "a" : { "b" : 3 }, "c": 2 } |} ~__POS__;
  update ~format:Jsont.Layout
    Jsont.(update_mem "a" @@ update_mem "b" recode_int_to_string)
    o {| { "a" : { "b" : "1" }, "c": 2 } |} ~__POS__;
  decode_error
    Jsont.(update_mem "a" @@ update_mem "c" (const int 4)) o ~__POS__;
  update ~format:Jsont.Layout
    Jsont.(update_mem "a" @@ update_mem "c" ~absent:4 (const int 5)) o
    {| { "a" : { "b" : 1 ,"c":5}, "c": 2 } |} ~__POS__;
  update ~format:Jsont.Layout
    Jsont.(set_mem int "a" 2) o
    {| { "a" : 2, "c": 2 } |} ~__POS__;
  decode_error Jsont.(set_mem int "d" 2) o ~__POS__;
  update ~format:Jsont.Layout Jsont.(set_mem ~allow_absent:true int "d" 3) o
    {| { "a" : { "b" : 1 }, "c": 2 ,"d":3} |} ~__POS__;
  (* Jsont.delete_mem *)
  decode_error Jsont.(update_mem "a" @@ delete_mem "c") o ~__POS__;
  update ~format:Jsont.Layout
    Jsont.(update_mem "a" @@ delete_mem ~allow_absent:true "c")
    o o ~__POS__;
  update ~format:Jsont.Layout Jsont.(update_mem "a" @@ delete_mem "b")
    o {| { "a" : {}, "c": 2 } |} ~__POS__;
  update ~format:Jsont.Layout Jsont.(delete_mem "a")
    o {| { "c": 2 } |} ~__POS__;
  (* Jsont.filter_map_object *)
  update ~format:Jsont.Layout
    Jsont.(filter_map_object Jsont.json Jsont.json
             (fun m n v -> if n = "a" then None else Some ((n, m), v)))
    o {| { "c": 2 } |} ~__POS__;
  (* Jsont.fold *)
  decode_ok Jsont.(mem "a" @@
                   fold_object int (fun _ n i acc -> i + acc) 2)
    o ~value:3 ~__POS__;
  ()

let test_path_queries () =
  Test.test "Jsont.{path,*_path}" @@ fun () ->
  let v = {| [ 0, { "a": 1}, 2 ] |} in
  (* Jsont.path *)
  decode_error Jsont.(path Path.root int) v ~__POS__;
  update ~format:Jsont.Layout Jsont.(path Path.root Jsont.json) v v ~__POS__;
  decode_ok Jsont.(path Path.(root |> nth 1 |> mem "a") int) v ~value:1;
  decode_ok Jsont.(path Path.(root |> nth 1 |> mem "b") ~absent:2 int) v
    ~value:2 ~__POS__;
  (* Jsont.{set,update}_path} *)
  update ~format:Jsont.Layout Jsont.(set_path int Path.root 2)
    v  {|2|} ~__POS__;
  update ~format:Jsont.Layout
    Jsont.(set_path string Path.(root |> nth 1 |> mem "a") "hey")
    v {| [ 0, { "a": "hey"}, 2 ] |} ~__POS__;
  update ~format:Jsont.Layout
    Jsont.(set_path ~allow_absent:true
             string Path.(root |> nth 1 |> mem "b") "hey")
    v {| [ 0, { "a": 1,"b":"hey"}, 2 ] |} ~__POS__;
  update ~format:Jsont.Layout
    Jsont.(update_path Path.(root |> nth 1 |> mem "a")
             (map int ~dec:succ ~enc:Fun.id))
    v {| [ 0, { "a": 2}, 2 ] |} ~__POS__;
  (* Jsont.delete_path *)
  update ~format:Jsont.Layout
    Jsont.(delete_path Path.(root |> nth 1 |> mem "a")) v
    {| [ 0, {}, 2 ] |} ~__POS__;
  update ~format:Jsont.Layout
    Jsont.(delete_path Path.(root |> nth 1)) v
    {| [ 0, 2 ] |} ~__POS__;
  update ~format:Jsont.Layout
    Jsont.(delete_path Path.root) v
    {|null|} ~__POS__;
  decode_error Jsont.(delete_path Path.(root |> nth 1 |> mem "b")) v ~__POS__;
  update ~format:Jsont.Layout
    Jsont.(delete_path ~allow_absent:true Path.(root |> nth 1 |> mem "b"))
    v v ~__POS__;
  ()

let tests () =
  test_basic_invalid ();
  test_null ();
  test_bool ();
  test_numbers ();
  test_strings ();
  test_option ();
  test_ints ();
  test_floats ();
  test_enum_and_binary_string ();
  test_arrays ();
  test_objects ();
  test_unknown_mems ();
  test_cases ();
  test_rec ();
  test_const ();
  test_array_queries ();
  test_object_queries ();
  test_path_queries ();
  ()
