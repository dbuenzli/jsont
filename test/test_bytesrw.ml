(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_testing
open Bytesrw

(* Tests the common test suite with the Jsont_bytesrw codec. *)

let decode ?layout t json =
  Jsont_bytesrw.decode_string ?layout ~locs:true t json

let encode ?format t v = Jsont_bytesrw.encode_string ?format t v
let test_funs = { Test_common.supports_layout = true; decode; encode }

(* Other tests *)

let test_eod =
  Test.test "Jsont_bytesrw.encode ~eod" @@ fun () ->
  let b = Buffer.create 255 in
  let w = Bytes.Writer.of_buffer b in
  let () = Result.get_ok (Jsont_bytesrw.encode' Jsont.bool true ~eod:false w) in
  let () = Result.get_ok (Jsont_bytesrw.encode' Jsont.bool true ~eod:true w) in
  Test.string (Buffer.contents b) "truetrue";
  Snap.raise (fun () -> Jsont_bytesrw.encode' Jsont.bool true ~eod:true w) @@
  __POS_OF__ (Invalid_argument("slice written after eod"));
  ()

let main () =
  Test.main @@ fun () ->
  Test_common.test_funs := test_funs;
  Test.autorun ();
  ()

let () = if !Sys.interactive then () else exit (main ())
