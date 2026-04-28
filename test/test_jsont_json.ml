(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_testing

(* Tests the common test suite with the Jsont.Json codec. *)

(* Since the Jsont.Json codec works only on Jsont.json values we use
   Jsont_bytesrw to codec JSON to Jsont.json values and then apply the
   Jsont.Json codec. So the tests rely on a working Jsont_bytesrw
   codec *)

let decode ?layout t json =
  match Jsont_bytesrw.decode_string ?layout ~locs:true Jsont.json json with
  | Error _ as e -> e
  | Ok json -> Jsont.Json.decode t json

let encode ?format t v =
  match Jsont.Json.encode t v with
  | Error _ as e -> e
  | Ok json -> Jsont_bytesrw.encode_string ?format Jsont.json json

let test_funs = { Test_common.supports_layout = true; decode; encode }

let main () =
  Test.main @@ fun () ->
  Test_common.test_funs := test_funs;
  Test_common.tests ();
  ()

let () = if !Sys.interactive then () else exit (main ())
