(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_testing

(* Tests the common test suite with the Jsont_bytesrw codec. *)

let decode ?layout t json =
  Jsont_bytesrw.decode_string ?layout ~locs:true t json

let encode ?format t v = Jsont_bytesrw.encode_string ?format t v
let test_funs = { Test_common.supports_layout = true; decode; encode }

let main () =
  Test.main @@ fun () ->
  Test_common.test_funs := test_funs;
  Test_common.tests ();
  ()

let () = if !Sys.interactive then () else exit (main ())
