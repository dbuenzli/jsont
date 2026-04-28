(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Brr
open B0_testing

(* Tests the common test suite with the Jsont_brr codec. *)

let error_to_string e = Jstr.to_string (Jv.Error.message e)

let decode ?layout t json =
  Result.map_error error_to_string @@ Jsont_brr.decode t (Jstr.v json)

let encode ?format t v = match Jsont_brr.encode ?format t v with
| Ok v -> Ok (Jstr.to_string v) | Error e -> Error (error_to_string e)

let test_funs = { Test_common.supports_layout = false; decode; encode }

let main () =
  let exit = Test.main @@ fun () ->
    Test_common.test_funs := test_funs;
    Test_common.tests ();
  in
  let result = if exit = 0 then "All tests passed!" else "Some tests FAILED!" in
  let children =
    [ El.h1 [ El.txt' "Jsont_brr tests" ];
      El.p [ El.txt' result];
      El.p [ El.txt' "Open the browser console for details."] ]
  in
  El.set_children (Document.body G.document) children

let () = if !Sys.interactive then () else main ()
