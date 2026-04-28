(*---------------------------------------------------------------------------
   Copyright (c) 2026 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing

let () = Jsont.Error.disable_ansi_styler ()

let decode t s = Jsont_bytesrw.decode_string t s
let encode t v = Jsont_bytesrw.encode_string t v

let test_uint8 =
  Test.test "Jsont.uint8" @@ fun () ->
  let dec s = Snap.(result T.int) (decode Jsont.uint8 s) in
  let enc v = Snap.(result T.string) (encode Jsont.uint8 v) in
  dec "0" @> __POS_OF__ (Ok 0);
  dec "1" @> __POS_OF__ (Ok 1);
  dec "255" @> __POS_OF__ (Ok 255);
  dec "-1" @> __POS_OF__
    (Error "Number -1 not in uint8 range\nFile \"-\":");
  dec "256" @> __POS_OF__
    (Error "Number 256 not in uint8 range\nFile \"-\":");
  dec "128.5" @> __POS_OF__ (Ok 128);
  dec "null" @> __POS_OF__
    (Error "Expected uint8 number but found null\nFile \"-\":");
  enc 0 @> __POS_OF__ (Ok "0");
  enc 1 @> __POS_OF__ (Ok "1");
  enc 255 @> __POS_OF__ (Ok "255");
  enc (-1) @> __POS_OF__ (Error "Integer -1 not in uint8 range");
  enc 256 @> __POS_OF__ (Error "Integer 256 not in uint8 range");
  ()

let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
