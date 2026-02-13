(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Runs the codec on https://github.com/nst/JSONTestSuite *)

open B0_std
open B0_testing
open Result.Syntax

let status_of_filename name =
  if String.starts_with ~prefix:"y_" name then `Accept else
  if String.starts_with ~prefix:"n_" name then `Reject else
  if String.starts_with ~prefix:"i_" name then `Indeterminate else
  Test.failstop "Unknown kind of test: %s" name

let args = Test.Arg.make ()

let test_file ~show_errors file =
  Test.error_to_fail @@
  let* json = Os.File.read file in
  let name = Fpath.basename file in
  let status = status_of_filename name in
  let file = Fpath.to_string file in
  match Jsont_bytesrw.decode_string ~file ~locs:true Jsont.json json with
  | Ok _ ->
      if status = `Accept || status = `Indeterminate then Ok (Test.pass ()) else
      Fmt.error "@[<v>Test %s@,Should have been rejected:@,%s@]" name json
  | Error e ->
      if show_errors then Test.Log.msg "@[<v>Test %s@,%s@]" name e;
      if status = `Reject || status = `Indeterminate then Ok (Test.pass ()) else
      Fmt.error "@[<v>Test %s@,Should have been accepted:@,%s@]" name json

let test =
  Test.test' args "test_parsing tests" @@ fun (show_errors, test_files) ->
  Test.block ~kind:"test file" @@ fun () ->
  List.iter (test_file ~show_errors) test_files

let get_test_files dir =
  let* exists = Os.Dir.exists dir in
  if not exists then
    Fmt.error
      "@[%a @[<v>JSONTestSuite not found@,Use %a to download it@]@]"
      Test.Fmt.skip () Fmt.code "b0 -- download-seriot-suite"
  else
  let dir = Fpath.(dir / "test_parsing") in
  let dotfiles = false and follow_symlinks = true and recurse = false in
  Os.Dir.contents ~kind:`Files ~dotfiles ~follow_symlinks ~recurse dir

open Cmdliner
open Cmdliner.Term.Syntax

let main () =
  Test.main' @@
  let+ show_errors =
    let doc = "Show errors" in
    Arg.(value & flag & info ["e"; "show-errors"] ~doc)
  and+ dir =
    let doc = "Repository directory of the test suite." in
    let default = Fpath.v "../tmp/JSONTestSuite" in
    Arg.(value & opt B0_std_cli.dirpath default & info ["repo-dir"] ~doc)
  in
  fun () ->
    let dir = Fpath.(Test.dir () // dir) in
    let files = get_test_files dir |> Test.error_to_failstop in
    Test.autorun ~args:Test.Arg.[value args (show_errors, files)] ()

let () = if !Sys.interactive then () else exit (main ())
