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

let test ~show_errors file =
  let name = Fpath.basename file in
  Test.test name @@ fun () ->
  Test.noraise ~__POS__ @@ fun () ->
  Result.get_ok' @@
  let* json = Os.File.read file in
  let status = status_of_filename name in
  let file = Fpath.to_string file in
  match Jsont_bytesrw.decode_string ~file ~locs:true Jsont.json json with
  | Ok _ ->
      if status = `Accept || status = `Indeterminate
      then Ok ()
      else (Test.failstop " @[<v>Should have been rejected:@,%s@]" json)
  | Error e ->
      if show_errors then Log.err (fun m -> m "%s" e);
      if status = `Reject || status = `Indeterminate
      then Ok ()
      else (Test.failstop " @[<v>Should have been accepted:@,%s@]" json)

let run ~dir ~show_errors =
  let dir = Fpath.v dir in
  Log.if_error ~use:1 @@
  let* exists = Os.Dir.exists dir in
  if not exists
  then begin
    Fmt.pr "@[%a @[<v>JSONTestSuite not found@,\
            Use %a to download it@]@]" Test.Fmt.skip ()
      Fmt.code "b0 -- download-seriot-suite";
    Ok 0
  end else
  let dir = Fpath.(dir / "test_parsing") in
  let* files = Os.Dir.fold_files ~recurse:false Os.Dir.path_list dir [] in
  Result.ok @@ Test.main @@ fun () ->
  List.iter (fun file -> test ~show_errors file ()) files

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  let doc = "Run Nicolas Seriot's JSON test suite" in
  Cmd.v (Cmd.info "test_seriot_suite" ~doc) @@
  let+ show_errors =
    let doc = "Show errors" in
    Arg.(value & flag & info ["e"; "show-errors"] ~doc)
  and+ dir =
    let doc = "Repository directory of the test suite." in
    Arg.(value & pos 0 dir "tmp/JSONTestSuite" & info [] ~doc ~docv:"REPO")
  in
  run ~dir ~show_errors

let main () = Cmd.eval' cmd
let () = if !Sys.interactive then () else exit (main ())
