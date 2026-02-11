(*---------------------------------------------------------------------------
   Copyright (c) 2026 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax
open B0_testing

let args = Test.Arg.make ()

let src_in ~cwd src = Fpath.drop_strict_prefix ~prefix:cwd src |> Option.get
let snap_stdout ~cwd cmd ~ext src =
  let cmd = Cmd.(cmd %% path (src_in ~cwd src)) in
  Snap.stdout ~cwd ~trim:false cmd !@ Fpath.(src -+ ext) ~__POS__

let test_textlocs =
  Test.test' args "locs" @@ fun (finit, cwd, (_i, valid_srcs)) ->
  let cmd = Cmd.(finit % "locs") and ext = ".locs" in
  List.iter (snap_stdout ~cwd cmd ~ext) valid_srcs;
  ()

let test_pretty =
  Test.test' args "fmt -fpretty" @@ fun (finit, cwd, (_i, valid_srcs)) ->
  let cmd = Cmd.(finit % "fmt" % "-fpretty") and ext = ".pretty.json" in
  List.iter (snap_stdout ~cwd cmd ~ext) valid_srcs;
  ()

let test_indent =
  Test.test' args "fmt -findent" @@ fun (finit, cwd, (_i, valid_srcs)) ->
  let cmd = Cmd.(finit % "fmt" % "-findent") and ext = ".indent.json" in
  List.iter (snap_stdout ~cwd cmd ~ext) valid_srcs;
  ()

let test_minify =
  Test.test' args "fmt -fminify" @@ fun (finit, cwd, (_i, valid_srcs)) ->
  let cmd = Cmd.(finit % "fmt" % "-fminify") and ext = ".minify.json" in
  List.iter (snap_stdout ~cwd cmd ~ext) valid_srcs;
  ()

let test_preserve =
  Test.test' args "fmt -fpreserve" @@ fun (finit, cwd, (_i, valid_srcs)) ->
  let cmd = Cmd.(finit % "fmt" % "-fpreserve") and ext = ".layout.json" in
  List.iter (snap_stdout ~cwd cmd ~ext) valid_srcs;
  ()

let test_invalid =
  Test.test' args "fmt invalid JSON" @@ fun (finit, cwd, (invalid_srcs, _v)) ->
  let snap src =
    let cmd = Cmd.(finit % "fmt" %% path (src_in ~cwd src)) in
    Snap.run ~cwd cmd !@ Fpath.(src -+ ".run") ~__POS__
  in
  List.iter snap invalid_srcs

(* Try to streamline that in B0_testing *)

let get_jsont_cmd () =
  let var = "B0_TESTING_JSONT" in
  match Os.Env.var ~empty_is_none:true var with
  | None -> Fmt.error "%s unspecified, needs to point to jsont executable" var
  | Some cmd -> Ok (Cmd.tool cmd)

let get_srcs dir =
  let* files =
    let dotfiles = false and follow_symlinks = true and recurse = true in
    Os.Dir.contents ~kind:`Files ~dotfiles ~follow_symlinks ~recurse dir
  in
  let is_json f = Fpath.take_ext ~multi:true f = ".json" in
  let is_invalid f = String.starts_with ~prefix:"invalid" (Fpath.basename f) in
  Ok (List.partition is_invalid (List.filter is_json files))

let main () =
  Test.main @@ fun () ->
  Test.error_to_failstop @@
  let* cmd = get_jsont_cmd () in
  let snapshot_dir = Fpath.(Test.dir () / "snapshots") in
  let* srcs = get_srcs snapshot_dir in
  let args = Test.Arg.[value args (cmd, snapshot_dir, srcs)] in
  Ok (Test.autorun ~args ())

let () = if !Sys.interactive then () else exit (main ())
