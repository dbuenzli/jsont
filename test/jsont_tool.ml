(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let exit_err_file = 1
let exit_err_json = 2
let exit_err_diff = 3

let diff src fmted =
  Result.join @@ Os.Dir.with_tmp @@ fun cwd ->
  let null = Fpath.(to_string null) in
  let* env =
    let* env = Os.Env.current () in
    let env = Os.Env.add "GIT_CONFIG_SYSTEM" null env in
    let env = Os.Env.add "GIT_CONFIG_GLOBAL" null env in
    Ok (Os.Env.to_assignments env)
  in
  let* git = Os.Cmd.get (Cmd.tool "git") in
  let src_file = Fpath.v "src" and fmted_file = Fpath.v "fmt" in
  let cmd =
    Cmd.(git % "diff" % "--ws-error-highlight=all" % "--no-index" %
         "--patience" %% path src_file %% path fmted_file)
  in
  let force = false and make_path = true in
  let* () = Os.File.write ~force ~make_path Fpath.(cwd // src_file) src in
  let* () = Os.File.write ~force ~make_path Fpath.(cwd // fmted_file) fmted in
  let* status = Os.Cmd.run_status ~env ~cwd cmd in
  match status with
  | `Exited n -> Ok n
  | `Signaled _ -> Fmt.error "%a" Os.Cmd.pp_cmd_status (cmd, status)

let with_infile file f = (* XXX add something to bytesrw. *)
  let process file ic = try Ok (f (Bytesrw.Bytes.Reader.of_in_channel ic)) with
  | Sys_error e -> Error (Format.sprintf "@[<v>%s:@,%s@]" file e)
  in
  try match file with
  | "-" -> process file In_channel.stdin
  | file -> In_channel.with_open_bin file (process file)
  with Sys_error e -> Error e

let output ~format ~number_format j = match format with
| `Pretty -> Ok (Format.printf "@[%a@]@." (Jsont.pp_json' ~number_format ()) j)
| `Format format ->
    let w = Bytesrw.Bytes.Writer.of_out_channel stdout in
    Jsont_bytesrw.encode ~format ~number_format ~eod:true Jsont.json j w

let output_string ~format ~number_format j = match format with
| `Pretty -> Ok (Fmt.str "@[%a@]" (Jsont.pp_json' ~number_format ()) j)
| `Format format ->
    Jsont_bytesrw.encode_string ~format ~number_format Jsont.json j

let trip_type
    ?(dec_only = false) ~file ~format ~number_format ~diff:do_diff ~locs t
  =
  Log.if_error ~use:exit_err_file @@
  with_infile file @@ fun r ->
  Log.if_error ~use:exit_err_json @@
  let layout = format = `Format Jsont.Layout in
  match do_diff with
  | false ->
      let* j = Jsont_bytesrw.decode ~file ~layout ~locs t r in
      if dec_only then Ok 0 else
      let* () = output ~format ~number_format j in
      Ok 0
  | true ->
      let src = Bytesrw.Bytes.Reader.to_string r in
      let* j = Jsont_bytesrw.decode_string ~file ~layout ~locs t src in
      let* fmted = output_string ~format ~number_format j in
      (match diff src fmted with
      | Ok exit -> if exit = 0 then Ok 0 else Ok exit_err_diff
      | Error e -> Log.err (fun m -> m "%s" e); Ok Cmdliner.Cmd.Exit.some_error)

let delete ~file ~path ~format ~number_format ~diff ~allow_absent ~locs =
  let del = Jsont.delete_path ~allow_absent path in
  trip_type ~file ~format ~number_format ~diff ~locs del

let fmt ~file ~format ~number_format ~diff ~locs ~dec_only =
  trip_type ~file ~format ~number_format ~diff ~locs ~dec_only Jsont.json

let get ~file ~path ~format ~number_format ~diff ~absent ~locs =
  let get = Jsont.path ?absent path Jsont.json in
  trip_type ~file ~format ~number_format ~diff ~locs get

let pp_locs_outline ppf v =
  let indent = 2 in
  let loc label ppf m =
    Fmt.pf ppf "@[<v>%s:@,%a@]@,"
      label Jsont.Textloc.pp_ocaml (Jsont.Meta.textloc m)
  in
  let rec value ppf = function
  | Jsont.Null ((), m) ->
      loc (Fmt.str "%a" Fmt.(code' Jsont.pp_null) ()) ppf m
  | Jsont.Bool (b, m) ->
      loc (Fmt.str "Bool %a" Fmt.(code' bool) b) ppf m
  | Jsont.Number (n, m) ->
      loc (Fmt.str "Number %a" Fmt.(code' Jsont.pp_number) n) ppf m
  | Jsont.String (s, m) ->
      loc (Fmt.str "String %a" (Fmt.code' Fmt.text_string_literal) s) ppf m
  | Jsont.Array (l, m) ->
      Format.pp_open_vbox ppf indent;
      loc "Array" ppf m; (Fmt.list value) ppf l;
      Format.pp_close_box ppf ()
  | Jsont.Object (o, m) ->
      let mem ppf ((name, m), v) =
        let l = Fmt.str "Member %a" (Fmt.code' Fmt.text_string_literal) name in
        loc l ppf m; value ppf v;
      in
      Format.pp_open_vbox ppf indent;
      loc "Object" ppf m; (Fmt.list mem) ppf o;
      Format.pp_close_box ppf ()
  in
  value ppf v

let output_locs ~file ~locs =
  Log.if_error ~use:exit_err_file @@
  with_infile file @@ fun reader ->
  Log.if_error ~use:exit_err_json @@
  let* j = Jsont_bytesrw.decode ~file ~locs Jsont.json reader in
  pp_locs_outline Format.std_formatter j;
  Ok 0

let set ~file ~path ~format ~number_format ~diff ~allow_absent ~json:j ~locs =
  let set = Jsont.set_path ~allow_absent Jsont.json path j in
  trip_type ~file ~format ~number_format ~diff ~locs set

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let exits =
  Cmd.Exit.info exit_err_file ~doc:"on file read errors." ::
  Cmd.Exit.info exit_err_json ~doc:"on JSON parse or path errors." ::
  Cmd.Exit.info exit_err_diff ~doc:"on JSON output differences." ::
  Cmd.Exit.defaults

let path_arg = Arg.conv' ~docv:"JPATH" Jsont.Path.(of_string, pp)
let caret_arg = Arg.conv' ~docv:"JCARET" Jsont.Caret.(of_string, pp)
let json_arg =
  let of_string s =
    Jsont_bytesrw.decode_string ~locs:true ~layout:true Jsont.json s
  in
  let pp = Jsont.pp_json in
  Arg.conv' ~docv:"JSON" (of_string, pp)

let format_opt ~default =
  let fmt =
    [ "indent", `Format Jsont.Indent;
      "minify", `Format Jsont.Minify;
      "preserve", `Format Jsont.Layout;
      "pretty", `Pretty ]
  in
  let doc =
    Fmt.str "Output style. Must be %s. $(b,minify) guarantess there is \
             no CR (U+000D) or LF (U+000A) in the output. $(b,pretty) is \
             similar to $(b,indent) but may yield more compact outputs."
      (Arg.doc_alts_enum fmt)
  in
  Arg.(value & opt (enum fmt) default & info ["f"; "format"] ~doc ~docv:"FMT")

let format_opt_default_pretty = format_opt ~default:`Pretty
let format_opt_default_preserve = format_opt ~default:(`Format Jsont.Layout)

let allow_absent_opt =
  let doc = "Do not error if $(i,JPATH) does not exist." in
  Arg.(value & flag & info ["a"; "allow-absent"] ~doc)

let locs =
  let doc = "Do not keep track of source locations." in
  Term.(const ( not ) $ Arg.(value & flag & info ["no-locs"] ~doc))

let number_format_opt =
  let doc = "Use C float format string $(docv) to format JSON numbers." in
  let number_format : Jsont.number_format Arg.conv =
    let parse s =
      try Ok (Scanf.format_from_string s Jsont.default_number_format) with
      | Scanf.Scan_failure _ ->
          Error (Fmt.str "Cannot format a float with %S" s)
    in
    let pp ppf fmt = Format.pp_print_string ppf (string_of_format fmt) in
    Arg.conv' (parse, pp)
  in
  Arg.(value & opt number_format Jsont.default_number_format &
       info ["n"; "number-format"] ~doc ~docv:"FMT")

let diff_flag =
  let doc =
    "Output diff between input and output (needs $(b,git) in \
     your $(b,PATH)). Exits with 0 only there are no differences."
  in
  Arg.(value & flag & info ["diff"] ~doc)

let dec_only =
  let doc = "Decode only, no output." in
  Arg.(value & flag & info ["d"; "decode-only"] ~doc)

let file_pos ~pos:p =
  let doc = "$(docv) is the JSON file. Use $(b,-) for stdin." in
  Arg.(value & pos p string "-" & info [] ~doc ~docv:"FILE")

let file_pos0 = file_pos ~pos:0
let file_pos1 = file_pos ~pos:1
let file_pos2 = file_pos ~pos:2

let common_man =
  [ `S Manpage.s_bugs;
    `P "This program is distributed with the jsont OCaml library. \
        See $(i,https://erratique.ch/software/jsont) for contact \
        information."; ]

let delete_cmd =
  let doc = "Delete the value indexed by a JSON path" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) deletes the value indexed by a JSON path. Outputs \
        $(b,null) if the path is empty. For example:";
    `Pre "$(iname) $(b,keywords.[0] package.json)";
    `Blocks common_man; ]
  in
  let path_opt =
    let doc = "Delete JSON path $(docv)." in
    Arg.(required & pos 0 (some path_arg) None & info [] ~doc ~docv:"JPATH")
  in
  Cmd.v (Cmd.info "delete" ~doc ~sdocs ~exits ~man) @@
  let+ file = file_pos1
  and+ path = path_opt
  and+ format = format_opt_default_preserve
  and+ number_format = number_format_opt
  and+ diff = diff_flag
  and+ allow_absent = allow_absent_opt
  and+ locs = locs in
  delete ~file ~path ~format ~number_format ~diff ~allow_absent ~locs

let fmt_cmd =
  let doc = "Format JSON" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) formats the given JSON data. For example:";
    `Pre "$(iname) $(b,-f minify package.json)";
    `Blocks common_man; ]
  in
  Cmd.v (Cmd.info "fmt" ~doc ~sdocs ~exits ~man) @@
  let+ file = file_pos0
  and+ format = format_opt_default_pretty
  and+ number_format = number_format_opt
  and+ diff = diff_flag
  and+ locs = locs
  and+ dec_only = dec_only in
  fmt ~file ~format ~number_format ~diff ~locs ~dec_only

let get_cmd =
  let doc = "Extract the value indexed by a JSON path" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) outputs the value indexed by a JSON path. For example:";
    `Pre "$(iname) $(b,'.' package.json)"; `Noblank;
    `Pre "$(iname) $(b,'keywords.[0]' package.json)";
    `Blocks common_man; ]
  in
  let path_pos =
    let doc = "Extract the value indexed by JSON path $(docv)." in
    Arg.(required & pos 0 (some path_arg) None & info [] ~doc ~docv:"JPATH")
  in
  let absent_opt =
    let doc = "$(docv) to return if the path does not exist." in
    Arg.(value & opt (some json_arg) None &
         info ["a"; "absent"] ~doc ~docv:"JSON")
  in
  Cmd.v (Cmd.info "get" ~doc ~sdocs ~exits ~man) @@
  let+ file = file_pos1
  and+ path = path_pos
  and+ format = format_opt_default_pretty
  and+ number_format = number_format_opt
  and+ diff = diff_flag
  and+ absent = absent_opt
  and+ locs = locs in
  get ~file ~path ~format ~number_format ~diff ~absent ~locs

let set_cmd =
  let doc = "Set the value indexed by a JSON path" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) sets the value indexed by a JSON path. For example:";
    `Blocks common_man; ]
  in
  let path_pos =
    let doc = "Set the value indexed by JSON path $(docv)." in
    Arg.(required & pos 0 (some path_arg) None & info [] ~doc ~docv:"JPATH")
  in
  let json_pos =
    let doc = "$(docv) to insert or substitute" in
    Arg.(required & pos 1 (some json_arg) None & info [] ~doc ~docv:"JSON")
  in
  Cmd.v (Cmd.info "set" ~doc ~sdocs ~exits ~man) @@
  let+ file = file_pos2
  and+ path = path_pos
  and+ json = json_pos
  and+ format = format_opt_default_preserve
  and+ number_format = number_format_opt
  and+ diff = diff_flag
  and+ allow_absent = allow_absent_opt
  and+ locs = locs in
  set ~file ~path ~format ~number_format ~diff ~allow_absent ~json ~locs

let locs_cmd =
  let doc = "Show JSON parse locations" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) outputs JSON parse locations. For example:";
    `Pre "$(iname) package.json)";
    `Blocks common_man; ]
  in
  Cmd.v (Cmd.info "locs" ~doc ~sdocs ~exits ~man) @@
  let+ file = file_pos0
  and+ locs = locs in
  output_locs ~file ~locs

let jsont =
  let doc = "Process JSON data" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;
    `P "$(mname) processes JSON data in various ways.";
    `Pre "$(b,curl -L URL) | $(mname) $(b,fmt -)"; `Noblank;
    `Pre "$(mname) $(b,get '.' package.json)"; `Noblank;
    `Pre "$(mname) $(b,get 'keywords.[0]' package.json)";
    `P "For $(mname) a JSON path is a dot separated sequence of \
        indexing operations. For example $(b,books.[1].authors.[0]) indexes \
        an object on the $(b,books) member, then on the second element of \
        an array, then the $(b,authors) member of an object and finally \
        the first element of that array.";
    `P "In general because of your shell's special characters you are better \
        off single quoting your JSON paths.";
    `P "Note that $(mname) JSON PATH are unrelated to the JSONPath \
        query language (RFC 9535).";
    `Blocks common_man; ]
  in
  Cmd.group (Cmd.info "jsont" ~version:"%%VERSION%%" ~doc ~sdocs ~exits ~man) @@
  [get_cmd; delete_cmd; fmt_cmd; locs_cmd; set_cmd;]

let main () = Cmd.eval' jsont
let () = if !Sys.interactive then () else exit (main ())
