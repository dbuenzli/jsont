(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind

let strf = Format.asprintf
let log_if_error ~use = function
| Ok v -> v
| Error e ->
    let exec = Filename.basename Sys.executable_name in
    let lines = String.split_on_char '\n' e in
    Format.eprintf "%s: %a @[<v>%a@]@."
      exec Jsont.Error.puterr () Format.(pp_print_list pp_print_string) lines;
    use

let exit_err_file = 1
let exit_err_json = 2
let exit_err_diff = 3

module Os = struct

  (* Emulate B0_std.Os functionality to eschew the dep.
     Note: this is only used for the [diff] function. *)

  let read_file file =
    try
      let ic = if file = "-" then stdin else open_in_bin file in
      let finally () = if file = "-" then () else close_in_noerr ic in
      Fun.protect ~finally @@ fun () -> Ok (In_channel.input_all ic)
    with
    | Sys_error err -> Error err

  let write_file file s =
    try
      let oc = if file = "-" then stdout else open_out_bin file in
      let finally () = if file = "-" then () else close_out_noerr oc in
      Fun.protect ~finally @@ fun () -> Ok (Out_channel.output_string oc s)
    with
    | Sys_error err -> Error err

  let with_tmp_dir f =
    try
      let tmpdir =
        let file = Filename.temp_file "cmarkit" "dir" in
        (Sys.remove file; Sys.mkdir file 0o700; file)
      in
      let finally () = try Sys.rmdir tmpdir with Sys_error _ -> () in
      Fun.protect ~finally @@ fun () -> Ok (f tmpdir)
    with
    | Sys_error err -> Error ("Making temporary dir: " ^ err)

  let with_cwd cwd f =
    try
      let curr = Sys.getcwd () in
      let () = Sys.chdir cwd in
      let finally () = try Sys.chdir curr with Sys_error _ -> () in
      Fun.protect ~finally @@ fun () -> Ok (f ())
    with
    | Sys_error err -> Error ("With cwd: " ^ err)
end

let diff src fmted =
  let env = ["GIT_CONFIG_SYSTEM=/dev/null"; "GIT_CONFIG_GLOBAL=/dev/null"; ] in
  let set_env = match Sys.win32 with
  | true -> String.concat "" (List.map (fun e -> "set " ^ e ^ " && ") env)
  | false -> String.concat " " env
  in
  let diff = "git diff --ws-error-highlight=all --no-index --patience " in
  let src_file = "src" and fmted_file = "fmt" in
  let cmd = String.concat " " [set_env; diff; src_file; fmted_file] in
  Result.join @@ Result.join @@ Os.with_tmp_dir @@ fun dir ->
  Os.with_cwd dir @@ fun () ->
  let* () = Os.write_file src_file src in
  let* () = Os.write_file fmted_file fmted in
  Ok (Sys.command cmd)
        
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
| `Pretty -> Ok (Format.asprintf "@[%a@]" (Jsont.pp_json' ~number_format ()) j)
| `Format format ->
    Jsont_bytesrw.encode_string ~format ~number_format Jsont.json j

let trip_type
    ?(dec_only = false) ~file ~format ~number_format ~diff:do_diff ~locs t
  =
  log_if_error ~use:exit_err_file @@
  with_infile file @@ fun r ->
  log_if_error ~use:exit_err_json @@
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
      | Error e -> Format.eprintf "%s" e; Ok Cmdliner.Cmd.Exit.some_error)

let delete ~file ~path ~format ~number_format ~diff ~allow_absent ~locs =
  let del = Jsont.delete_path ~allow_absent path in
  trip_type ~file ~format ~number_format ~diff ~locs del

let fmt ~file ~format ~number_format ~diff ~locs ~dec_only =
  trip_type ~file ~format ~number_format ~diff ~locs ~dec_only Jsont.json

let get ~file ~path ~format ~number_format ~diff ~absent ~locs =
  let get = Jsont.path ?absent path Jsont.json in
  trip_type ~file ~format ~number_format ~diff ~locs get

let locs' ~file =
  let pf = Format.fprintf in
  let pp_code = Jsont.Repr.pp_code in
  let pp_locs_outline ppf v =
    let indent = 2 in
    let loc label ppf m =
      pf ppf "@[<v>%s:@,%a@]@,"
        label Jsont.Textloc.pp_ocaml (Jsont.Meta.textloc m)
    in
    let rec value ppf = function
    | Jsont.Null ((), m) ->
        loc (strf "%a" pp_code (strf "%a" Jsont.pp_null ())) ppf m
    | Jsont.Bool (b, m) ->
        loc (strf "Bool %a" pp_code (strf "%a" Jsont.pp_bool b)) ppf m
    | Jsont.Number (n, m) ->
        loc (strf "Number %a" pp_code (strf "%a" Jsont.pp_number n)) ppf m
    | Jsont.String (s, m) ->
        loc (strf "String %a" pp_code  (strf "%a" Jsont.pp_string s)) ppf m
    | Jsont.Array (l, m) ->
        Format.pp_open_vbox ppf indent;
        loc "Array" ppf m; (Format.pp_print_list value) ppf l;
        Format.pp_close_box ppf ()
    | Jsont.Object (o, m) ->
      let mem ppf ((name, m), v) =
        let l = strf "Member %a" pp_code (strf "%a" Jsont.pp_string name) in
        loc l ppf m; value ppf v;
      in
      Format.pp_open_vbox ppf indent;
      loc "Object" ppf m; (Format.pp_print_list mem) ppf o;
      Format.pp_close_box ppf ()
    in
    value ppf v
  in
  log_if_error ~use:exit_err_file @@
  with_infile file @@ fun reader ->
  log_if_error ~use:exit_err_json @@
  let* j = Jsont_bytesrw.decode ~file ~locs:true Jsont.json reader in
  pp_locs_outline Format.std_formatter j;
  Ok 0

let set
    ~file ~path ~format ~number_format ~diff ~allow_absent ~stub ~json:j ~locs
  =
  let set = Jsont.set_path ?stub ~allow_absent Jsont.json path j in
  trip_type ~file ~format ~number_format ~diff ~locs set

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let exits =
  Cmd.Exit.info exit_err_file ~doc:"on file read errors." ::
  Cmd.Exit.info exit_err_json ~doc:"on JSON parse or path errors." ::
  Cmd.Exit.info exit_err_diff ~doc:"on JSON output differences." ::
  Cmd.Exit.defaults

let path_arg = Arg.conv' ~docv:"JSON_PATH" Jsont.Path.(of_string, pp)
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
    strf "Output style. Must be %s. $(b,minify) guarantess there is \
         no CR (U+000D) or LF (U+000A) in the output. $(b,pretty) is \
         similar to $(b,indent) but may yield more compact outputs."
      (Arg.doc_alts_enum fmt)
  in
  Arg.(value & opt (enum fmt) default & info ["f"; "format"] ~doc ~docv:"FMT")

let format_opt_default_pretty = format_opt ~default:`Pretty
let format_opt_default_preserve = format_opt ~default:(`Format Jsont.Layout)

let allow_absent_opt =
  let doc = "Do not error if $(i,JSON_PATH) does not exist." in
  Arg.(value & flag & info ["a"; "allow-absent"] ~doc)

let locs_default_false =
  let doc = "Keep track of source locations (improves error messages)." in
  Arg.(value & flag & info ["locs"] ~doc)

let locs_default_true =
  let doc = "Do not keep track of source locations." in
  Term.(const ( not ) $ Arg.(value & flag & info ["no-locs"] ~doc))

let number_format_opt =
  let doc = "Use C float format string $(docv) to format JSON numbers." in
  let number_format : Jsont.number_format Arg.conv =
    let parse s =
      try Ok (Scanf.format_from_string s Jsont.default_number_format) with
      | Scanf.Scan_failure _ ->
          Error (strf "Cannot format a float with %S" s)
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
    `P "$(iname) deletes the value indexed by a JSON path. Outputs $(b,null) \
        on the root path $(b,'.'). Examples:";
    `Pre "$(iname) $(b,keywords.[0] package.json)"; `Noblank;
    `Pre "$(iname) $(b,-a keywords.[0] package.json)";
    `Blocks common_man; ]
  in
  let path_opt =
    let doc = "Delete JSON path $(docv)." and docv = "JSON_PATH" in
    Arg.(required & pos 0 (some path_arg) None & info [] ~doc ~docv)
  in
  Cmd.v (Cmd.info "delete" ~doc ~sdocs ~exits ~man) @@
  let+ file = file_pos1
  and+ path = path_opt
  and+ format = format_opt_default_preserve
  and+ number_format = number_format_opt
  and+ diff = diff_flag
  and+ allow_absent = allow_absent_opt
  and+ locs = locs_default_true in
  delete ~file ~path ~format ~number_format ~diff ~allow_absent ~locs

let fmt_cmd =
  let doc = "Format JSON" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;
    `P "$(iname) formats JSON. Examples:";
    `Pre "$(iname) $(b,package.json)"; `Noblank;
    `Pre "$(iname) $(b,-f minify package.json)";
    `Blocks common_man; ]
  in
  Cmd.v (Cmd.info "fmt" ~doc ~sdocs ~exits ~man) @@
  let+ file = file_pos0
  and+ format = format_opt_default_pretty
  and+ number_format = number_format_opt
  and+ diff = diff_flag
  and+ locs = locs_default_false
  and+ dec_only = dec_only in
  fmt ~file ~format ~number_format ~diff ~locs ~dec_only

let get_cmd =
  let doc = "Extract the value indexed by a JSON path" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;
    `P "$(iname) outputs the value indexed by a JSON path. Examples:";
    `Pre "$(iname) $(b,'keywords.[0]' package.json)"; `Noblank;
    `Pre "$(iname) $(b,-a 'null' 'keywords.[0]' package.json)"; `Noblank;
    `Pre "$(iname) $(b,-a '[]' 'keywords' package.json)"; `Noblank;
    `Pre "$(iname) $(b,'.' package.json)";
    `Blocks common_man; ]
  in
  let path_pos =
    let doc = "Extract the value indexed by JSON path $(docv)." in
    Arg.(required & pos 0 (some path_arg) None & info [] ~doc ~docv:"JSON_PATH")
  in
  let absent_opt =
    let doc = "Do not error if $(i,JSON_PATH) does not exist, output $(docv) \
               instead."
    in
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
  and+ locs = locs_default_true in
  get ~file ~path ~format ~number_format ~diff ~absent ~locs

let set_cmd =
  let doc = "Set the value indexed by a JSON path" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;
    `P "$(iname) sets the value indexed by a JSON path. Examples:";
    `Pre "$(iname) $(b,keywords '[\"codec\"]' package.json)"; `Noblank;
    `Pre "$(iname) $(b,keywords.[0] '\"codec\"' package.json)"; `Noblank;
    `Pre "$(iname) $(b,-a keywords.[4] '\"codec\"' package.json)"; `Noblank;
    `Pre "$(iname) $(b,-s null -a keywords.[4] '\"codec\"' package.json)";
    `Blocks common_man; ]
  in
  let path_pos =
    let doc = "Set the value indexed by JSON path $(docv)." in
    Arg.(required & pos 0 (some path_arg) None & info [] ~doc ~docv:"JSON_PATH")
  in
  let json_pos =
    let doc = "Set value to $(docv)." in
    Arg.(required & pos 1 (some json_arg) None & info [] ~doc ~docv:"JSON")
  in
  let stub =
    let doc =
      "Use $(b,docv) as a stub value to use if an array needs to be extended \
       when $(b,-a) is used. By default uses the natural zero of the \
       set data: null for null, false for booleans, 0 for numbers, empty
       string for strings, empty array for array, empty object for object."
    in
    Arg.(value & opt (some json_arg) None & info ["s"; "stub"] ~doc
           ~docv:"JSON")
  in
  Cmd.v (Cmd.info "set" ~doc ~sdocs ~exits ~man) @@
  let+ file = file_pos2
  and+ path = path_pos
  and+ json = json_pos
  and+ stub = stub
  and+ format = format_opt_default_preserve
  and+ number_format = number_format_opt
  and+ diff = diff_flag
  and+ allow_absent = allow_absent_opt
  and+ locs = locs_default_true in
  set ~file ~path ~format ~number_format ~diff ~allow_absent ~stub ~json ~locs

let locs_cmd =
  let doc = "Show JSON parse locations" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) outputs JSON parse locations. Example:";
    `Pre "$(iname) $(b,package.json)";
    `Blocks common_man; ]
  in
  Cmd.v (Cmd.info "locs" ~doc ~sdocs ~exits ~man) @@
  let+ file = file_pos0 in
  locs' ~file

let jsont =
  let doc = "Process JSON data" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;
    `P "$(mname) processes JSON data in various ways.";
    `Pre "$(b,curl -L URL) | $(mname) $(b,fmt)"; `Noblank;
    `Pre "$(mname) $(b,fmt package.json)"; `Noblank;
    `Pre "$(mname) $(b,get 'keywords.[0]' package.json)"; `Noblank;
    `Pre "$(mname) $(b,set 'keywords.[0]' '\"codec\"' package.json)"; `Noblank;
    `Pre "$(mname) $(b,delete 'keywords.[0]' package.json)";
    `P "More information about $(b,jsont)'s JSON paths is in the section \
        JSON PATHS below.";
    `S Manpage.s_commands;
    `S Manpage.s_common_options;
    `S "JSON PATHS";
    `P "For $(mname) a JSON path is a dot separated sequence of \
        indexing operations. For example $(b,books.[1].authors.[0]) indexes \
        an object on the $(b,books) member, then on the second element of \
        an array, then the $(b,authors) member of an object and finally \
        the first element of that array. The root path is $(b,.), it can
        be omitted if there are indexing operations.";
    `P "In general because of your shell's special characters it's better \
        to single quote your JSON paths.";
    `P "Note that $(mname)'s JSON PATH are unrelated to the JSONPath \
        query language (RFC 9535).";
    `Blocks common_man; ]
  in
  Cmd.group (Cmd.info "jsont" ~version:"%%VERSION%%" ~doc ~sdocs ~exits ~man) @@
  [get_cmd; delete_cmd; fmt_cmd; locs_cmd; set_cmd;]

let main () = Cmd.eval' jsont
let () = if !Sys.interactive then () else exit (main ())
