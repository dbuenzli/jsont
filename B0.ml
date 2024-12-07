open B0_kit.V000
open Result.Syntax

(* Library names *)

let b0_std = B0_ocaml.libname "b0.std"
let bytesrw = B0_ocaml.libname "bytesrw"
let cmdliner = B0_ocaml.libname "cmdliner"
let brr = B0_ocaml.libname "brr"

let jsont = B0_ocaml.libname "jsont"
let jsont_bytesrw = B0_ocaml.libname "jsont.bytesrw"
let jsont_brr = B0_ocaml.libname "jsont.brr"

(* Libraries *)

let jsont_lib =
  let srcs = [ `Dir ~/"src" ] in
  B0_ocaml.lib jsont ~name:"jsont-lib" ~srcs

let jsont_bytesrw_lib =
  let srcs = [ `Dir ~/"src/bytesrw" ] in
  let requires = [bytesrw; jsont] in
  B0_ocaml.lib jsont_bytesrw ~srcs ~requires ~exports:requires

let jsont_brr_lib =
  let srcs = [ `Dir ~/"src/brr" ] in
  let requires = [brr; jsont] in
  B0_ocaml.lib jsont_brr ~srcs ~requires ~exports:requires

(* Tools *)

let jsont_tool =
  let srcs = [ `File ~/"test/jsont_tool.ml" ] in
  let requires = [cmdliner; bytesrw; jsont_bytesrw; jsont] in
  B0_ocaml.exe "jsont" ~public:true ~doc:"The jsont tool" ~srcs ~requires

(* Tests *)

let test ?(requires = []) =
  (* TODO b0: don't let jsoo builds downgrade everything to bytecode *)
  let meta =
    B0_meta.empty
    |> B0_meta.add B0_ocaml.Code.needs `Native
  in
  B0_ocaml.test ~meta ~requires:(jsont :: requires)

let quickstart =
  let doc = "Quick start examples" in
  test ~/"test/quickstart.ml" ~run:false ~requires:[jsont_bytesrw] ~doc

let cookbook =
  test ~/"test/cookbook.ml" ~run:false ~doc:"Cookbook examples"

let trials =
  test ~/"test/trials.ml" ~run:false ~doc:"Experiments"

let topojson =
  let doc = "Jsont modelling of TopoJSON" in
  let requires = [cmdliner; bytesrw; jsont_bytesrw] in
  test ~/"test/topojson.ml" ~run:false ~doc ~requires

let geojson =
  let doc = "Jsont modelling of GeoJSON" in
  let requires = [cmdliner; bytesrw; jsont_bytesrw] in
  test ~/"test/geojson.ml" ~run:false ~doc ~requires

let test_common =
  [ `File ~/"test/test_common.ml"; `File ~/"test/test_common_samples.ml" ]

let test_bytesrw =
  let doc = "Test Jsont_bytesrw codec" in
  let srcs = test_common in
  let requires = [b0_std; jsont_bytesrw] in
  test ~/"test/test_bytesrw.ml" ~run:true ~srcs ~requires ~doc

let test_jsont =
  let doc = "Test Jsont.Json codec" in
  let srcs = test_common in
  let requires = [b0_std; jsont_bytesrw] in
  test ~/"test/test_json.ml" ~run:true ~srcs ~requires ~doc

let test_brr =
  let doc = "Test Jsont_brr codec in the browser" in
  let srcs = `File ~/"test/test_brr.ml" :: test_common in
  let requires = [b0_std; brr; jsont; jsont_brr] in
  let meta = B0_meta.(empty |> tag test) in
  B0_jsoo.html_page "test_brr" ~doc ~meta ~srcs ~requires

(* Seriot JSON test suite *)

let seriot_suite_repo = "https://github.com/nst/JSONTestSuite.git"
let seriot_suite = ~/"tmp/JSONTestSuite"
let download_seriot_suite =
  let doc = "Download the Seriot test suite to tmp/" in
  B0_unit.of_action "download-seriot-suite" ~doc @@ fun env _ ~args:_ ->
  let* git = B0_env.get_cmd env (Cmd.tool "git") in
  let suite = B0_env.in_scope_dir env seriot_suite in
  let* created = Os.Dir.create ~make_path:true suite in
  if created
  then Os.Cmd.run Cmd.(git % "clone" % seriot_suite_repo %% path suite)
  else Os.Cmd.run Cmd.(git % "-C" %% path suite % "pull")

let test_seriot_suite =
  let doc = "Run the Seriot test suite" in
  let requires = [b0_std; cmdliner; jsont_bytesrw] in
  test ~/"test/test_seriot_suite.ml" ~doc ~requires

(* Expectation tests *)

let expect =
  let doc = "Test jsont expectations" in
  let meta = B0_meta.(empty |> tag test |> tag run) in
  B0_unit.of_action' "expect" ~meta ~units:[jsont_tool] ~doc @@
  B0_expect.action_func ~base:(Fpath.v "test/expect") @@ fun ctx ->
  let jsont = B0_expect.get_unit_exe_file_cmd ctx jsont_tool in
  let expect_valid_file ctx json file =
    let runs = (* command, output suffix *)
      [ Cmd.(arg "fmt" % "-fpretty"), ".pretty.json";
        Cmd.(arg "fmt" % "-findent"), ".indent.json";
        Cmd.(arg "fmt" % "-fminify"), ".minify.json";
        Cmd.(arg "fmt" % "-fpreserve"), ".layout.json";
        Cmd.(arg "locs"), ".locs" ]
    in
    let test_run ctx jsont file (cmd, ext) =
      let cmd = Cmd.(cmd %% path file) in
      let cwd = B0_expect.base ctx and stdout = Fpath.(file -+ ext) in
      B0_expect.stdout ctx ~cwd ~stdout Cmd.(jsont %% cmd)
    in
    List.iter (test_run ctx json file) runs
  in
  let expect_invalid_file ctx jsont file =
    let cwd = B0_expect.base ctx and stderr = Fpath.(file -+ ".stderr") in
    B0_expect.stderr ctx ~cwd ~stderr Cmd.(jsont % "fmt" %% path file)
  in
  let valid_files, invalid_files =
    let base_files = B0_expect.base_files ctx ~rel:true ~recurse:false in
    let input f = Fpath.get_ext ~multi:true f = ".json" in
    let files = List.filter input base_files in
    let is_valid f =
      not (String.starts_with ~prefix:"invalid" (Fpath.basename f))
    in
    List.partition is_valid files
  in
  List.iter (expect_valid_file ctx jsont) valid_files;
  List.iter (expect_invalid_file ctx jsont) invalid_files;
  ()

(* Paper *)

let paper = B0_meta.Key.make_tag "paper"

let soup_code =
  let doc = "Soup paper code and tests" in
  let srcs = [ `File ~/"paper/soup.ml"; `File ~/"paper/soup_test.ml" ] in
  let meta = B0_meta.(empty |> tag test |> tag paper) in
  B0_ocaml.exe "soup-code" ~srcs ~requires:[b0_std] ~meta ~doc

let soup =
  let doc = "Soup paper" in
  let base = Fpath.v "soup.tex" in
  let build b =
    let m = B0_build.memo b in
    let pdflatex =
      let vars = ["TEXINPUTS"]  in
      B0_memo.tool m (B0_memo.Tool.make ~vars (Fpath.v "pdflatex"))
    in
    let docdir = B0_build.in_scope_dir b ~/"paper" in
    let pdf = B0_build.in_current_dir b (Fpath.(base -+ ".pdf")) in
    let reads = [Fpath.(docdir / "jfp.cls"); Fpath.(docdir // base) ] in
    let writes = [pdf; Fpath.(pdf -+ ".aux"); Fpath.(pdf -+ ".log")] in
    let cwd = B0_build.current_dir b in
    let env =
      Os.Env.(empty |> add "TEXINPUTS" (Fpath.to_string docdir ^ "//:"))
    in
    let run_tex =
      pdflatex Cmd.(arg "-file-line-error" % "-halt-on-error" %
                    "-interaction=errorstopmode" %% path base)
    in
    B0_memo.ready_files m reads;
    B0_memo.spawn m ~cwd ~env ~reads ~writes:[] run_tex ~k:(fun _ _ ->
        (* Let's hope it reaches the fix point :-) *)
        B0_memo.spawn m ~cwd ~env ~reads ~writes run_tex);
    Fut.return ()
  in
  let show_pdf e u ~args:_ = (* TODO b0: B0_show_pdf action ? *)
    let pdf = Fpath.(B0_env.unit_dir e u // base -+ ".pdf") in
    let* view = B0_pdf_viewer.find ~search:(B0_env.get_cmd e) () in
    let* () = B0_pdf_viewer.show view pdf in
    Ok Os.Exit.ok
  in
  let meta =
    B0_meta.empty
    |> ~~ B0_unit.Action.key (`Fun ("show-pdf", show_pdf))
    |> B0_meta.tag paper
  in
  B0_unit.make ~meta ~doc "soup" build

(* Packs *)

let soup_pack =
  B0_pack.make "soup" ~doc:"Soup paper and code" ~locked:true @@
  [ soup; soup_code ]

let default =
 let meta =
   B0_meta.empty
   |> ~~ B0_meta.authors ["The jsont programmers"]
   |> ~~ B0_meta.maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
   |> ~~ B0_meta.homepage "https://erratique.ch/software/jsont"
   |> ~~ B0_meta.online_doc "https://erratique.ch/software/jsont/doc"
   |> ~~ B0_meta.licenses ["ISC"]
   |> ~~ B0_meta.repo "git+https://erratique.ch/repos/jsont.git"
   |> ~~ B0_meta.issues "https://github.com/dbuenzli/jsont/issues"
   |> ~~ B0_meta.description_tags ["json"; "codec"; "org:erratique"; ]
   |> ~~ B0_opam.build
     {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
                  "--with-cmdliner" "%{cmdliner:installed}%"
                  "--with-bytesrw" "%{bytesrw:installed}%"
                  "--with-brr" "%{brr:installed}%"]]|}
   |> ~~ B0_opam.depopts
     ["cmdliner", "";
      "brr", "";
      "bytesrw", ""]
   |> ~~ B0_opam.conflicts [ "cmdliner", {|< "1.3.0"|};
                             "brr", {|< "0.0.6"|}; ]
   |> ~~ B0_opam.depends
     [ "ocaml", {|>= "4.14.0"|};
       "ocamlfind", {|build|};
       "ocamlbuild", {|build|};
       "topkg", {|build & >= "1.0.3"|};
       "b0", {|dev & with-test|};
     ]
   |> B0_meta.tag B0_opam.tag
 in
 B0_pack.make "default" ~doc:"The jsont package" ~meta ~locked:true @@
 (* TODO b0: we should have something more convenient *)
 List.filter (Fun.negate (B0_unit.has_tag paper)) (B0_unit.list ())
