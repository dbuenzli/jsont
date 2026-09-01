#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let cmdliner = Conf.with_pkg "cmdliner"
let bytesrw = Conf.with_pkg "bytesrw"
let brr = Conf.with_pkg "brr"
let typegist = Conf.with_pkg "typegist"
let () =
  Pkg.describe "jsont" @@ fun c ->
  let cmdliner = Conf.value c cmdliner in
  let bytesrw = Conf.value c bytesrw in
  let brr = Conf.value c brr in
  let typegist = Conf.value c typegist in
  Ok [ Pkg.mllib ~api:["Jsont"] "src/jsont.mllib";
       Pkg.mllib ~cond:bytesrw
         ~dst_dir:"bytesrw" "src/bytesrw/jsont_bytesrw.mllib";
       Pkg.mllib ~cond:brr ~dst_dir:"brr" "src/brr/jsont_brr.mllib";
       Pkg.mllib ~cond:typegist ~dst_dir:"typegist"
         "src/typegist/jsont_typegist.mllib";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.doc "doc/cookbook.mld" ~dst:"odoc-pages/cookbook.mld";
       Pkg.bin ~cond:(cmdliner && bytesrw) "test/jsont_tool" ~dst:"jsont"; ]
