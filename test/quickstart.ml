(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* Examples from the docs *)

let data =
{|{ "task": "Make new release",
    "status": "todo",
    "tags": ["work", "softwre"] }|}

let () =
  let p = Jsont.Path.(root |> mem "tags" |> nth 1) in
  let update = Jsont.(set_path string p "software") in
  let correct = Jsont_bytesrw.recode_string ~layout:true update data in
  print_endline (Result.get_ok correct)

module Status = struct
  type t = Todo | Done | Cancelled
  let assoc = ["todo", Todo; "done", Done; "cancelled", Cancelled ]
  let jsont = Jsont.enum ~kind:"Status" assoc
end

module Item = struct
  type t = { task : string; status : Status.t; tags : string list; }
  let make task status tags = { task; status; tags }
  let task i = i.task
  let status i = i.status
  let tags i = i.tags
  let jsont =
    Jsont.Object.map ~kind:"Item" make
    |> Jsont.Object.mem "task" Jsont.string ~enc:task
    |> Jsont.Object.mem "status" Status.jsont ~enc:status
    |> Jsont.Object.mem "tags" Jsont.(list string) ~enc:tags
      ~dec_absent:[] ~enc_omit:(( = ) [])
    |> Jsont.Object.finish
end

let items = Jsont.list Item.jsont
let items_of_json s = Jsont_bytesrw.decode_string items s
let items_to_json ?format is = Jsont_bytesrw.encode_string ?format items is
