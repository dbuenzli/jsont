(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* Examples from the docs *)

let data =
{json|
{ "task": "Make new release",
  "status": "todo",
  "tags": ["work", "softwre"] }
|json}

let () =
  let p = Jsont.Path.(root |> mem "tags" |> nth 1) in
  let upd = Jsont.(set_path string p "software") in
  let correct = Jsont_bytesrw.recode_string ~layout:true upd data in
  print_string (Result.get_ok correct)

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
    Jsont.Obj.map ~kind:"Item" make
    |> Jsont.Obj.mem "task" Jsont.string ~enc:task
    |> Jsont.Obj.mem "status" Status.jsont ~enc:status
    |> Jsont.Obj.mem "tags"
         Jsont.(list string) ~dec_absent:[] ~enc:tags ~enc_omit:(( = ) [])
    |> Jsont.Obj.finish
end

let items = Jsont.list Item.jsont
let items_of_json s = Jsont_bytesrw.decode_string items s
let items_to_json ?format is = Jsont_bytesrw.encode_string ?format items is
