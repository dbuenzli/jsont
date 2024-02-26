(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)


let update_nth' ?stub ?absent n q =
  let dec_empty _ctx = [] in
  let dec_add _ctx i v a = (if i = n then update_json q v else v) :: a in
  let dec_finish ctx meta len a =
    let count = n - len in
    let a =
      if count < 0 then a else match absent with
      | None -> err_index_out_of_bounds ctx meta ~len ~n
      | Some v ->
          let v = update_absent q v in
          let stub = Option.value ~default:v stub in
          let rec loop ~stub v count a =
            if count = 0 then v :: a else loop ~stub v (count - 1) (stub :: a)
          in
          loop ~stub v count a
    in
    Json.list ~meta (List.rev a)
  in
  let enc ctx f acc = function
  | Array (a, _) -> Array.list_enc ctx f acc a
  | j -> Json.error_sort ctx ~exp:Sort.Array j
  in
  let enc_meta _ctx j = Json.meta j in
  let enc = { Array.enc = enc } in
  let kind = "array" and doc = "A JSON array" in
  Array.type' @@
  Array.map ~kind ~doc ~dec_empty ~dec_add ~dec_finish ~enc ~enc_meta json
