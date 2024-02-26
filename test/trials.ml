(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Message = struct
  type t = { content : string; public : bool }
  let make content public = { content; public }
  let content msg = msg.content
  let public msg = msg.public
  let jsont : t Jsont.t =
    Jsont.Obj.map make
    |> Jsont.Obj.mem "content" Jsont.string ~enc:content
    |> Jsont.Obj.mem "public" Jsont.bool ~enc:public
    |> Jsont.Obj.finish
end

type ('ret, 'f) app =
| Fun : 'f -> ('ret, 'f) app
| App : ('ret, 'a -> 'b) app * 'a -> ('ret, 'b) app

let ret : 'f -> ('ret, 'f) app = fun f -> Fun f
let app : ('ret, 'a -> 'b) app -> 'a -> ('ret, 'b) app = fun f a -> App (f, a)

let g ~i ~s = string_of_int i ^ s

let t0 : (string, string) app =
  app (app (ret (fun i s -> g ~i ~s)) 2) "bla"

(* That works but it's not the tructure that we want. *)

let ( let+ ) : 'a -> ('a -> 'b) -> ('ret, 'b) app = fun v f -> App (Fun f, v)
let ( and+ ) : 'a -> 'b -> 'a * 'b = fun x y -> (x, y)

let t1 : (string, string) app =
  let+ i = 2
  and+ s = "bla" in
  g ~i ~s
