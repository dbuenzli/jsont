(*---------------------------------------------------------------------------
   Copyright (c) 2024 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* Tests for soup.ml *)

open B0_testing

open Soup

(* More combinators *)

let number = Number { dec = Fun.id; enc = Fun.id }
let array elt =
  let dec_empty = [] and dec_add a _i v = v :: a in
  let dec_finish elts = List.rev elts in
  let dec_skip _ _ = false in
  let enc f acc vs = List.fold_left f acc vs in
  Array { elt; dec_empty; dec_add; dec_skip; dec_finish; enc }

(* Test data *)

let content = "J'aime pas la soupe" and public = true
let json_msg0 = Json.(Obj ["public", Bool public; "content", String content])
let json_msg1 =
  Json.(Obj ["content", String "Heyho!"; "public", Bool public; "time",
             Number 1.])

let json_msg3 =
  Json.(Obj ["public", Bool public; "content", String (content ^ "!")])

let json_msgs = Json.Array [json_msg0; json_msg1]

(* Tests *)

let test_trip () =
  Test.test "generic trip test" @@ fun () ->
  let dec = decode json json_msgs in
  let trip = encode json dec in
  if json_msgs <> trip
  then (Test.log_fail "json_msgs <> trip"; assert false);
  ()

let test_msg () =
  Test.test "Message modelling and queries tests" @@ fun () ->
  let msg = { Message.content; public } in
  let msg' = decode Message.jsont json_msg0 in
  if msg <> msg' then (Test.log_fail "msg <> msg'"; assert false);
  let q n = get_nth n @@ get_mem "time" number in
  assert (query (q 1) json_msgs = 1.);
  Test.failure @@ (fun () -> query (q 0) json_msgs);
  let json_msgs' =
    let q =
      update_nth 0 @@ update_mem "content" @@
      map (fun s -> s ^ "!") Fun.id string
    in
    query (delete_nth 1) (query q json_msgs)
  in
  if json_msgs' <> Json.Array[json_msg3]
  then (Test.log_fail "json_msgs''"; assert false);
  let json_msgs' =
    let q =
      update_nth 0 @@ update_mem "content" (const string (content ^ "!"))
    in
    (query q json_msgs)
  in
  if json_msgs' <> Json.Array[json_msg3;json_msg1]
  then (Test.log_fail "json_msgs''"; assert false);
  ()

module Cases = struct

  type point = { x : float; y : float }
  type line = { p0 : point; p1 : point }
  type type' = Point of point | Line of line
  type geom = { name : string; type' : type' }

  (* more data *)

  let ml_geom =
    { name = "Hey";
      type' = Line { p0 = { x = 0.; y = 1.}; p1 = { x = 2.; y = 3.}} }

  let json_geom = (* out of order *)
    Json.(Obj ["name", String "Hey";
               "p0", Obj ["x", Number 0.; "y", Number 1.];
               "p1", Obj ["y", Number 3.; "x", Number 2.];
               "type", String "line"; ])

  (* JSON types *)

  let point_jsont =
    obj_map (fun x y -> { x; y })
    |> obj_mem "x" number ~enc:(fun p -> p.x)
    |> obj_mem "y" number ~enc:(fun p -> p.y)

  let line_jsont =
    let point = obj_finish point_jsont in
    obj_map (fun p0 p1 -> { p0; p1 })
    |> obj_mem "p0" point ~enc:(fun p -> p.p0)
    |> obj_mem "p1" point ~enc:(fun p -> p.p1)

  let case_point =
    { tag = "point"; obj_map = point_jsont; dec = fun p -> Point p }

  let case_line =
    { tag = "line"; obj_map = line_jsont; dec = fun l -> Line l }

  let cases =
    {  tag = { name = "type"; type' = string; id = Type.Id.make ();
               dec_absent = None; enc = (fun _ -> assert false);
               enc_omit = (fun _ -> assert false); };
       tag_compare = String.compare;
       id = Type.Id.make ();
       cases = [Case case_point; Case case_line];
       enc = (fun g -> g.type');
       enc_case = (function
         | Point p -> Case_value (case_point, p)
         | Line l -> Case_value (case_line, l)) }

  let geom_jsont : geom jsont =
    let obj = obj_map (fun name type' -> { name; type' }) in
    let obj = obj_mem "name" string obj ~enc:(fun g -> g.name) in
    obj_finish @@
    { obj with shape = Obj_cases cases; dec = Dec_app (obj.dec, cases.id) }
end

let test_cases () =
  Test.test "cases" @@ fun () ->
  let g = decode Cases.geom_jsont Cases.json_geom in
  if Cases.ml_geom <> g then (Test.log_fail "Cases.geom.ml <> g"; assert false);
  ()

let main () =
  Test.main @@ fun () ->
  test_trip ();
  test_msg ();
  test_cases ();
  ()

let () = if !Sys.interactive then () else exit (main ())
