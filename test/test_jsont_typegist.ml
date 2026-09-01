(*---------------------------------------------------------------------------
   Copyright (c) 2026 The typegist programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open B0_std
open B0_testing
open Typegist

let gjson ?(trip = true) g v =
  let jsont = Jsont_typegist.to_jsont ~versioned:false g in
  let enc =
    let format = Jsont.Indent in
    Jsont_bytesrw.encode_string ~format jsont v |> Test.error_to_failstop
  in
  if trip then begin
    let dec = Jsont_bytesrw.decode_string jsont enc |> Test.error_to_failstop in
    Test.holds (Fun.Generic.equal g dec v)
  end;
  enc

let test_some_scalars =
  Test.test "Type.Gist.{bool,char,uchar,int64,float}" @@
  fun () ->
  Snap.string (gjson Type.Gist.bool true) @> __POS_OF__
    {|true|};
  Snap.string (gjson Type.Gist.char 'a') @> __POS_OF__
    {|"61"|};
  Snap.string (gjson Type.Gist.char '\n') @> __POS_OF__
    {|"0a"|};
  Snap.string (gjson Type.Gist.uchar (Uchar.of_int 0x1F42B)) @> __POS_OF__
    {|"🐫"|};
  Snap.string (gjson Type.Gist.uchar (Uchar.of_int 0x0A)) @> __POS_OF__
    {|"\n"|};
  Snap.string (gjson Type.Gist.int64 557L) @> __POS_OF__
    {|557|};
  Snap.string (gjson Type.Gist.int64 Int64.max_int) @> __POS_OF__
    {|"9223372036854775807"|};
  Snap.string (gjson Type.Gist.float nan) @> __POS_OF__
    {|"nan"|};
  Snap.string (gjson Type.Gist.float infinity) @> __POS_OF__
    {|"inf"|};
  Snap.string (gjson Type.Gist.float 1.0) @> __POS_OF__
    {|1|};
  Snap.string (gjson Type.Gist.float 558.5) @> __POS_OF__
    {|558.5|};
  ()

let test_tuple =
  Test.test "Type.Gist.tuple" @@ fun () ->
  let point_gist = Type.Gist.(t2 int int) in
  Snap.string (gjson point_gist (3, 2)) @> __POS_OF__
    {|{
  "0": 3,
  "1": 2
}|};
  ()

let test_record =
  Test.test "Type.Gist.record" @@ fun () ->
  let type t = { username : string; transient : string; } in
  let user username transient = { username; transient } in
  let name u = u.username in
  let transient u = u.transient in
  let gist =
    let meta =
      Type.Gist.Meta.(empty |> Ignore.add true |> Default.add "start")
    in
    Type.Gist.record "User.t" user
    |> Type.Gist.field "username" Type.Gist.utf_8_string name
    |> Type.Gist.field "transient" ~meta Type.Gist.utf_8_string transient
    |> Type.Gist.finish
   in
  let v = user "Hey" "Not saved" in
  Snap.string (gjson gist v) @> __POS_OF__
    {|{
  "username": "Hey"
}|};
  (* Test that we can't inject data. *)
  let jsont = Jsont_typegist.to_jsont ~versioned:false gist in
  let malicious = {|{ "username": "Hey", "transient": "got you!" }|} in
  begin match Jsont_bytesrw.decode_string jsont malicious with
  | Ok _ -> Test.fail "Should not have parsed"
  | Error _ -> Test.pass ()
  end;
  (* But probing is possible *)
  let probe = {|{ "username": "Hey", "transient": null }|} in
  begin match Jsont_bytesrw.decode_string jsont probe with
  | Ok u -> Test.string (transient u) "start";
  | Error e ->  Test.fail "Should have parsed but got %s" e
  end;
  ()

let test_option =
  Test.test "Type.Gist.option" @@ fun () ->
  let gist = Type.Gist.(option int) in
  Snap.string (gjson gist None) @> __POS_OF__ {|null|};
  Snap.string (gjson gist (Some 3)) @> __POS_OF__ {|3|};
  ()

let test_list =
  Test.test "Type.Gist.list" @@ fun () ->
  let gist = Type.Gist.(list int) in
  Snap.string (gjson gist []) @> __POS_OF__ {|[]|};
  Snap.string (gjson gist [1;2;3]) @> __POS_OF__ {|[
  1,
  2,
  3
]|};
  ()

let test_result =
  Test.test "Type.Gist.result" @@ fun () ->
  let gist = Type.Gist.(result int utf_8_string) in
  Snap.string (gjson gist (Ok 3)) @> __POS_OF__ {|{
  "case": "Ok",
  "0": 3
}|};
  Snap.string (gjson gist (Error "not found")) @> __POS_OF__ {|{
  "case": "Error",
  "0": "not found"
}|};
  ()

let test_variant =
  Test.test "Type.Gist.variant" @@ fun () ->
  let type operation = Create | Delete of { id : int } in
  let gist =
    let create = Type.Gist.case "Create" Create |> Type.Gist.finish in
    let delete =
      let make id = Delete { id } in
      let id = function Delete { id } -> id | _ -> assert false in
      Type.Gist.case "Delete" make
      |> Type.Gist.field "id" Type.Gist.int id
      |> Type.Gist.finish
    in
    let case_index = function Create -> 0 | Delete _ -> 1 in
    Type.Gist.variant ~name:"M.operation" [create; delete] ~case_index
  in
  Snap.string (gjson gist Create) @> __POS_OF__ {|{
  "case": "Create"
}|};
  Snap.string (gjson gist (Delete { id = 3})) @> __POS_OF__ {|{
  "case": "Delete",
  "id": 3
}|};
  ()

let test_variant =
  Test.test "Type.Gist.{variant,rec}" @@ fun () ->
  let type nat = Zero | Succ of nat in
  let nat_gist =
    let rec g = lazy begin
      let nat = Type.Gist.rec' g in
      let zero = Type.Gist.case "Zero" Zero |> Type.Gist.finish in
      let succ =
        let make n = Succ n in
        let get = function Succ n -> n | _ -> assert false in
        Type.Gist.case "Succ" make
        |> Type.Gist.comp nat get
        |> Type.Gist.finish
      in
      let case_index = function Zero -> 0 | Succ _ -> 1 in
      Type.Gist.variant ~name:"M.nat" [zero; succ] ~case_index
    end
    in
    Lazy.force g
  in
  Snap.string (gjson nat_gist Zero) @> __POS_OF__ {|{
  "case": "Zero"
}|};
  Snap.string (gjson nat_gist (Succ (Succ Zero))) @> __POS_OF__ {|{
  "case": "Succ",
  "0": {
    "case": "Succ",
    "0": {
      "case": "Zero"
    }
  }
}|};
  ()

let test_strings_and_bytes =
  Test.test "Type.Gist.{utf_8,binary}_string" @@ fun () ->
  Snap.string (gjson Type.Gist.binary_string "a🐫") @> __POS_OF__
    {|"61f09f90ab"|};
  Snap.string (gjson Type.Gist.utf_8_string "a🐫") @> __POS_OF__
    {|"a🐫"|};
  ()

let test_array =
  Test.test "Type.Gist.{array,iarray,floatarray}" @@ fun () ->
  Snap.string (gjson Type.Gist.(array int) [||]) @> __POS_OF__
    {|[]|};
  Snap.string (gjson Type.Gist.(array int) [|1;2|]) @> __POS_OF__
    {|[
  1,
  2
]|};
  Snap.string (gjson Type.Gist.(iarray int) (Iarray.of_list [])) @> __POS_OF__
    {|[]|};
  Snap.string (gjson Type.Gist.(iarray int) (Iarray.of_list[1;2])) @> __POS_OF__
    {|[
  1,
  2
]|};
  Snap.string (gjson Type.Gist.floatarray (Float.Array.of_list[1.5;2.5]))
  @> __POS_OF__
    {|[
  1.5,
  2.5
]|};
  ()

let test_map_module =
  Test.test "Type.Gist.map" @@ fun () ->
  let module String_map = Map.Make (String) in
  let int_string_map_gist =
    Type.Gist.(map "String_map.t" (module String_map) utf_8_string int)
  in
  let bindings = ["b🐫", 2; "a", 1] in
  let v = String_map.of_list bindings in
  Snap.string (gjson int_string_map_gist v) @> __POS_OF__
    {|{
  "a": 1,
  "b🐫": 2
}|};
  let int_string_map_gist =
    Type.Gist.(map "String_map.t" (module String_map) binary_string int)
  in
  Snap.string (gjson int_string_map_gist v) @> __POS_OF__
    {|{
  "61": 1,
  "62f09f90ab": 2
}|};
  let module Int_map = Map.Make (Int) in
  let v = Int_map.of_list (List.map Pair.swap bindings) in
  let string_int_map_gist =
    Type.Gist.(map "Int_map.t" (module Int_map) int utf_8_string)
  in
  Snap.string (gjson string_int_map_gist v) @> __POS_OF__
    {|[
  {
    "key": 1,
    "value": "a"
  },
  {
    "key": 2,
    "value": "b🐫"
  }
]|};
  ()

let test_map_module =
  Test.test "Type.Gist.map" @@ fun () ->
  let module String_map = Map.Make (String) in
  let int_string_map_gist = Type.Gist.(hashtbl utf_8_string int) in
  let bindings = ["b🐫", 2; "a", 1] in
  let h = Hashtbl.of_seq (List.to_seq bindings) in
  Snap.string (gjson int_string_map_gist h) @> __POS_OF__
    {|{
  "a": 1,
  "b🐫": 2
}|};
  let int_string_map_gist = Type.Gist.(hashtbl binary_string int) in
  Snap.string (gjson int_string_map_gist h) @> __POS_OF__
    {|{
  "61": 1,
  "62f09f90ab": 2
}|};
  let string_int_map_gist = Type.Gist.(hashtbl int utf_8_string) in
  let h = Hashtbl.of_seq (List.to_seq (List.map Pair.swap bindings)) in
  Snap.string (gjson string_int_map_gist h) @> __POS_OF__
    {|[
  {
    "key": 2,
    "value": "b🐫"
  },
  {
    "key": 1,
    "value": "a"
  }
]|};
  ()

let test_atomic =
  Test.test "Type.Gist.atomic" @@ fun () ->
  let type epoch = int Atomic.t in
  let epoch_gist = Type.Gist.atomic ~name:"M.epoch" Type.Gist.int in
  Snap.string (gjson epoch_gist (Atomic.make 3)) @> __POS_OF__
    {|3|};
  ()

let test_abstract =
  Test.test "Type.Gist.abstract" @@ fun () ->
  let module _ = struct
    type person = string
    let person_gist_v1 = Type.Gist.(of_gist ~name:"M.person" utf_8_string)
    let person_gist =
      let v1 = Type.Gist.Abstract.Version.make ~name:"v1" person_gist_v1 in
      Type.Gist.abstract "M.person" [v1]
    ;;

    let () =
      let v = "Jane" in
      Snap.string (gjson person_gist v) @> __POS_OF__
        {|{
  "version": "v1",
  "value": "Jane"
}|};
  end
  in
  let module _ = struct
    type person = string * string
    let person_gist_v2 =
      Type.Gist.(t2 ~name:"M.person" utf_8_string utf_8_string)

    let person_gist_v1 =
      let inject (last, first) = String.concat " " [last; first] in
      let project n = match String.split_first ~sep:" " n with
      | None -> ("", n) | Some (last, first) -> last, first
      in
      Type.Gist.view ~inject ~project @@
      Type.Gist.(of_gist ~name:"M.person" utf_8_string)

    let person_gist =
      let v1 = Type.Gist.Abstract.Version.make ~name:"v1" person_gist_v1 in
      let v2 = Type.Gist.Abstract.Version.make ~name:"v2" person_gist_v2 in
      Type.Gist.abstract "M.person" [v1; v2]

    let () =
      let person = "Doe", "Jane" in
      Snap.string (gjson person_gist person) @> __POS_OF__
        {|{
  "version": "v2",
  "value": {
    "0": "Doe",
    "1": "Jane"
  }
}|};
      let jsont = Jsont_typegist.to_jsont ~versioned:false person_gist in
      let v1_person = {| { "version": "v1", "value": "Doe Jane" } |} in
      begin match Jsont_bytesrw.decode_string jsont v1_person with
      | Ok v -> Test.holds (Fun.Generic.equal person_gist person v)
      | Error e ->  Test.fail "Should have parsed but got %s" e
      end
  end
  in
  let module _ = struct
    type person_v1 = string
    type person_v2 = string * string
    type person = V1 of person_v1 | V2 of person_v2

    let person_gist_v1 =
      let inject = function V1 v1 -> v1 | _ -> assert false in
      let project v1 = V1 v1 in
      Type.Gist.view ~inject ~project @@
      Type.Gist.(of_gist ~name:"M.person" utf_8_string)

    let person_gist_v2 =
      let inject = function V2 v2 -> v2 | _ -> assert false in
      let project v2 = V2 v2 in
      Type.Gist.view ~inject ~project @@
      Type.Gist.(t2 ~name:"M.person" utf_8_string utf_8_string)

    let person_gist =
      let v1 = Type.Gist.Abstract.Version.make ~name:"v1" person_gist_v1 in
      let v2 = Type.Gist.Abstract.Version.make ~name:"v2" person_gist_v2 in
      let version_index = function V1 _ -> 0 | V2 _ -> 1 in
      Type.Gist.abstract "M.person" [v1; v2] ~version_index

    let () =
      let person = V1 ("Doe Jane") in
      Snap.string (gjson person_gist person) @> __POS_OF__
        {|{
  "version": "v1",
  "value": "Doe Jane"
}|};
      let person = V2 ("Doe", "Jane") in
      Snap.string (gjson person_gist person) @> __POS_OF__
        {|{
  "version": "v2",
  "value": {
    "0": "Doe",
    "1": "Jane"
  }
}|}
  end
  in
  ()

let test_queue =
  Test.test "Type.Gist.queue" @@ fun () ->
  let q = Queue.create () in
  let () = Queue.add 1 q; Queue.add 2 q in
  Snap.string (gjson Type.Gist.(queue int) q) @> __POS_OF__ {|[
  1,
  2
]|};
  ()

let test_mutual =
  Test.test "Mutual recursion" @@ fun () ->
  let type even = Zero | Succ_odd of odd and odd = Succ_even of even in
  let even_gist, odd_gist =
    let rec even_gist = lazy begin
      let odd_gist = Type.Gist.rec' odd_gist in
      let zero = Type.Gist.case "Zero" Zero |> Type.Gist.finish in
      let succ_odd =
        let make odd = Succ_odd odd in
        let get = function Succ_odd odd -> odd | _ -> assert false in
        Type.Gist.case "Succ_odd" make
        |> Type.Gist.comp odd_gist get
        |> Type.Gist.finish
      in
      let case_index = function Zero -> 0 | Succ_odd _ -> 1 in
      Type.Gist.variant ~name:"M.even" [zero; succ_odd] ~case_index
    end
    and odd_gist = lazy begin
      let even_gist = Type.Gist.rec' even_gist in
      let succ_even =
        let make even = Succ_even even in
        let get = function Succ_even even -> even in
        Type.Gist.case "Succ_even" make
        |> Type.Gist.comp even_gist get
      |> Type.Gist.finish
      in
      Type.Gist.variant ~name:"M.odd" [succ_even] ~case_index:(Fun.const 0)
    end
    in
    (Lazy.force even_gist), (Lazy.force odd_gist)
  in
  Snap.string (gjson even_gist Zero) @> __POS_OF__
    {|{
  "case": "Zero"
}|};
  Snap.string (gjson odd_gist (Succ_even Zero)) @> __POS_OF__
    {|{
  "case": "Succ_even",
  "0": {
    "case": "Zero"
  }
}|};
  Snap.string (gjson even_gist (Succ_odd (Succ_even Zero))) @> __POS_OF__
    {|{
  "case": "Succ_odd",
  "0": {
    "case": "Succ_even",
    "0": {
      "case": "Zero"
    }
  }
}|};
  ()



let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
