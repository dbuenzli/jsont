(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)


module String_map = Map.Make (String)

(* Items to do. *)

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

module Item_data = struct
  let i0 = Item.{ task = "Hey"; status = Todo; tags = ["huhu";"haha"] }
  let i0_json = (* in Jsont.Indent format *)
    "{\n\
    \  \"task\": \"Hey\",\n\
    \  \"status\": \"todo\",\n\
    \  \"tags\": [\n\
    \    \"huhu\",\n\
    \    \"haha\"\n\
    \  ]\n\
    }"

  let i1 = Item.{ task = "Ho"; status = Done; tags = [] }
  let i1_json = (* in Jsont.Indent format *)
    "{\n\
    \  \"task\": \"Ho\",\n\
    \  \"status\": \"done\"\n\
    }"
end

(* JSON types to excerice the different unknown member behaviours. *)

module Unknown = struct
  type t = { m : bool }
  let make m = { m }
  let m v = v.m

  let skip_jsont =
    Jsont.Obj.map ~kind:"unknown-skip" make
    |> Jsont.Obj.mem "m" Jsont.bool ~enc:m
    |> Jsont.Obj.skip_unknown
    |> Jsont.Obj.finish

  let error_jsont =
    Jsont.Obj.map ~kind:"unknown-skip" make
    |> Jsont.Obj.mem "m" Jsont.bool ~enc:m
    |> Jsont.Obj.error_unknown
    |> Jsont.Obj.finish

  let keep_jsont : (t * int String_map.t) Jsont.t =
    let unknown = Jsont.Obj.Mems.string_map Jsont.int in
    Jsont.Obj.map ~kind:"unknown-keep" (fun m imap -> make m, imap)
    |> Jsont.Obj.mem "m" Jsont.bool ~enc:(fun (v, _) -> m v)
    |> Jsont.Obj.keep_unknown unknown ~enc:snd
    |> Jsont.Obj.finish
end

module Unknown_data = struct
  let u0 = {| { "m": true } |}
  let u1 = {| { "m": true, "u0": 0, "u1": 1 } |}
  let u2 = {| { "u": 0, "m": true } |}
end

(* Object cases *)

module Cases = struct
  (* There are two ways to encode object cases in OCaml, either as a toplevel
     variant or as a record with a field that is a variant. With the design
     we have the encoding is mostly the same. This is the JSON we deal with:

     { "type": "author",
       "name": "…",
       "pseudo": "…",
       "book_count": 1 }

     { "type": "editor",
       "name": "…",
       "publisher": "…" } *)

  module Person_top = struct (* Toplevel variant *)
    module Author = struct
      type t = { name : string; pseudo : string; book_count : int; }
      let make name book_count pseudo = { name; pseudo; book_count }
      let name a = a.name
      let book_count a = a.book_count
      let pseudo a = a.pseudo
      let jsont =
        Jsont.Obj.map ~kind:"Author" make
        |> Jsont.Obj.mem "name" Jsont.string ~enc:name
        |> Jsont.Obj.mem "book_count" Jsont.int ~enc:book_count
        |> Jsont.Obj.mem "pseudo" Jsont.string ~enc:pseudo
        |> Jsont.Obj.finish
    end

    module Editor = struct
      type t = { name : string; publisher : string }
      let make name publisher = { name; publisher}
      let name e = e.name
      let publisher e = e.publisher
      let jsont =
        Jsont.Obj.map ~kind:"Editor" make
        |> Jsont.Obj.mem "name" Jsont.string ~enc:name
        |> Jsont.Obj.mem "publisher" Jsont.string ~enc:publisher
        |> Jsont.Obj.finish
    end

    type t = Author of Author.t | Editor of Editor.t

    let author a = Author a
    let editor e = Editor e

    let jsont =
      let case_a = Jsont.Obj.Case.map "author" Author.jsont ~dec:author in
      let case_e = Jsont.Obj.Case.map "editor" Editor.jsont ~dec:editor in
      let cases = Jsont.Obj.Case.[make case_a; make case_e] in
      let enc_case = function
      | Author a -> Jsont.Obj.Case.value case_a a
      | Editor e -> Jsont.Obj.Case.value case_e e
      in
      Jsont.Obj.map ~kind:"Person" Fun.id
      |> Jsont.Obj.case_mem "type"
          Jsont.string ~tag_to_string:Fun.id ~enc:Fun.id ~enc_case cases
      |> Jsont.Obj.finish
  end

  module Person_field = struct (* Variant in a field *)
    type author = { pseudo : string; book_count : int }
    let make_author pseudo book_count = { pseudo; book_count }
    let pseudo a = a.pseudo
    let book_count a = a.book_count
    let author_jsont =
      Jsont.Obj.map ~kind:"Author" make_author
      |> Jsont.Obj.mem "pseudo" Jsont.string ~enc:pseudo
      |> Jsont.Obj.mem "book_count" Jsont.int ~enc:book_count
      |> Jsont.Obj.finish

    type editor = { publisher : string; }
    let make_editor publisher = { publisher }
    let publisher e = e.publisher
    let editor_jsont =
      Jsont.Obj.map ~kind:"Editor" make_editor
      |> Jsont.Obj.mem "publisher" Jsont.string ~enc:publisher
      |> Jsont.Obj.finish

    type type' = Author of author | Editor of editor
    let author a = Author a
    let editor e = Editor e

    type t = { type' : type'; name : string }
    let make type' name = { type'; name }
    let type' v = v.type'
    let name v = v.name

    let jsont =
      let case_a = Jsont.Obj.Case.map "author" author_jsont ~dec:author in
      let case_e = Jsont.Obj.Case.map "editor" editor_jsont ~dec:editor in
      let cases = Jsont.Obj.Case.[make case_a; make case_e] in
      let enc_case = function
      | Author a -> Jsont.Obj.Case.value case_a a
      | Editor e -> Jsont.Obj.Case.value case_e e
      in
      Jsont.Obj.map ~kind:"Person" make
      |> Jsont.Obj.case_mem "type"
        ~tag_to_string:Fun.id Jsont.string ~enc:type' ~enc_case cases
      |> Jsont.Obj.mem "name" Jsont.string ~enc:name
      |> Jsont.Obj.finish
  end

  module Keep_unknown = struct
    type a = string String_map.t
    let a_jsont =
      let unknown = Jsont.Obj.Mems.string_map Jsont.string in
      Jsont.Obj.map ~kind:"A" Fun.id
      |> Jsont.Obj.keep_unknown unknown ~enc:Fun.id
      |> Jsont.Obj.finish

    type b = { name : string }
    let name b = b.name
    let b_jsont =
      Jsont.Obj.map ~kind:"B" (fun name -> { name })
      |> Jsont.Obj.mem "name" Jsont.string ~enc:name
      |> Jsont.Obj.error_unknown
      |> Jsont.Obj.finish

    type type' = A of a | B of b
    let a a = A a
    let b b = B b
    type t = { type' : type'; unknown : Jsont.json }
    let make type' unknown = { type'; unknown }
    let type' v = v.type'
    let unknown v = v.unknown
    let equal v0 v1 = match v0.type', v1.type' with
    | A a0, A a1 ->
        String_map.equal String.equal a0 a1 &&
        Jsont.Json.equal v0.unknown v1.unknown
    | B b0, B b1 ->
        String.equal b0.name b1.name &&
        Jsont.Json.equal v0.unknown v1.unknown
    | _, _ -> false

    let pp ppf v = B0_std.Fmt.string ppf "<value>"

    let jsont =
      let case_a = Jsont.Obj.Case.map "A" a_jsont ~dec:a in
      let case_b = Jsont.Obj.Case.map "B" b_jsont ~dec:b in
      let cases = Jsont.Obj.Case.[make case_a; make case_b] in
      let enc_case = function
      | A a -> Jsont.Obj.Case.value case_a a
      | B b -> Jsont.Obj.Case.value case_b b
      in
      Jsont.Obj.map ~kind:"Keep_unknown" make
      |> Jsont.Obj.case_mem "type"
        ~tag_to_string:Fun.id Jsont.string ~enc:type' ~enc_case cases
      |> Jsont.Obj.keep_unknown Jsont.json_mems ~enc:unknown
      |> Jsont.Obj.finish
  end

end

module Cases_data = struct
  let author0_top, author0_field =
    let name = "Jane" and book_count = 2 and pseudo = "Jude" in
    Cases.Person_top.Author { name; book_count; pseudo },
    { Cases.Person_field.type' = Author { book_count; pseudo }; name }

  let invalid_miss = (* Missing type field. *)
    {| { "name": "Jane", "tope": "ha", "tape": "ha",
         "book_count": 2, "pseudo": "Jude" }|}

  let invalid_case =
    {| { "type": "reader", "name": "Jane" }|}

  let author0 =
    {| { "type": "author", "name": "Jane", "book_count": 2, "pseudo": "Jude" }|}

  let author0' = (* out of order case field in the middle *)
    {| { "name": "Jane", "book_count": 2, "type": "author", "pseudo": "Jude" }|}

  let editor0_top, editor0_field =
    let name = "Joe" and publisher = "Red books" in
    Cases.Person_top.Editor { name; publisher },
    { Cases.Person_field.type' = Editor { publisher }; name }

  let editor0 =
    {| { "type": "editor", "name": "Joe", "publisher": "Red books"  } |}

  let editor0' = (* out of order case field at the end *)
    {| { "name": "Joe", "publisher": "Red books", "type": "editor" } |}

  let unknown_a =
    {| { "m1": "n", "type": "A", "m0": "o" } |}

  let unknown_b =
    {| { "type": "B", "m1": "v1", "name": "ha", "m2": 0 } |}

  let unknown_a_value =
    let unknown =
      Jsont.Json.(obj [mem (name "m0") (string "o");
                       mem (name "m1") (string "n")])
    in
    Cases.Keep_unknown.make (A String_map.empty) unknown

  let unknown_a_a_value =
    String_map.empty
    |> String_map.add "m0" "o"
    |> String_map.add "m1" "n"
    |> String_map.add "type" "A"

  let unknown_a_no_a_unknown = "{\n  \"type\": \"A\"\n}"
  let unknown_a_no_a_unknown_value =
    (* Since the map should be ignored since the case object overides it *)
    let unknown = Jsont.Json.obj [] in
    Cases.Keep_unknown.make (A String_map.(empty |> add "bli" "bla")) unknown

  let unknown_b_value =
    let unknown =
      Jsont.Json.(obj [mem (name "m1") (string "v1");
                       mem (name "m2") (number 0.0)])
    in
    Cases.Keep_unknown.make (B { name = "ha" }) unknown
end

(* Type recursion *)

module Tree = struct
  type 'a tree = Empty | Node of 'a tree * 'a *  'a tree

  let rec pp pp_v ppf = function
  | Empty -> Format.fprintf ppf "Empty"
  | Node (l, v, r) ->
      Format.fprintf ppf "@[Node @[<1>(%a,@ %a,@ %a)@]@]"
        (pp pp_v) l pp_v v (pp pp_v) r

  (* Encoded with null for Empty and nodes with:

     { "left": …,
       "value": …,
       "right": … }

     and null is used for empty. *)
  let jsont_with_null t =
    let rec tree = lazy begin
      let empty = Jsont.null Empty in
      let node =
        let not_a_node () = failwith "not a node" in
        let value = function Node (_, v, _) -> v | _ -> not_a_node () in
        let left = function Node (l, _, _) -> l | _ -> not_a_node () in
        let right = function Node (_, _, r) -> r | _ -> not_a_node () in
        Jsont.Obj.map ~kind:"node" (fun l v r -> Node (l, v, r))
        |> Jsont.Obj.mem ~enc:left "left" (Jsont.rec' tree)
        |> Jsont.Obj.mem ~enc:value "value" t
        |> Jsont.Obj.mem ~enc:right "right" (Jsont.rec' tree)
        |> Jsont.Obj.finish
      in
      let enc = function Empty -> empty | Node _ -> node in
      Jsont.any ~kind:"tree" ~dec_null:empty ~dec_obj:node ~enc ()
    end
    in
    Lazy.force tree

  (* Encoded as two cases :

     { "type": "empty" }

     { "type": "node",
       "left": …,
       "value": …,
       "right": … } *)

  let jsont_with_cases t =
    let rec tree = lazy begin
      let leaf_jsont = Jsont.Obj.map Empty |> Jsont.Obj.finish in
      let node_jsont =
        let not_a_node () = failwith "not a node" in
        let value = function Node (_, v, _) -> v | _ -> not_a_node () in
        let left = function Node (l, _, _) -> l | _ -> not_a_node () in
        let right = function Node (_, _, r) -> r | _ -> not_a_node () in
        Jsont.Obj.map (fun l v r -> Node (l, v, r))
        |> Jsont.Obj.mem ~enc:left "left" (Jsont.rec' tree)
        |> Jsont.Obj.mem ~enc:value "value" t
        |> Jsont.Obj.mem ~enc:right "right" (Jsont.rec' tree)
        |> Jsont.Obj.finish
      in
      let case_leaf = Jsont.Obj.Case.map "empty" leaf_jsont ~dec:Fun.id in
      let case_node = Jsont.Obj.Case.map "node" node_jsont ~dec:Fun.id in
      let enc_case = function
      | Empty as v -> Jsont.Obj.Case.value case_leaf v
      | Node _ as v -> Jsont.Obj.Case.value case_node v
      in
      let cases = Jsont.Obj.Case.[ make case_leaf; make case_node ] in
      Jsont.Obj.map ~kind:"tree" Fun.id
      |> Jsont.Obj.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
      |> Jsont.Obj.finish
    end
    in
    Lazy.force tree

end

module Tree_data = struct
  let empty = Tree.Empty
  let empty_null = {| null |}
  let empty_cases = {| { "type": "empty" } |}

  let tree0 = Tree.Node (Node (Node (Empty, 1, Empty),
                               2,
                               Empty),
                         3,
                         Node (Empty, 4, Empty))

  let tree0_null =
    {| { "left": { "left": { "left": null, "value": 1, "right": null },
                   "value": 2,
                   "right": null },
         "value": 3,
         "right": { "left": null, "value": 4, "right": null } } |}

  let tree0_cases = (* Case member not in order to check decode delays. *)
    {| { "left": { "type": "node",
                   "left": { "type": "node",
                             "left": { "type": "empty" },
                             "right": { "type": "empty" },
                             "value": 1 },
                   "value": 2,
                   "right": { "type" : "empty" }},
         "value": 3,
         "type": "node",
         "right": { "type": "node",
                    "left": { "type" : "empty" },
                    "value": 4,
                    "right": { "type" : "empty" }}} |}
end
