(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t =
  { name : string;
    pseudo : string;
    book_count : int;
    address : string;
    email : string; }

let make name book_count pseudo address email =
  { name; pseudo; book_count; address; email }
  
let make_rand st =
  let rand_name st ~max =
    let alpha st = Char.chr (0x61 + Random.State.int st (0x7A - 0x61 + 1)) in
    String.init (Random.State.int st (max + 1)) (fun _ -> alpha st)
  in
  let name = rand_name st ~max:15 in
  let pseudo = rand_name st ~max:15 in
  let book_count = Random.State.int st 20 in
  let address = rand_name st ~max:20 in
  let email = rand_name st ~max:20 in
  { name; pseudo; book_count; address; email }

let name p = p.name
let pseudo p = p.pseudo
let book_count p = p.book_count
let address p = p.address
let email p = p.email
                
let author_jsont =
  Jsont.Object.map ~kind:"Author" make
  |> Jsont.Object.mem "name" Jsont.string ~enc:name
  |> Jsont.Object.mem "book_count" Jsont.int ~enc:book_count
  |> Jsont.Object.mem "pseudo" Jsont.string ~enc:pseudo
  |> Jsont.Object.mem "address" Jsont.string ~enc:address
  |> Jsont.Object.mem "email" Jsont.string ~enc:email
  |> Jsont.Object.finish
       
let author_jsont_syntax =
  let open Jsont.Object.Syntax in
  define ~kind:"Author" @@
  let+ name = mem "name" Jsont.string ~enc:name
  and+ book_count = mem "book_count" Jsont.int ~enc:book_count
  and+ pseudo = mem "pseudo" Jsont.string ~enc:pseudo
  and+ address = mem "address" Jsont.string ~enc:address
  and+ email = mem "email" Jsont.string ~enc:email
  in
  { name; book_count; pseudo; address; email }

let authors_jsont = Jsont.list author_jsont
let authors_jsont_syntax = Jsont.list author_jsont_syntax    

let streaming_gen st len make_rand jsont = (* Just for fun. *)
  let enc f acc () =
    let rec loop max i f acc =
      if i > max then acc else loop max (i + 1) f (f acc i (make_rand st))
    in
    loop (len - 1) 0 f acc
  in
  Jsont.Array.array @@
  Jsont.Array.map ~enc:Jsont.Array.{enc} jsont
    
let generate n file =
  let st = Random.State.make_self_init () in
  Out_channel.with_open_text file @@ fun oc ->
  let w = Bytesrw.Bytes.Writer.of_out_channel oc in
  let format = Jsont.Indent in
  let gen = streaming_gen st n make_rand author_jsont in
  match Jsont_bytesrw.encode ~format gen () ~eod:true w with
  | Ok () -> 0
  | Error e -> Printf.eprintf "%s\n%!" e; 1

let decode file let_syntax =
  let jsont = if let_syntax then authors_jsont_syntax else authors_jsont in
  In_channel.with_open_text file @@ fun ic ->
  let w = Bytesrw.Bytes.Reader.of_in_channel ic in
  Sys.opaque_identity @@
  match Jsont_bytesrw.decode jsont w with
  | Ok v -> Sys.opaque_identity (ignore v); 0
  | Error e -> Printf.eprintf "%s\n%!" e; 1

let main () =
  let usage = "Usage: test_syntax [OPTION]… FILE.json" in
  let gen = ref false in
  let n = ref 1_000_000 in
  let file = ref None in
  let let_syntax = ref false in
  let args =
    [ "--gen", Arg.Set gen, "Generate data";
      "--n", Arg.Set_int n, "Number of records";
      "--let-syntax", Arg.Set let_syntax, "Use let syntax"; ]
  in
  let pos s = match !file with
  | Some _ -> raise (Arg.Bad (Printf.sprintf "Don't know what to do with %S" s))
  | None -> file := Some s
  in
  Arg.parse args pos usage;
  match !file with
  | None -> prerr_endline "No file specified"; 1
  | Some file -> if !gen then generate !n file else decode file !let_syntax

let () = if !Sys.interactive then () else exit (main ())


