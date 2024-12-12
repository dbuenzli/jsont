(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(** JSON-RPC codec https://www.jsonrpc.org/ *)

(* JSON-RPC version *)

type jsonrpc = [`V2]
let jsonrpc_jsont = Jsont.enum ["2.0", `V2]

(* JSON-RPC identifiers *)

type id = [ `String of string | `Number of float | `Null ]
let id_jsont : id Jsont.t =
  let null = Jsont.null `Null in
  let string =
    let dec s = `String s in
    let enc = function `String s -> s | _ -> assert false in
    Jsont.map ~dec ~enc Jsont.string
  in
  let number =
    let dec n = `Number n in
    let enc = function `Number n -> n | _ -> assert false in
    Jsont.map ~dec ~enc Jsont.number
  in
  let enc = function
  | `Null -> null | `String _ -> string | `Number _ -> number
  in
  Jsont.any ~dec_null:null ~dec_string:string ~dec_number:number ~enc ()

(* JSON-RPC request object *)

type params = Jsont.json (* An array or object *)
let params_jsont =
  let enc = function
  | Jsont.Object _ | Jsont.Array _ -> Jsont.json
  | j ->
      let meta = Jsont.Meta.none in
      let fnd = Jsont.Sort.to_string (Jsont.Json.sort j) in
      Jsont.Error.expected meta "object or array" ~fnd
  in
  let kind = "JSON-RPC params" in
  Jsont.any ~kind ~dec_array:Jsont.json ~dec_object:Jsont.json ~enc ()

type request =
  { jsonrpc : jsonrpc;
    method' : string;
    params : params option;
    id : id option; }

let request jsonrpc method' params id = { jsonrpc; method'; params; id }
let request_jsont : request Jsont.t =
  Jsont.Object.map request
  |> Jsont.Object.mem "jsonrpc" jsonrpc_jsont ~enc:(fun r -> r.jsonrpc)
  |> Jsont.Object.mem "method" Jsont.string ~enc:(fun r -> r.method')
  |> Jsont.Object.opt_mem "params" params_jsont ~enc:(fun r -> r.params)
  |> Jsont.Object.opt_mem "id" id_jsont ~enc:(fun r -> r.id)
  |> Jsont.Object.finish

(* JSON-RPC error objects *)

type error =
  { code : int;
    message : string;
    data : Jsont.json option; }

let error code message data = { code; message; data }
let error_jsont =
  Jsont.Object.map error
  |> Jsont.Object.mem "code" Jsont.int ~enc:(fun e -> e.code)
  |> Jsont.Object.mem "message" Jsont.string ~enc:(fun e -> e.message)
  |> Jsont.Object.opt_mem "data" Jsont.json ~enc:(fun e -> e.data)
  |> Jsont.Object.finish

(* JSON-RPC response object *)

type response =
  { jsonrpc : jsonrpc;
    value : (Jsont.json, error) result;
    id : id; }

let response jsonrpc result error id : response =
  let err_both () =
    Jsont.Error.msgf Jsont.Meta.none "Both %a and %a members are defined"
      Jsont.Repr.pp_code "result" Jsont.Repr.pp_code "error"
  in
  let err_none () =
    Jsont.Error.msgf Jsont.Meta.none "Missing either %a or %a member"
      Jsont.Repr.pp_code "result" Jsont.Repr.pp_code "error"
  in
  match result, error with
  | Some result, None -> { jsonrpc; value = Ok result; id }
  | None, Some error -> { jsonrpc; value = Error error; id }
  | Some _ , Some _ -> err_both ()
  | None, None -> err_none ()

let response_result r = match r.value with Ok v -> Some v | Error _ -> None
let response_error r = match r.value with Ok _ -> None | Error e -> Some e

let response_jsont : response Jsont.t =
  Jsont.Object.map response
  |> Jsont.Object.mem "jsonrpc" jsonrpc_jsont ~enc:(fun r -> r.jsonrpc)
  |> Jsont.Object.opt_mem "result" Jsont.json ~enc:response_result
  |> Jsont.Object.opt_mem "error" error_jsont ~enc:response_error
  |> Jsont.Object.mem "id" id_jsont ~enc:(fun r -> r.id)
  |> Jsont.Object.finish
