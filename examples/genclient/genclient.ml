(*
 * XmlRpc Light, a small XmlRpc library based on Xml Light and Ocamlnet
 * Copyright (C) 2007-2008 Dave Benjamin (dave@ramenlabs.com)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

open Printf

module StringMap = Map.Make(String)

let url = ref ""

let use_int32 = ref false
let use_multicall = ref false
let verbose = ref false

let variant_name = function
  | `String "array" -> Some "`Array"
  | `String "base64" -> Some "`Binary"
  | `String "boolean" -> Some "`Boolean"
  | `String "dateTime.iso8601" -> Some "`DateTime"
  | `String "double" -> Some "`Double"
  | `String "int" -> Some (if !use_int32 then "`Int32" else "`Int")
  | `String "string" -> Some "`String"
  | `String "struct" -> Some "`Struct"
  | `String "undefined" -> None
  | _ as it -> failwith (XmlRpc.dump it)

let dot_to_underscore s =
  let s = String.copy s in
  let len = String.length s in
  for i = 0 to len - 1 do
    match s.[i] with
      | '.' -> s.[i] <- '_'
      | _ -> ()
  done;
  s

let () =
  let specs = 
    ["-i", Arg.Set use_int32, "Use int32 for all integers";
     "-m", Arg.Set use_multicall, "Generate lazy interface using multicall";
     "-v", Arg.Set verbose, "Display verbose debugging output"] in
  let usage = (sprintf "Usage: %s [-i] [-m] [-v] url" Sys.argv.(0)) in
  Arg.parse specs (fun url' -> url := url') usage;
  if !url = "" then (Arg.usage specs usage; exit 2)

let rpc = new XmlRpc.client ~debug:!verbose !url

let methods = rpc#call "system.listMethods" []

let mc = new XmlRpc.multicall rpc
let signatures =
  match methods with
    | `Array methods ->
        List.map
          (function
             | `String meth ->
                 (meth, mc#call "system.methodSignature" [`String meth])
             | _ -> failwith "got non-string for method name")
          methods
    | _ -> failwith "got non-array from system.listMethods"

let module_map =
  List.fold_left
    (fun acc (name, signature) ->
       let dot_index = String.index name '.' in
       let module_name = String.sub name 0 dot_index in
       let meth_name =
         String.sub name
           (dot_index + 1)
           (String.length name - dot_index - 1) in
       let acc =
         if StringMap.mem module_name acc
         then acc
         else StringMap.add module_name [] acc in
       try
         match Lazy.force signature with
           | `Array param_lists ->
               StringMap.add
                 module_name
                 ((meth_name, Some param_lists) :: StringMap.find module_name acc)
                 acc
           | _ ->
               StringMap.add
                 module_name
                 ((meth_name, None) :: StringMap.find module_name acc)
                 acc
       with
         | XmlRpc.Error _ ->
             StringMap.add
               module_name
               ((meth_name, None) :: StringMap.find module_name acc)
               acc
         | Xml.Error (msg, pos) ->
             prerr_endline (Xml.error_msg msg);
             acc)
    StringMap.empty
    signatures

let values map =
  let result = ref [] in
  StringMap.iter (fun _ v -> result := v :: !result) map;
  List.rev !result

let impl_with_signature module_name func_name name =
  function
    | `Array params ->
        let result = List.hd params in
        let params = Array.of_list (List.tl params) in
        let param_names =
          String.concat " "
            (Array.to_list
               (Array.mapi
                  (fun i _ ->
                     sprintf "_%d" i)
                  params)) in
        let param_values =
          String.concat "; "
            (Array.to_list
               (Array.mapi
                  (fun i param ->
                     match variant_name param with
                       | Some n ->
                           sprintf "%s _%d" n i
                       | None ->
                           sprintf "_%d" i)
                  params)) in
        let mc_init =
          if !use_multicall
          then "if mc#executed then mc <- new XmlRpc.multicall rpc;\n      "
          else "" in
        let rpc_call = if !use_multicall then "mc#call" else "rpc#call" in
        let begin_lazy_force, lazy_force, end_lazy_force =
          if !use_multicall
          then "lazy (", "Lazy.force ", ")"
          else "", "",  "" in
        (match variant_name result with
           | Some result_type ->
               sprintf "    method %s %s =
      %slet result = %s \"%s.%s\" [%s] in
      %smatch %sresult with
        | %s r -> r
        | other -> raise (Type_error (XmlRpc.dump other))%s"
                 func_name
                 (if param_names = "" then "()" else param_names)
                 mc_init
                 rpc_call
                 module_name
                 name
                 param_values
                 begin_lazy_force
                 lazy_force
                 (if result_type = "`Int32"
                  then "`Int r -> Int32.of_int r | `Int32"
                  else result_type)
                 end_lazy_force
           | None ->
               sprintf "    method %s %s =
      %s%s \"%s.%s\" [%s]"
                 func_name
                 (if param_names = "" then "()" else param_names)
                 mc_init
                 rpc_call
                 module_name
                 name
                 param_values)
    | _ -> failwith "method signature was not an array"

let impl_without_signature module_name func_name name =
  let mc_init =
    if !use_multicall
    then "if mc#executed then mc <- new XmlRpc.multicall rpc;\n      "
    else "" in
  let rpc_call = if !use_multicall then "mc#call" else "rpc#call" in
  sprintf "    method %s params =
      %s%s \"%s.%s\" params"
    func_name
    mc_init
    rpc_call
    module_name
    name

let impls module_name meths =
  let seen_names = Hashtbl.create 0 in
  let make_func_name name =
    let base_name = dot_to_underscore name in
    let func_name = ref base_name in
    let counter = ref 1 in
    while Hashtbl.mem seen_names !func_name do
      incr counter;
      func_name := base_name ^ (string_of_int !counter)
    done;
    Hashtbl.replace seen_names !func_name 0;
    !func_name in
  String.concat "\n\n"
    (List.flatten
       (List.map
          (function
             | (name, Some param_lists) ->
                 List.map
                   (fun signature ->
                      impl_with_signature
                        module_name
                        (make_func_name name)
                        name
                        signature)
                   param_lists
             | (name, None) ->
                 [impl_without_signature
                    module_name
                    (make_func_name name)
                    name])
          meths))

let objects =
  String.concat "\n"
    (values
       (StringMap.mapi
          (fun module_name meths ->
             sprintf "  method %s = object
%s
  end
"
               module_name
               (impls module_name meths))
          module_map))

let multicall =
  if !use_multicall
  then "
  val mutable mc = new XmlRpc.multicall rpc
  method mc = mc
"
  else ""

let () =
  printf "(* Automatically generated from an XML-RPC server
   by running the following command:

   %s
*)

exception Type_error of string

class client url =
  let rpc = new XmlRpc.client url in
object (self)
  method rpc = rpc
%s
%s
end
" (String.concat " " (Array.to_list Sys.argv)) multicall objects
