open Printf

module StringMap = Map.Make(String)

let variant_name = function
  | `String "array" -> Some "`Array"
  | `String "base64" -> Some "`Binary"
  | `String "boolean" -> Some "`Boolean"
  | `String "dateTime.iso8601" -> Some "`DateTime"
  | `String "double" -> Some "`Double"
  | `String "int" -> Some "`Int"
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
  if Array.length Sys.argv < 2
  then (printf "Usage: %s url\n" Sys.argv.(0); exit 1)
  else ()

let url = Sys.argv.(1)
let rpc = new XmlRpc.client url

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
        (match variant_name result with
           | Some result_type ->
               sprintf "    method %s %s =
      match rpc#call \"%s.%s\" [%s] with
        | %s r -> r
        | other -> raise (Type_error (XmlRpc.dump other))"
                 func_name
                 (if param_names = "" then "()" else param_names)
                 module_name
                 name
                 param_values
                 result_type
           | None ->
               sprintf "    method %s %s =
      rpc#call \"%s.%s\" [%s]"
                 func_name
                 (if param_names = "" then "()" else param_names)
                 module_name
                 name
                 param_values)
    | _ -> failwith "method signature was not an array"

let impl_without_signature module_name func_name name =
  sprintf "    method %s params =
      rpc#call \"%s.%s\" params"
    func_name
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

let () =
  printf "(* Automatically generated by running genclient with an
   XML-RPC server located at the following URL:

   %s
*)

exception Type_error of string

class client url =
object (self)
  val rpc = new XmlRpc.client url
  method rpc = rpc

%s
end
" url objects
