open Printf

module StringMap = Map.Make(String)

let variant_name = function
  | `Array _ -> "`Array"
  | `Binary _ -> "`Binary"
  | `Boolean _ -> "`Boolean"
  | `DateTime _ -> "`DateTime"
  | `Double _ -> "`Double"
  | `Int _ -> "`Int"
  | `String _ -> "`String"
  | `Struct _ -> "`Struct"

let dot_to_underscore s =
  let s = String.copy s in
  let len = String.length s in
  for i = 0 to len - 1 do
    match s.[i] with
      | '.' -> s.[i] <- '_'
      | _ -> ()
  done;
  s

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
           | `Array params ->
               StringMap.add
                 module_name
                 ((meth_name, Some params) :: StringMap.find module_name acc)
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

let () =
  printf "class client url =
object (self)
  val rpc = new XmlRpc.client url

%s
end
"
    (String.concat "\n"
       (values
          (StringMap.mapi
             (fun module_name meths ->
                let impls =
                  String.concat "\n\n"
                    (List.map
                       (function
                          | (name, Some params) ->
                              let result = List.hd params in
                              let params = Array.of_list (List.tl params) in
                              let param_names =
                                String.concat " "
                                  (Array.to_list
                                     (Array.mapi
                                        (fun i _ ->
                                           sprintf "p%d" i)
                                        params)) in
                              let param_values =
                                String.concat "; "
                                  (Array.to_list
                                     (Array.mapi
                                        (fun i param ->
                                           sprintf "%s p%d"
                                             (variant_name param)
                                             i)
                                        params)) in
                              let result_type = variant_name result in
                              sprintf "    method %s %s =
      match rpc#call \"%s.%s\" [%s] with
        | %s r -> r
        | _ -> failwith \"unexpected result type\""
                                (dot_to_underscore name)
                                (if param_names = "" then "()" else param_names)
                                module_name
                                name
                                param_values
                                result_type
                          | (name, None) ->
                              sprintf "    method %s params =
      rpc#call \"%s.%s\" params"
                                (dot_to_underscore name)
                                module_name
                                name)
                       meths) in
                sprintf "  method %s = object
%s
  end
"
                  module_name
                  impls)
             module_map)))
