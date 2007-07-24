let () =
  let server = new XmlRpcServer.netplex () in
  server#register "demo.addTwoNumbers"
    (function
       | [`Int x; `Int y] -> `Int (x + y)
       | [`Int x; `Double y] -> `Double (float_of_int x +. y)
       | [`Double x; `Int y] -> `Double (x +. float_of_int y)
       | [`Double x; `Double y] -> `Double (x +. y)
       | [`String x; `String y] -> `Double (float_of_string x
                                            +. float_of_string y)
       | _ -> XmlRpcServer.invalid_params ());
  server#run ()
