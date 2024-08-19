let parse_input input =
  let fields = String.split_on_char ',' input in
  List.map (fun field ->
    match String.split_on_char ':' field with
    | [name; typ] -> (name, typ)
    | _ -> failwith "Invalid input format"
  ) fields

let generate_json fields =
  let json_fields = List.map  (fun (name, typ) ->
    let value = match typ with
      | "string" -> `String ""
      | "number" -> `Int 1
      | "boolean" -> `Bool false
      | "array" -> `List []
      | "object" -> `Assoc []
      | "null" -> `Null
      | _ -> failwith "Unsupported type"
    in
    (name, value)
  ) fields in
  `Assoc json_fields

let () =
    print_endline "Enter the fields in format 'your_var_name:json_type,another_var_name:another_json_type'";
    let input = read_line () in
    let fields = parse_input input in
    let json = generate_json fields in
    print_endline (Yojson.Basic.pretty_to_string json)