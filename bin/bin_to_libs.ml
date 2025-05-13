open Dune_deps

let to_json r =
  `List
    (List.map
       (fun (b, ls) ->
          `Assoc [ "bin", `String b; "libs", `List (List.map (fun l -> `String l) ls) ])
       r)
;;

let find_label_of_id dep graph =
  match Filterable.resolve_name graph dep with
  | [] -> failwith "not found"
  | [ name ] -> name.label
  | name :: _ ->
    (* what to do here?? *)
    name.label
;;

let find_lib_deps name graph =
  Filter.deps graph [ name ]
  |> Filter.no_exe
  |> Filterable.to_list
  |> List.map (fun { Filterable.label; _ } -> label)
;;

let bin_to_libs deps graph =
  let assoc =
    List.map (fun dep -> find_label_of_id dep graph, find_lib_deps dep graph) deps
  in
  let json : Yojson.t = to_json assoc in
  json
;;
