open Dune_deps

type config = {
  roots : string list;
  exclude : string list;
  no_ext : bool;
  deps : string list;
}

let to_json r =
  `List
    (List.map
      (fun (b, ls) ->
        `Assoc
          [
            ("bin", `String b);
            ("libs", `List (List.map (fun l -> `String l) ls))
          ]
          )
      r)

let find_label_of_id dep graph =
  match Filterable.resolve_name graph dep with
  | [] -> failwith "not found"
  | [name] -> name.label
  | name :: _ ->
      (* what to do here?? *)
      name.label

let find_lib_deps name graph =
  Filter.deps graph [name]
  |> Filter.no_exe
  |> Filterable.to_list
  |> List.map (fun {Filterable.label; _} -> label)

let optimistic_run {roots; exclude; no_ext; deps;} =
  let graph =
    Find.find_dune_files roots ~exclude
    |> Dune.load_files
  in
  let graph =
    if no_ext then Filter.no_ext graph
    else graph
  in
  let deps =
    match deps with
    | [] ->
        Filterable.filter graph (fun node -> node.kind = Exe)
        |> Filterable.to_list
        |> List.map (fun {Filterable.id; _} -> id)
    | _ :: _ -> deps
  in
  let graph = Filter.deps_or_revdeps graph ~deps ~revdeps:[] in
  let r =
    List.map
      (fun dep -> (find_label_of_id dep graph, find_lib_deps dep graph))
      deps
  in
  let r : Yojson.t = to_json r in
  Yojson.to_file "/dev/stdout" r

let safe_run config =
  try optimistic_run config
  with
  | Failure msg ->
      Printf.eprintf "Error: %s\n%!" msg;
      exit 1
  | e ->
      let trace = Printexc.get_backtrace () in
      Printf.eprintf "Error: exception %s\n%s%!"
        (Printexc.to_string e)
        trace

(* cmdline definitions *)
open Cmdliner

let roots_term =
  let info =
    Arg.info []
      ~docv:"ROOT"
      ~doc:"$(docv) can be either a folder in which 'dune' files are to be \
            found recursively, or simply a 'dune' file. Multiple $(docv) \
            arguments are supported. If no $(docv) is specified, the current \
            folder is used."
  in
  Arg.value (Arg.pos_all Arg.file ["."] info)

let exclude_term =
  let info =
    Arg.info ["exclude"; "x"]
      ~docv:"ROOT"
      ~doc:"Ignore folder or file $(docv) when scanning the file tree \
            for 'dune' files."
  in
  Arg.value (Arg.opt_all Arg.string [] info)

let no_ext_term =
  let info =
    Arg.info ["no-ext"]
      ~doc:"Omit external libraries."
  in
  Arg.value (Arg.flag info)

let deps_term =
  let info =
    Arg.info ["deps"; "d"]
      ~docv:"NAME"
      ~doc:"Targets to list the dependencies of."
  in
  Arg.value (Arg.opt_all Arg.string [] info)

let cmdline_term =
  let combine roots exclude no_ext deps =
    { roots; exclude; no_ext; deps; }
  in
  Term.(const combine
        $ roots_term
        $ exclude_term
        $ no_ext_term
        $ deps_term
       )

let doc =
  "extract a list of dependency for a target in a dune project"

let parse_command_line () =
  let info =
    Cmd.info
      ~doc
      "dune-deps-lib-deps-of-bins"
  in
  match Cmd.eval_value (Cmd.v info cmdline_term) with
  | Error _ -> exit 1
  | Ok (`Version | `Help) -> exit 0
  | Ok (`Ok config) -> config

let main () =
  Printexc.record_backtrace true;
  let config = parse_command_line () in
  safe_run config

let () = main ()
