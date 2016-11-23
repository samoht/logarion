open Cmdliner

let info =
  let doc = "Command based interface for a Logarion." in
  let man = [
      `S "BUGS";
      `P "Submit bugs https://github.com/orbifx/logarion/issues.";
    ] in
  Term.info "logarion" ~version:"0.1.0" ~doc ~man

let operation =
  let doc = "Logarion operation" in
  Arg.(value & pos 0 string "help" & info [] ~docv:"operation" ~doc)
let logarion operation =
  match operation with
  | "create" -> print_endline "create"
  | _ -> print_endline "help"
            
let logarion_term =
  Term.(const logarion $ operation)
  
let () =
  match Term.eval (logarion_term, info) with
    `Error _ -> exit 1 | _ -> exit 0
