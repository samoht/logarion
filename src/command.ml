open Cmdliner

let create =
  let title =
    let doc = "(Optional) title for new article" in
    Arg.(value & pos 0 string "" & info [] ~docv:"TITLE" ~doc)
  in
  let doc = "create a new article and start $EDITOR" in
  let man = [
      `S "DESCRIPTION";
      `P "Create a new article with a generated UUID.
          If `title` is not provided, 'Draft' is used."]
  in
  let create_f title =
    let cfg = Logarion.Configuration.of_filename "logarion.toml" in
    let t = match title with "" -> "Draft" | _ -> title in
    Logarion.Entry.to_file cfg Ymd.({ (blank_ymd ()) with meta = { (blank_meta ()) with title = t }})
    |> Lwt_main.run
  in
  Term.(const create_f $ title),
  Term.info "create" ~doc ~man

let default_cmd =
  let doc = "an article collection & publishing system" in
  let man = [ `S "BUGS"; `P "Submit bugs https://github.com/orbifx/logarion/issues."; ] in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "logarion" ~version:"0.1.0" ~doc ~man

let cmds = [ create; ]

let () = match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1 | _ -> exit 0
