open Cmdliner
module C = Logarion.Configuration

let init =
  let f force =
    let repo =
      C.((try of_filename "logarion.toml" with Sys_error _ -> default ()).repository)
      |> Logarion.repo_path |> Fpath.to_string
    in
    let make_dir d =
      let open Unix in
      try mkdir d 0o700
      with Unix_error (EEXIST, "mkdir", _) -> prerr_endline @@ "Already exists: " ^ d
    in 
    if not force && Array.length (Sys.readdir repo) > 0 then
      prerr_endline "Directory is it not empty. Call with -f to init anyway."
    else
      List.iter make_dir ["title"; "uuid"];
  in
  let force =
    Arg.(value & flag & info ["f"; "force"] ~doc:"Initialise repository even if directory is non empty")
  in
  Term.(const f $ force),
  Term.info
    "init" ~doc:"initialise a logarion repository in present directory"
    ~man:[ `S "DESCRIPTION"; `P "Create a repository in current directory" ]

let create =
  let title =
    Arg.(value & pos 0 string "" & info [] ~docv:"TITLE" ~doc:"(Optional) title for new article")
  in
  let f title =
    let repo = C.((of_filename "logarion.toml").repository) in
    let t = match title with "" -> "Draft" | _ -> title in
    Logarion.Archive.add repo Ymd.({ (blank ()) with meta = { (Meta.blank ()) with Meta.title = t }})
    |> Lwt_main.run;
    ()
  in
  Term.(const f $ title),
  Term.info "create"
    ~doc:"create a new article"
    ~man:[ `S "DESCRIPTION";  `P "Create a new article, with title 'Draft' when none provided"]

let default_cmd =
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "logarion" ~version:"0.1.0" ~doc:"an article collection & publishing system"
            ~man:[ `S "BUGS"; `P "Submit bugs https://github.com/orbifx/logarion/issues."; ]

let cmds = [ init; create ]

let () = match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1 | _ -> exit 0
