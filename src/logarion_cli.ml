open Cmdliner
open Logarion
module C = Archive.Configuration

let conf () =
  try C.of_toml_file (Lpath.from_config_paths "logarion.toml")
  with Not_found -> prerr_endline ("No logarion.toml; using default values"); C.default ()

let init =
  let f force =
    let repo = Lpath.string_of_repo @@ (conf ()).C.repository in
    prerr_endline repo;
    let make_dir d =
      let open Unix in
      try mkdir d 0o700
      with Unix_error (EEXIST, "mkdir", _) -> prerr_endline @@ "Already exists: " ^ d
    in 
    if not force && Array.length (Sys.readdir repo) > 0 then
      prerr_endline @@ "Directory " ^ repo ^ " is it not empty. Call with -f to init anyway."
    else
      List.iter make_dir [Fpath.to_string Lpath.notes];
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
    Arg.(value & pos 0 string "" & info [] ~docv:"TITLE" ~doc:"Title for new article")
  in
  let f title =
    let repo = (conf ()).C.repository in
    let t = match title with "" -> "Draft" | _ -> title in
    let note = Note.({ (blank ()) with meta = { (Meta.blank ()) with Meta.title = t }}) in
    File.Lwt.with_note (File.store repo) note
    |> Lwt_main.run
    |> ignore
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
