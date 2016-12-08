open TomlLenses

let int toml table_name key_name default =
  match get toml (key table_name |-- table |-- key key_name |-- int) with
    Some i -> i | None -> default

let float toml table_name key_name default =
  match get toml (key table_name |-- table |-- key key_name |-- float) with
    Some f -> f | None -> default

let str_opt toml table_name key_name =
  get toml (key table_name |-- table |-- key key_name |-- string)

let str toml table_name key_name default =
  match str_opt toml table_name key_name with
    Some s -> s | None -> default

let strs_opt toml table_name key_name =
  get toml (key table_name |-- table |-- key key_name |-- array |-- strings)

let strs toml table_name key_name default =
  match strs_opt toml table_name key_name with
    Some ss -> ss | None -> default
