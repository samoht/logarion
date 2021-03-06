# Logarion

Logarion is a [free and open-source](https://joinup.ec.europa.eu/software/page/eupl) personal note taking, journaling and publication system, a blog-wiki hybrid.

[Screenshots of stock themes](https://gitlab.com/orbifx/logarion/wikis/screenshots).

## Features

Plain file system store (each note is a file).
Command line & web interfaces.

Two publishing modes:

- Static, published upon a command.
  Suitable for scenarios where installation on the server is not possible.

- Dynamic, using web server. 
  Supports interactive features like searching and more advanced Atom feed parameters.

## Community & support

- Matrix (chat): #logarion:matrix.org (Via Riot webapp: https://riot.im/app/#/room/#logarion:matrix.org)
- Reddit: https://www.reddit.com/r/logarion/
- Issues: https://gitlab.com/orbifx/logarion/issues

## Install

This software is in alpha development stage, so install it if you know what you are doing.

### Source

Make sure you have OCaml >= 4.03.
All requirements are available for automatic installation via [OPAM](https://opam.ocaml.org/)

	opam pin add logarion https://gitlab.com/orbifx/logarion.git
	opam install logarion

## Configure

The main configuration file is [logarion.toml](logarion.toml).
For configurating the webserver, see _Web server_ section below.

## Running

Once installed you will have `logarion` for command line control of the repository and `logarion-web` for web access.

### Command line

Run `logarion --help`.

### Web server

The web server's configuration is optionally controlled by [web.toml](web.toml) in the directory the server is executed from.

Run `logarion-web`, and open a browser to <http://localhost:3666>.
To post a new article visit <http://localhost:3666/new.note>.

Optionally install a [sass](http://sass-lang.com/) compiler, like [sassc](http://sass-lang.com/libsass#sassc), and then run `sassc share/sass/main.sass > share/static/main.css`, to generate a stylesheet in `share/static/main.css`, using `share/sass/main.sass`.

## See also

- [CONTRIBUTING.md](CONTRIBUTING.md)
- [Licence](https://joinup.ec.europa.eu/software/page/eupl)
