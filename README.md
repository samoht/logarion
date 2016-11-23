# Logarion

## Summary

_(Note: the following summary is a specification, not everything described is complete)_

Logarion is a personal journaling and publication system.
The suite provides a command line archive interface, a graphical user interfance (coming soon) and a web server.

Design principles:

1. System simplicity & interoperability.
2. Output quality.
3. Distributed interactivity, like sharing with friends.

Logarions articles are stored as plain text Yamado files (`.ymd`).
_YMD_  files can be stored internally and controlled by Logarion, or they can be piped from other sources.

Logarion can be used in two modes:

- Static, published upon a command.

  Suitable for scenarios where installation on the server is not possible.

- Dynamic, using web server. 
  
  Supports interactive features like searching and more advanced Atom feed parameters.

## Install

Make sure you have OCaml >= 4.03.
All requirements are available for automatic installation via [OPAM](https://opam.ocaml.org/)

    opam install batteries lens mustache omd opium ppx_deriving ptime re toml tyxml uuidm
    git clone git@github.com:orbifx/logarion.git
    cd logarion
    make

Also, running `make style` will generate a stylesheet in `share/static/main.css`, using `share/sass/main.sass`.

This should generate `web`. Run with:

    web

and open a browser to <http://localhost:3000>.
To post a new article visit <http://localhost:3000/new>.

## See also

- [CONTRIBUTING.md](CONTRIBUTING.md)
