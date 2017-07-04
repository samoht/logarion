# Contributing to Logarion

Logarions primary aim is to create a note system, which doesn't waste resources.
The secondary aim is to provide an exemplary OCaml project to demonstrate and promote the language (as it happens with many other "Blogging" systems written in other languages).

As part of the secondary aim, the source code needs to written in a way that encourages the language's adoption and the participation to the OCaml developer community.

## Starting with OCaml

_"OCaml is an industrial strength programming language supporting functional, imperative and object-oriented styles"_ -- https://ocaml.org/

OCaml simply rocks.

If you are unfamiliar with OCaml, consider starting with these resources:

- Install OCaml: https://ocaml.org/docs/install.html
- Read about OCaml: https://ocaml.org/learn/books.html
- Ask questions & join the community:
  - Mailing lists: https://ocaml.org/community/
  - IRC: irc://irc.freenode.net/#ocaml (Web client: https://riot.im/app/#/room/#freenode_#ocaml:matrix.org )
  - Reddit: http://www.reddit.com/r/ocaml/
  - Discourse: https://discuss.ocaml.org/
  - .. other: https://ocaml.org/community/

## Design principles

[Unix phisophy](https://en.wikipedia.org/wiki/Unix_philosophy#Do_One_Thing_and_Do_It_Well)

1. System simplicity & interoperability.
2. Output quality.
3. Distributed interactivity, like sharing with friends.

## Source

### Layers

There are three layers:

- notes 
- archive 
- interfaces & intermediate formats

### Core

- `logarion.ml`: repository related functions (listing, adding/removing, etc). ([src/logarion.ml](https://gitlab.com/orbifx/logarion/blob/master/src/logarion.ml))
- `note.ml`: parsing from and to note files. ([src/note.ml](https://gitlab.com/orbifx/logarion/blob/master/src/note.ml))

### Intermediate formats

Converters:

- `html.ml`: archive to HTML pages.
- `atom.ml`: archive to Atom feeds.

### Interfaces

Programs:

- `web.ml`: accessing logarion over HTTP.
- `command.ml`: interacting with logarion archive with commands, for both interactive and scripted use.
