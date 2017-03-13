# Contributing to Logarion

There are three layers: the file format, the repository system, and the outlets.

    ymd.ml <--> logarion.ml <--> intermediate formats <--> programs

Design principles:

1. System simplicity & interoperability.
2. Output quality.
3. Distributed interactivity, like sharing with friends.

## Source

### Core

- `logarion.ml`: repository related functions (listing, adding/removing, etc).
- `ymd.ml`: parsing from and to YMD files.

### Intermediate formats

Converters:

- `html.ml`: archive to HTML pages.
- `atom.ml`: archive to Atom feeds.

### Interfaces

Programs:

- `web.ml`: accessing logarion over HTTP.
- `command.ml`: interacting with logarion archive with commands, for both interactive and scripted use.
