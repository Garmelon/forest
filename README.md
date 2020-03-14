# forest

Forest is an experiment in tree-based interaction: One or more clients connect
to a server and interact with it (and each other) via an interface consisting of
text-based nodes forming a tree.

The project is split into multiple subprojects, most of which are Haskell
packages. For more information on individual subprojects, see their README or
the summary below.

[API documentation](docs/API.md)

## Subprojects

- [forest-cabin](forest-cabin/): Server (Haskell)
- [forest-common](forest-common/): Common types and functions (Haskell)
- [forest-server](forest-server/): Server framework (Haskell)
- [forest-tui](forest-tui/): Terminal-based client (Haskell)
- [forest-web](forest-web/): Web-based client (static site)
