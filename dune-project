(lang dune 3.12)

(name aoc_ocaml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Sandro Maglione")

(maintainers "Sandro Maglione")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name aoc_ocaml)
 (synopsis "aoc_ocaml")
 (description "OCaml and Advent of code to learn functional programming")
 (depends ocaml dune)
 (tags
  (functional-programming)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
