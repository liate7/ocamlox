(lang dune 3.14)

(using menhir 3.0)

(name ocamlox)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Andrew Patterson")

(maintainers "Andrew Patterson")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ocamlox)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
