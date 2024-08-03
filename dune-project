(lang dune 3.14)

(using menhir 3.0)

(name ocamlox)

(generate_opam_files true)

(source
 (github liate7/ocamlox))

(authors "Andrew Patterson")

(maintainers "Andrew Patterson")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name ocamlox)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
