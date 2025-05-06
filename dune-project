(lang dune 3.16)

(name dune_deps_extra)

(generate_opam_files true)

(source
 (github ahrefs/dune-deps-extra))

(authors "Raphaël Proust")

(maintainers "Raphaël Proust" "ahrefs")

(license MIT)

(documentation https://github.com/ahrefs/dune-deps-extra)

(package
 (name dune_deps_extra)
 (synopsis "Additional commands for dune-deps")
 (description "dune-deps-extra provides additional ways to query the dependency graph of a dune project")
 (depends ocaml dune (dune-deps (>= 1.5.0)) cmdliner)
 (tags
  (topics dune build)))
