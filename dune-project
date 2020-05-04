(lang dune 2.0)

(name tiled)

(license MIT)

(maintainers "Hezekiah M. Carty <hez@0ok.org>")

(authors "Hezekiah M. Carty <hez@0ok.org>")

(source
 (github hcarty/ocaml-tiled))

(generate_opam_files true)

(package
 (name tiled)
 (synopsis "Parser for tmx and tsx files as created by Tiled")
 (depends
  (bos (>= 0.2.0))
  (ezxmlm (>= 1.1.0))
  (fmt (>= 0.8.8))
  (fpath (>= 0.7.2))
  (ocaml (>= 4.08.0))
  (dune (>= 2.0.0))))
