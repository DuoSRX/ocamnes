(lang dune 3.17)

(name ocamnes)

(generate_opam_files true)

(maintainers "duosrx@gmail.com")

(package
 (name ocamnes)
 (synopsis "NES Emulator")
 (description "")
 (allow_empty)
 (depends
  (core
   (>= 0.16.0))
  core_compat
  ppx_deriving
  tsdl
  stdint
  camlimages
  ounit))
