(lang dune 3.17)

(name ocamnes)

(generate_opam_files true)

(maintainers "duosrx@gmail.com")

(package
 (name ocamnes)
 (synopsis "NES Emulator")
 (description "An NES emulator written in OCaml")
 (authors "Xavier Perez <duosrx@gmail.com>")
 (homepage "https://github.com/DuoSRX/ocamnes")
 (bug_reports "https://github.com/DuoSRX/ocamnes/issues")
 (license "MIT")
 (allow_empty)
 (depends
  (core
   (>= 0.17.0))
  ppx_deriving
  tsdl
  stdint
  imagelib
  ounit2))
