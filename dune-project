(lang dune 3.10)
(name bftest)
(generate_opam_files true)
(license MIT)
(authors "Kumar Appaiah <a.kumar@alumni.iitm.ac.in>")
(source
 (github kumanna/bf-ocaml))
(using menhir 2.1)

(package
 (name bftest)
 (synopsis "Brainfuck interpreter written in OCaml")
 (description "A Brainfuck interpreter written in OCaml. Can use direct parsing as well as Menhir.")
 (depends
  (alcotest :with-test)
  (ocaml
   (>= 4.02.3))
  dune
 (menhir :with-test)
 (ocaml
  (>= 4.02.3))))
