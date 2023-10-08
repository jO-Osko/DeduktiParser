(lang dune 2.5)

(name deduktiParser)

(version 0.0.1)

(source
 (github jO-Osko/deduktiParser))

(license MIT)

(authors "Filip Koprivec")

(package
 (name deduktiParser)
 (depends
  dedukti
  utop
  (ocaml
   (>= 4.14.1))
  (ocamlformat
   (= 0.25.1))))
