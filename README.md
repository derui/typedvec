# Typedvec #
Provide ability to make strong typed vector and matrix, and algebra functions using it.

# Requirement #
Typedvec needs library to compile and use as follows.
You should use opam to install these.

- core
- ppx_tools
- omake (for build)

# Install #
You must install requirements before install Typedvec.

1. clone this repository
2. `opam install .`

# Use ppx #
You can use typedvec's ppx with ocamlfind.

```sh
$ ocamlfind ocamlc -package typedvec.ppx -c test.ml
```

# Todo #

- Add more functions of Algebra.
