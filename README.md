# Parses Dedukti files to format easier for relation graph in ML


1. Clone dedukti to some folder (prefix it with `.` to disable dune from building it)
2. run `make && ./deduktiParser.exe .deduktiGithub/libraries > output.out`

The program fill first find all the `.dk` files in the specified directory and
then parse all of them.

## Troubleshooting

When running `make`, you may face the error ``

```bash
dune build @fmt --auto-promote
make: dune: No such file or directory
make: *** [Makefile:7: format] Error 127
```

Make sure `dune` is installed and/or run 
`eval $(opam env)` first.
