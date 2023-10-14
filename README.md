# Parses Dedukti files to format easier for relation graph in ML


1. Clone dedukti to some folder (prefix it with `.` to disable dune from building it)
2. run `make && ./deduktiParser.exe .deduktiGithub/libraries/**/*.dk > output.out`

## Troubleshooting

When running `make`, you may face the error ``

```bash
dune build @fmt --auto-promote
make: dune: No such file or directory
make: *** [Makefile:7: format] Error 127
```

Make sure `dune` is installed and/or run 
`eval $(opam env)` first.

When running

```./deduktiParser.exe .deduktiGithub/libraries/**/*.dk > output.out```,

some libraries (those that are nested deeper) might not be processed. For those, you should use, for example, the pattern

`.deduktiGithub/libraries/**/**/*.dk`

instead.