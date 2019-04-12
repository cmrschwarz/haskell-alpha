# haskell-alpha

WIP equation solver, which outputs not only the solution but also the steps.

## Example

```sh
./main '3 * (x + 1) = 42'
(3 * (x + 1)) = 42
(1 + x) = 14
x = 13
```

## Building

```sh
cd src
ghc -O2 main.hs -outputdir bin
```