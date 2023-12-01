# Filename Normalizer

Don't you hate it when someone has so poor a taste as to name files and directories using any characters which aren't alphanumeric, dots, dashes or underscores? Well imagine my shock when I found out such unseemly forms are commonplace, let alone standard in certain (albeit niche) operating systems!

But fear not, for the filename normalizer is here to fix your filenames!

## Installation
`fnorm` is only available from source, though that's not much of an issue: It's literally a single Haskell file that depends on nothing but the language's standard library.

First, clone the repo:
```sh
git clone https://github.com/RayOfSunDull/fnorm
cd fnorm
```
If you're on Linux, you can use the makefile as such:
```sh
make
make install
```
Of course, you need the [Glasgow Haskell Compiler](https://www.haskell.org/) for this.

By default, it'll  install `fnorm` into `${HOME}/bin`, which isn't  always in the `PATH`. In that case, edit the `INSTALLDIR` variable in the makefile to your liking.

If you're on Windows, you can compile it manually:
```sh
ghc --make src/main.hs -o bin/fnorm.exe
```
Then, I believe you're supposed to add the folder of the executable (should be `wherever_you_cloned_the_repo_to\fnorm\bin`) to your path.

## Usage
It's very straightforward. Say you have a file (or directory) with the name `filename`. Fixing it is as simple as:
```sh
fnorm filename
```

In the future, I plan to add support for glob patterns and recursive filename fixing. Perhaps custom install scripts for each OS too.

