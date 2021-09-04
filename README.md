# Compiler Benchmarker

How fast do compilers compile?

This is a small exploration of that. Here, very basic, but identical and very large single-file programs are generated for a variety of languages. The building blocks are random integer  bit-twiddling and arithmetic functions like this one:

```C
int f7(const int p) {
    int x1 = f6(86);
    const int x2 = x1;
    const int x3 = 57;
    x1 = (x1 * f0(x3));
    const int x4 = x3;
    x1 = (x1 ^ (f2(x1) - f4(x4)));
    const int x5 = f2(x4);
    const int x6 = ((p * 74) ^ (f6(x2) & (83 | (f3(x5) * x3))));
    const int x7 = f2(x2);
    return ((((((((57 & p) & x1) | x2) ^ x3) ^ x4) * x5) + x6) * x7);
}
```

This function has a nearly identical version in almost every language.

# Plot

[You can see a plot here](https://ajdust.github.io/compiler-benchmarker/) of how compilers handle between 5k and 20k of such functions.

# Overview

Languages and [compilers tested include](https://github.com/ajdust/compiler-benchmarker/blob/master/docs/202109041755_compilers.csv):

- C/C++
  - gcc
  - clang
  - g++
  - clang++
- D
  - dmd
  - gdc
  - ldc2
- Go (go)
- Rust (rustc)
- Swift (swiftc)
- OCaml (ocamlopt)
- Haskell (ghc)
- Java (javac)
- Scala (scalac)
- Kotlin (kotlinc)
- CSharp and FSharp (dotnet)

Please note silly benchmarks like these shouldn't be taken seriously. (1) I have only ran these benchmarks a few times on my machine, not hundreds. (2) Many compilers have associated build systems that can drastically reduce compilation time via techniques such as incremental compilation and background-compilation. This is measuring cold compiles. (3) Random bit twiddling arithmetic functions are not representative of average code in a language, and use of fancy language features such as templates and macros can drastically increase compilation time.

Nonetheless, it is interesting to see how compilers handle a 10MB file with 200,000 lines of code.

> Inspired by this [reddit post](https://www.reddit.com/r/rust/comments/55k577/rust_compilation_times_compared_to_c_d_go_pascal).

## Running and Building

Currently:
- This is a .NET Core project, and so requires the [the .NET Core SDK](https://dotnet.microsoft.com/download)
- Build by running `dotnet build` in the directory with the `.csproj` file
- Run `dotnet run --project ~/doc/compiler-benchmarker/compiler-benchmarker.csproj` in an external directory
  - Generated code files and results are made in `./testfiles`
  - Info on compilers used is gathered from `./compilers.csv`
  - Function count test cases are set and ran by reading `./empty.csv`

