# compiler-benchmarker

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

This function has a nearly identical version in almost every language. You can see results for how compilers handle from 5000 to 25000 of these randomized functions [here](https://ajdust.github.io/compiler-benchmarker/). Languages and compilers tested (all on Ubuntu 18 with kernel 4.15.0-39-generic):

- C/C++
  - gcc
  - clang
  - g++
  - clang++
- D
  - dmd
  - ldc2
- Go (go)
- Rust (rustc)
- Swift (swiftc)
- OCaml (ocamlopt)
- Haskell (ghc)
- Java (javac)
- Scala
  - scalac
  - dotc
- Kotlin (kotlinc)
- CSharp and FSharp (dotnet)

Please note silly benchmarks like these shouldn't be taken seriously. (1) I'm not an expert in everything, I most likely missed some obvious optimization or compiler flag. (2) I have only ran these benchmarks a few times on my machine, not hundreds. (3) Many compilers have associated build systems that can drastically reduce compilation time via techniques such as incremental compilation and background-compilation. This is measuring cold compiles. (4) Random bit twiddling arithmetic functions are not representative of average code in a language; compilers aren't made with this in mind. (5) Use of fancy language features (for instance templates, macros, etc.) can drastically increase compilation time.

Nonetheless, it is interesting to see how compilers handle a 10MB file with 250,000 lines of code.

> Inspired by this [reddit post](https://www.reddit.com/r/rust/comments/55k577/rust_compilation_times_compared_to_c_d_go_pascal).

## Running and Building

Currently:
- Code files and results are made (and deleted) in `~/testfiles`.
- This is a .NET Core project, and so requires the [the .NET Core SDK](https://dotnet.microsoft.com/download)
- Building is simply `dotnet build` inside the code directory
- Rebuilding/running the project is `dotnet run`
    - Ensure compilers listed in `Benchmarker.cs` are installed and available on the path, or comment out those you do not want.
    - Modify the starting number of functions, step size and number of steps as needed.
- I have only ran this in Ubuntu. Notably, the Windows/Mac system spec collection code in `BasicSystemInfo.cs` is totally untested.

## Results discussion

An interactive graph of the results can be seen [here](https://ajdust.github.io/compiler-benchmarker/). Below is a table of results (with seconds to compile) from a run on 2018-12-02, as sorted ascending on the 20000 column. System information is included in the results directory, and the results CSV includes the version of the compiler used. VM runtimes installed: `openjdk 10.0.2 2018-07-17` and `Microsoft.NETCore.App 2.1.5`.

| Functions | 5000 | 10000 | 15000 | 20000 | 25000 |
| ---       | ---: | ---:  | ---:  | ---:  | ---: |
| D (dmd)               | 0.6  | 1.0  | 1.4  | 1.8  | 2.4 |
| D (dmd -O)            | 0.9  | 1.6  | 2.3  | 3.0  | 3.8 |
| Java (javac)          | 1.6  | 2.1  | 2.6  | 3.0  |     |
| Go (go build)         | 1.0  | 1.8  | 2.6  | 3.4  | 4.2 |
| C (clang)             | 1.2  | 2.3  | 3.5  | 4.7  | 5.9 |
| D (ldc2)              | 1.3  | 2.5  | 3.6  | 4.9  | 6.1 |
| C++ (clang++)         | 1.5  | 2.8  | 4.1  | 5.5  | 6.7 |
| CSharp (dotnet -c ..) | 2.4  | 4.1  | 5.6  | 7.0  | 9.3 |
| CSharp (dotnet)       | 3.4  | 4.2  | 6.0  | 7.8  | 9.3 |
| C (gcc)               | 2.9  | 6.0  | 9.1  | 11.9 | 14.9  |
| C++ (g++)             | 3.4  | 6.7  | 10.0 | 13.4 | 16.9  |
| Scala (dotc)          | 9.0  | 14.3 | 19.8 | 21.6 |       |
| Rust (rustc)          | 6.9  | 14.2 | 21.8 | 29.5 |       |
| C (gcc -O2)           | 8.9  | 17.6 | 26.7 | 35.7 | 44.9  |
| C++ (g++ -O2)         | 9.3  | 18.8 | 28.2 | 37.9 | 47.2  |
| C (clang -O2)         | 10.9 | 21.8 | 31.7 | 42.4 | 53.1  |
| C++ (clang++ -O2)     | 11.0 | 21.7 | 32.3 | 43.1 | 53.6  |
| D (ldc2 -O)           | 11.4 | 22.2 | 34.0 | 44.7 | 56.5  |
| OCaml (ocamlopt -O2)  | 4.7  | 12.9 | 26.6 | 49.4 |       |
| FSharp (dotnet)       | 14.1 | 26.4 | 38.8 | 50.5 | 64.2  |
| OCaml (ocamlopt)      | 5.2  | 13.5 | 26.3 | 50.6 |       |
| FSharp (dotnet -c ..) | 16.7 | 31.3 | 45.9 | 62.3 | 77.4  |
| Haskell (ghc)       | 12.7 | 27.4 | 54.2 | 75.6 | 117.0 |
| Haskell (ghc -O2)   | 12.6 | 27.0 | 57.3 | 75.8 | 122.4 |
| Swift (swiftc)        | 11.6 | 31.4 | 54.7 | 77.3 | 106.6 |
| Kotlin (kotlinc)      | 34.9 | 68.6 | 101.5 | 135.9 |     |
| Swift (swiftc -O)     | 13.8 | 40.0 | 72.8  |       |     |
| Scala (scalac)        | 8.9  | 12.4 |       |       |     |
| Scala (scalac -opt..) | 11.0 | 18.0 |       |       |     |

Memory usage from `/usr/bin/time -v` for compiling 20000 functions. Not the best indicator of memory usage, note.

| Compiler  | Memory (MB) |
| ---       | ---: |
| CSharp (dotnet -c ..) | 99 |
| CSharp (dotnet)       | 100 |
| C (clang)             | 298 |
| C++ (clang++)         | 320 |
| D (dmd -O)            | 386 |
| D (dmd)               | 390 |
| C (clang -O2)         | 420 |
| C++ (clang++ -O2)     | 443 |
| Java (javac)          | 518 |
| C (gcc)               | 528 |
| Go (go build)         | 659 |
| C++ (g++)             | 715 |
| D (ldc2)              | 728 |
| C (gcc -O2)           | 732 |
| C++ (g++ -O2)         | 793 |
| Swift (swiftc)        | 831 |
| D (ldc2 -O)           | 832 |
| FSharp (dotnet)       | 1123 |
| FSharp (dotnet -c ..) | 1279 |
| Scala (dotc)          | 1507 |
| OCaml (ocamlopt -O2)  | 1538 |
| OCaml (ocamlopt)      | 1539 |
| Rust (rustc)          | 2171 |
| Kotlin (kotlinc)      | 4348 |
| Haskell (ghc -O2)   | 4433 |
| Haskell (ghc)       | 4809 |

A few notes/opinions might be made from these results. First, some compilers error out with too many functions. Java, Scala, Kotlin, Rust, OCaml, and optimised Swift error in one way or another. Compiling optimised Rust does not work with these random functions generated for some reason.

These results aren't too surpising - though I would expect Kotlin to cold compile faster. The imperative languages - D, Java, Go, C/C++, C# - compile very fast and use relatively little memory doing so. More functional languages with more complicated type systems - Swift, Rust, FSharp, Haskell, OCaml - compile slower and use relatively more memory. D is the fastest to compile by far among imperative languages, OCaml is the leanest by a little bit for functional languages. Most of the time, optimization flags definitely increase the build time, though CSharp and OCaml don't appear to have much to optimise here. Haskell and Kotlin both take particularly long time to compile and use much memory doing so.

If this contest had winners, it would be:

- Gold Metal: `dmd` representing D
- Silver Metal: `clang` representing C/C++
- Bronze Metal: `go` representing Go
- Runner ups: `javac`, `dotnet`
