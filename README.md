# compiler-benchmarker

How fast do compilers compile?

This is a small exploration of that. Here, very basic, but identical (except for the language) and very large single-file programs are generated for a variety of languages. The building block is a random integer-bit-twiddling function. For instance:

```C
int f1() {
    int x1 = 307;
    x1 = (x1 | 396);
    int x2 = ((223 ^ x1) ^ ((x1 | ((928 ^ x1) & 781)) & 878));
    x1 = 663;
    int x3 = 63;
    int x4 = 606;
    x3 = (x3 ^ 165);
    int x5 = 906;
    int x6 = 920;
    int x7 = x5;
    return (x7 ^ (x6 | (x4 | (x3 & (x2 | (x1 ^ 911))))));
}
```

This function is nearly identical is almost every language. You can see results for how compilers handle from 5000 to 50000 of these randomized functions [here](https://johnsabr.github.io/compiler-benchmarker/). Compilers tested are gcc, g++, clang, rustc, dmd, gdc, ldc2, ocamlopt, ghc, go, csc, fsharpc, scalac, dotc, javac, and kotlinc. Naturally, benchmarks like these must be taken with a grain of salt. Firstly, I could have done something wrong. Secondly, this is just one set of results from one machine. Thirdly,  use of fancy language features (templates, macros, etc.) can drastically increase compilation time, just as fancy features such as incremental compilation can drastically reduce compilation time. This is only a look at raw cold-start compilation time. Nonetheless, it is interesting to see how compilers handle a 20MB 750K LOC file.

Inspired by this [reddit post](https://www.reddit.com/r/rust/comments/55k577/rust_compilation_times_compared_to_c_d_go_pascal).

## Running and Building

Currently:
- Code files and results are made (and deleted) in `~/testfiles`.
- This is a .NET Core project, and so requires the [dotnet cli](https://docs.microsoft.com/en-us/dotnet/core/tools/?tabs=netcore2x) and [the .NET Core 2.0 SDK](https://www.microsoft.com/net/download/core)
- Building is simply `dotnet restore` followed by `dotnet build`
- Running the project is `dotnet run`
    - Ensure compilers in `Benchmarker.cs` are installed or comment out those you do not want.
    - Modify the starting number of functions, step size and number of steps as needed.
- I have only ran this in Ubuntu. Notably, the Windows/Mac system spec collection code (so you can see what kind of machine the bench ran on) is totally untested.

## Results discussion

An interactive graph of the results can be seen [here](https://johnsabr.github.io/compiler-benchmarker/). Below is a table of results (with seconds to compile) from a run on 2017-10-15, as sorted ascending on the 20000 column. System information is included in the results directory, and the results CSV includes the version of the compiler used. VM runtimes used: OpenJDK 1.8.0_131, Mono 5.2.0.224, and .NET Core 2.1.0 preview1.

| Number Functions | 5000 | 10000 | 15000 | 20000 | 25000 | 30000 | 35000 | 40000 | 45000 | 50000 |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| D (dmd) | 1.2 | 2.0 | 3.1 | 4.2 | 5.5 | 7.2 | 9.0 | 11.1 | 13.7 | 16.9 |
| Java (javac) | 2.4 | 3.4 | 4.1 | 5.2 |  |  |  |  |  |  |
| D (dmd -O) | 1.8 | 3.2 | 4.6 | 6.3 | 8.2 | 10.3 | 12.7 | 15.1 | 18.4 | 22.6 |
| D (ldc2) | 2.6 | 4.8 | 7.3 | 9.7 | 12.6 | 15.8 | 18.9 | 22.5 | 25.8 | 30.6 |
| Go (go build) | 3.0 | 4.7 | 7.1 | 9.7 | 11.9 | 14.1 | 16.4 | 18.7 | 21.4 | 23.7 |
| C++ (clang) | 2.7 | 5.2 | 7.8 | 10.5 | 13.1 | 15.5 | 18.1 | 20.9 | 23.4 | 26.0 |
| D (ldc2 -O) | 3.8 | 6.7 | 9.6 | 12.9 | 16.0 | 19.2 | 23.0 | 26.6 | 31.2 | 35.1 |
| CSharp (csc) | 4.2 | 8.0 | 11.9 | 15.7 | 19.2 | 23.4 | 27.0 | 30.6 | 34.6 | 38.2 |
| CSharp (csc /o) | 4.6 | 7.9 | 12.0 | 16.0 | 19.8 | 23.7 | 27.6 | 31.5 | 34.9 | 38.8 |
| C++ (clang -O2) | 4.0 | 7.9 | 11.8 | 16.0 | 19.8 | 23.7 | 27.9 | 31.9 | 35.9 | 39.7 |
| C (gcc) | 4.7 | 9.2 | 14.0 | 18.7 | 23.4 | 28.0 | 32.7 | 37.7 | 42.9 | 47.0 |
| C++ (g++) | 4.7 | 9.4 | 14.4 | 19.2 | 23.8 | 28.6 | 33.5 | 38.3 | 43.0 | 48.0 |
| D (gdc) | 5.5 | 10.2 | 14.9 | 19.6 | 24.2 | 28.8 | 33.7 | 38.4 | 43.0 | 47.7 |
| C (gcc -O2) | 5.4 | 11.2 | 17.1 | 23.6 | 29.3 | 35.1 | 41.6 | 47.5 | 53.2 | 59.4 |
| C++ (g++ -O2) | 5.9 | 11.9 | 18.0 | 24.7 | 31.1 | 37.1 | 43.8 | 50.3 | 56.7 | 62.4 |
| D (gdc -O) | 11.2 | 20.4 | 29.7 | 39.1 | 48.3 | 58.2 | 67.7 | 76.9 | 86.4 | 95.7 |
| Scala (dotc) | 16.4 | 25.0 | 31.7 | 40.1 |  |  |  |  |  |  |
| Rust (rustc) | 29.2 | 22.5 | 33.5 | 44.9 | 56.0 | 67.1 | 78.3 | 89.5 | 100.9 | 112.6 |
| Rust (rustc -C opt-level=2) | 29.6 | 23.5 | 36.1 | 48.0 | 63.0 | 76.2 | 89.4 | 102.8 | 115.7 | 139.4 |
| Scala (scalac) | 16.1 | 26.5 | 39.5 | 56.6 |  |  |  |  |  |  |
| OCaml (ocamlopt -O2) | 8.3 | 21.8 | 43.4 | 75.9 | 124.5 |  |  |  |  |  |
| Scala (scalac -optimise) | 21.2 | 35.3 | 52.5 | 77.7 |  |  |  |  |  |  |
| OCaml (ocamlopt) | 8.3 | 21.7 | 42.9 | 77.8 | 122.8 |  |  |  |  |  |
| FSharp (fsharpc) | 19.0 | 36.8 | 57.8 | 78.9 | 104.2 | 130.0 | 158.9 | 183.8 | 216.7 | 248.2 |
| FSharp (fsharpc -O) | 19.3 | 37.2 | 57.9 | 80.2 | 104.0 | 127.0 | 159.0 | 187.1 | 222.9 | 252.8 |
| Scala (dotc -optimise) | 28.7 | 50.0 | 68.4 | 81.9 |  |  |  |  |  |  |
| Haskell (ghc -O) | 35.7 | 73.0 | 117.9 | 172.3 | 234.6 | 312.3 | 390.8 | 478.0 | 624.9 | 816.5 |
| Kotlin (kotlinc) | 41.7 | 99.7 | 180.7 | 276.8 |  |  |  |  |  |  |
| Haskell (ghc) | 50.7 | 109.4 | 184.0 | 276.8 | 386.9 | 510.6 | 651.8 | 812.0 | 1052.5 | 1365.6 |

A few notes can be made about these results:

- First, the gaps: JVM languages ran into a 'too many objects' error past 20000 functions, and OCaml ran into a StackOverflow error past 25000 functions.
- Haskell and Kotlin are really slow. This could be an artifact of the code: global Kotlin functions could be slow; I wonder if the timings would change if I put them into companion object (static class) if it would change anything. Haskell's slowness is justified by being Haskell, but I wonder if the timings would change if I put strictness annotations on everything to make it more semantically akin to OCaml and FSharp. Also of interest is the fact that optimized Haskell is faster to compile! My guess right now is that the inclusion of debug information which the optimized version throws away is the source of the slowdown. Both Haskell and Kotlin are so far from the other compile times that I can't help feel the generated code is wrong somehow.
- It is clear that functional languages compile slower, for good reason. Haskell, Scala, FSharp, and OCaml are the slowest to compile at 20000 (not counting Kotlin); however, at the small end (5000 functions) OCaml is by far the quickest of the functional bunch. If you want a fast-compiling functional programming language you can't go wrong with OCaml (of course, as mentioned above, incremental compilation throws a wrench into that judgement). Otherwise, except for OCaml at the start, Scala is the quickest to compile out of all of the functional languages. Another caveat: the FSharp toolchain still depends on Mono; .NET Core may compile FSharp faster.
- D with DMD (and LDC2), Java, and Go are incredibly fast to compile.
- Clang is faster than G++, and similarly LDC2 and DMD are faster than GDC D.
- Simple C++ is fast to compile (with Clang)! But C++ has a reputation of being horrifically slow to compile. How can this be? It only goes to show, in my opinion, just how incredibly, absurdly bad for compilation time a lack of modules can be. C++ needs to stop using `#include` technology from 1972 (that's 45 years ago!).
- Of the natively compiled imperative languages, Rust is the slowest to compile. In fact, it's about the same speed as Scala. It's so high-level that calling it imperative seems off somehow. Seems like it should almost be in its own class of borrow-checker languages. Incremental compilation is a must for these languages. On the other hand, the likes of Go and DMD are so fast to compile that you may as well be running a scripting language.
- CSharp is pretty fast to compile too - about as fast as Clang. But not as quick as Java.



