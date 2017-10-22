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

This function is nearly identical is almost every language. You can see results for how compilers handle from 5000 to 50000 of these randomized functions [here](https://johnsonaa.github.io/compiler-benchmarker/). Compilers tested are gcc, g++, clang, rustc, dmd, gdc, ldc2, ocamlopt, ghc, go, csc, fsharpc, scalac, dotc, javac, and kotlinc. Naturally, benchmarks like these must be taken with a grain of salt. Firstly, I could have done something wrong. Secondly, this is just one set of results from one machine. Thirdly,  use of fancy language features (templates, macros, etc.) can drastically increase compilation time, just as fancy features such as incremental compilation can drastically reduce compilation time. This is only a look at raw cold-start compilation time. Nonetheless, it is interesting to see how compilers handle a 20MB 750K LOC file.

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

An interactive graph of the results can be seen [here](https://johnsonaa.github.io/compiler-benchmarker/). Below is a table of results from a run on 2017-10-15, as sorted ascending on the 20000 column. System information is included in the results directory, and the results CSV includes the version of the compiler used. VM runtimes used: OpenJDK 1.8.0_131, Mono 5.2.0.224, and .NET Core 2.1.0 preview1.

| Number Functions             | 5000  | 10000  | 15000  | 20000  | 25000  | 30000  | 35000  | 40000  | 45000  | 50000   |
| ---                          | ---:  | ---:   | ---:   | ---:   | ---:   | ---:   | ---:   | ---:   | ---:   | ---:    |
|  D (dmd)                     | 1.24  | 2.04   | 3.14   | 4.24   | 5.55   | 7.15   | 8.96   | 11.06  | 13.67  | 16.88   |
|  Java (javac)                | 2.44  | 3.44   | 4.14   | 5.25   |        |        |        |        |        |         |
|  D (dmd -O)                  | 1.84  | 3.24   | 4.64   | 6.35   | 8.16   | 10.26  | 12.66  | 15.07  | 18.38  | 22.59   |
|  D (ldc2)                    | 2.64  | 4.84   | 7.25   | 9.65   | 12.56  | 15.77  | 18.87  | 22.49  | 25.79  | 30.61   |
|  Go (go build)               | 2.95  | 4.74   | 7.15   | 9.66   | 11.86  | 14.06  | 16.36  | 18.67  | 21.37  | 23.68   |
|  C++ (clang)                 | 2.75  | 5.24   | 7.85   | 10.45  | 13.06  | 15.46  | 18.07  | 20.87  | 23.38  | 25.990  |
|  D (ldc2 -O)                 | 3.84  | 6.74   | 9.55   | 12.86  | 15.96  | 19.17  | 22.98  | 26.59  | 31.19  | 35.110  |
|  CSharp (csc)                | 4.24  | 8.05   | 11.86  | 15.67  | 19.17  | 23.38  | 26.98  | 30.59  | 34.60  | 38.21   |
|  CSharp (csc /o)             | 4.65  | 7.95   | 11.95  | 15.96  | 19.78  | 23.68  | 27.58  | 31.49  | 34.91  | 38.81   |
|  C++ (clang -O2)             | 4.05  | 7.94   | 11.76  | 15.96  | 19.77  | 23.68  | 27.89  | 31.89  | 35.91  | 39.71   |
|  C (gcc)                     | 4.74  | 9.15   | 13.96  | 18.67  | 23.38  | 27.98  | 32.70  | 37.70  | 42.92  | 47.02   |
|  C++ (g++)                   | 4.75  | 9.35   | 14.36  | 19.17  | 23.78  | 28.59  | 33.50  | 38.30  | 43.02  | 48.03   |
|  D (gdc)                     | 5.55  | 10.25  | 14.86  | 19.57  | 24.18  | 28.79  | 33.69  | 38.40  | 43.02  | 47.72   |
|  C (gcc -O2)                 | 5.44  | 11.15  | 17.07  | 23.58  | 29.29  | 35.10  | 41.61  | 47.52  | 53.24  | 59.44   |
|  C++ (g++ -O2)               | 5.85  | 11.86  | 17.97  | 24.68  | 31.09  | 37.10  | 43.82  | 50.33  | 56.74  | 62.36   |
|  D (gdc -O)                  | 11.15 | 20.37  | 29.69  | 39.11  | 48.33  | 58.25  | 67.67  | 76.89  | 86.39  | 95.72   |
|  Scala (dotc)                | 16.38 | 24.98  | 31.70  | 40.12  |        |        |        |        |        |         |
|  Rust (rustc)                | 29.19 | 22.47  | 33.50  | 44.92  | 56.04  | 67.06  | 78.28  | 89.50  | 100.92 | 112.64  |
|  Rust (rustc -C opt-level=2) | 29.58 | 23.48  | 36.10  | 48.03  | 62.95  | 76.18  | 89.40  | 102.83 | 115.65 | 139.39  |
|  Scala (scalac)              | 16.06 | 26.49  | 39.51  | 56.64  |        |        |        |        |        |         |
|  OCaml (ocamlopt -O2)        | 8.25  | 21.78  | 43.43  | 75.90  | 124.52 |        |        |        |        |         |
|  Scala (scalac -optimise)    | 21.18 | 35.30  | 52.45  | 77.70  |        |        |        |        |        |         |
|  OCaml (ocamlopt)            | 8.25  | 21.68  | 42.93  | 77.81  | 122.81 |        |        |        |        |         |
|  FSharp (fsharpc)            | 18.97 | 36.80  | 57.85  | 78.88  | 104.24 | 130.00 | 158.86 | 183.81 | 216.74 | 248.16  |
|  FSharp (fsharpc -O)         | 19.28 | 37.21  | 57.95  | 80.20  | 104.05 | 126.97 | 158.97 | 187.12 | 222.90 | 252.76  |
|  Scala (dotc -optimise)      | 28.70 | 50.04  | 68.36  | 81.89  |        |        |        |        |        |         |
|  Haskell (ghc -O)            | 35.71 | 72.98  | 117.88 | 172.29 | 234.62 | 312.28 | 390.76 | 477.98 | 624.85 | 816.52  |
|  Kotlin (kotlinc)            | 41.74 | 99.74  | 180.70 | 276.78 |        |        |        |        |        |         |
|  Haskell (ghc)               | 50.74 | 109.36 | 184.02 | 276.81 | 386.95 | 510.61 | 651.76 | 811.97 | 1052.50| 1365.58 |

A few notes can be made about these results:

- First, the gaps: JVM languages ran into a 'too many objects' error past 20000 functions, and OCaml ran into a StackOverflow error past 25000 functions.
- Haskell and Kotlin are really slow. This could be an artifact of the code: global Kotlin functions could be slow; I wonder if the timings would change if I put them into companion object (static class) if it would change anything. Haskell's slowness is justified by being Haskell, but I wonder if the timings would change if I put strictness annotations on everything to make it more semantically akin to OCaml and FSharp. Also of interest is the fact that optimized Haskell is faster to compile! My guess right now is that the inclusion of debug information which the optimized version throws away is the source of the slowdown. Both Haskell and Kotlin are so far from the other compile times that I can't help feel the generated code is wrong somehow.
- It is clear that functional languages compile slower, for good reason. Haskell, Scala, FSharp, and OCaml are the slowest to compile at 20000 (not counting Kotlin); however, at the small end (5000 functions) OCaml is by far the quickest of the functional bunch. If you want a fast-compiling functional programming language you can't go wrong with OCaml (of course, as mentioned above, incremental compilation throws a wrench into that judgement). Otherwise, except for OCaml at the start, Scala is the quickest to compile out of all of the functional languages. Another caveat: the FSharp toolchain still depends on Mono; .NET Core may compile FSharp faster.
- D with DMD (and LDC2), Java, and Go are incredibly fast to compile.
- Clang is faster than G++, and similarly LDC2 and DMD are faster than GDC D.
- Simple C++ is fast to compile (with Clang)! But C++ has a reputation of being horrifically slow to compile. How can this be? It only goes to show, in my opinion, just how incredibly, absurdly bad for compilation time a lack of modules can be. C++ needs to stop using `#include` technology from 1972 (that's 45 years ago!).
- Of the natively compiled imperative languages, Rust is the slowest to compile. In fact, it's about the same speed as Scala. It's so high-level that calling it imperative seems off somehow. Seems like it should almost be in its own class of borrow-checker languages. Incremental compilation is a must for these languages. On the other hand, the likes of Go and DMD are so fast to compile that you may as well be running a scripting language.
- CSharp is pretty fast to compile too - about as fast as Clang. But not as quick as Java.



