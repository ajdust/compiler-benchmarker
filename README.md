# compiler-benchmarker
How fast do compilers compile the same thing?

Compiled languages are amazing - except when they are compiling.

This is a look at how various programming languages handle compiling the same program - *n* simple bit manipulation functions. As inspired by this [reddit post](https://www.reddit.com/r/rust/comments/55k577/rust_compilation_times_compared_to_c_d_go_pascal).

Currently:
- Code files and results are made (and deleted) in `~/testfiles`
- This is a .NET Core project, and so requires the dotnet cli and [.NET Core](https://docs.microsoft.com/en-us/dotnet/core/tools/?tabs=netcore2x) to be installed, with the [.NET Core 2.0 SDK](https://www.microsoft.com/net/download/core)
- `dotnet restore` followed by `dotnet build` will build
- `dotnet run` runs the project
- The following compilers are tested: gcc, g++, clang, rustc, dmd, gdc, ldc2, ocamlopt, ghc, go, csc, fsharpc, scalac, dotc, javac, kotlinc. If any are missing, the program will fail. You can comment them out in Main if need be.

Obviously this is not a full-spectrum test. The generated program does not use any fancy features specific to a language; this could be thought of as a C program written in many other languages. It does offer an interesting glimpse into how a compiler responds when the going gets tough - can your compiler handle a 20MB test file with 50000 functions? If so, how long does it take compared to its sibling?

Eventually, I'll find time to run this completely and post a report here.

