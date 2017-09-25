/* Run the compilation benchmark */

using System;
using System.Linq;
using System.Collections.Generic;

namespace CompilerTimeBenchmark
{
	struct Compiler
	{
		// The name of the language, e.g. "C"
		public string Language { get; set; }
		// The extension of the files in this language, e.g. "c"
		public string Extension { get; set; }
		// The compiler executable, e.g. "gcc"
		public string Exe { get; set; }
		// The arguments to the compiler executabe, e.g. "-O"
		public string[] Arguments { get; set; }

		public Compiler(string language, string extension, string exe, params string[] arguments)
		{
			if (string.IsNullOrWhiteSpace(language))
				throw new ArgumentNullException(nameof(language));
			if (string.IsNullOrWhiteSpace(extension))
				throw new ArgumentNullException(nameof(extension));
			if (string.IsNullOrWhiteSpace(exe))
				throw new ArgumentNullException(nameof(exe));

			Language = language;
			Extension = extension;
			Exe = exe;
			Arguments = arguments ?? new string[] {};
		}
	}

	struct CompilerTime
	{
		Compiler Compiler;
		TimeSpan TimeToCompile;
		int NumberFunctions;

		public CompilerTime(Compiler compiler, TimeSpan timeToCompile, int numberFunctions)
		{
			if (timeToCompile == TimeSpan.Zero)
				throw new ArgumentException(nameof(timeToCompile));
			if (numberFunctions < 0)
				throw new ArgumentException(nameof(numberFunctions));
			Compiler = compiler;
			TimeToCompile = timeToCompile;
			NumberFunctions = numberFunctions;
		}
	}

	static class Program
	{
		static CompilerTime RunBenchmark(Compiler compiler, int numberFunctions)
		{
			// todo:
			// call out to subprocess with "time" or otherwise time it somehow
			var timeToCompile = new TimeSpan(1);
			return new CompilerTime(compiler, timeToCompile, numberFunctions);
		}

		static IEnumerable<CompilerTime> RunBenchmarks(
			List<Compiler> compilers, int numberAtStart, int numberOfSteps, int increaseOnStep)
		{
			foreach (var langCompilers in compilers.GroupBy(x => x.Language))
			{

				for (int numFun = numberAtStart, step = 1;
					step < numberOfSteps;
					step += 1, numFun += increaseOnStep)
				{
					// generate file
					var codegenFilePath = new CodeGen().WriteLang(langCompilers.Key, numberAtStart, numFun);
					foreach (var compiler in langCompilers)
					{
						yield return RunBenchmark(compiler, numFun);
					}

					// todo: enact file cleanup options
				}
			}
		}

		static void Main(string[] args)
		{
			int numberAtStart = 10;
			int numberOfSteps = 1;
			int increaseOnStep = 10;

			// todo: good command-line options library for C#?
			var helpInfo = new Dictionary<string, int>
			{
				{ "numberAtStart", numberAtStart },
				{ "numberOfSteps", numberOfSteps },
				{ "increaseOnStep", increaseOnStep }
				// todo:
				// cleanup on/off (on by default)
				// show results to stdout (on by default)
				// csv file name (timestamped default)
				// put results in timestamped CSV file (on by default)
			};

			var compilers = new List<Compiler>
			{
				// native section
				// c gcc
				// c++ gcc
				new Compiler("c", "c", "gcc", "-O2"),
				new Compiler("c++", "cpp", "clang", "-O2"),
				new Compiler("go", "go", "go", "build"),
				new Compiler("rust", "rs", "rustc", "opt-level=2"),
				new Compiler("d", "d", "dmd", "-O"),
				new Compiler("d", "d", "gdc", "-O"),
				new Compiler("d", "d", "ldc", "-O"),
				new Compiler("haskell", "hs", "ghc"), // optimize?
				new Compiler("ocaml", "ml", "ocamlopt"), // optimize?

				// VM section
				new Compiler("csharp", "cs", "csc"),
				new Compiler("fsharp", "fs", "fsharpc"),
				new Compiler("java", "java", "javac"),
				new Compiler("kotlin", "kt", "kotlinc"),
				new Compiler("scala", "scala", "scalac")
			};

			var results = RunBenchmarks(compilers, numberAtStart, numberOfSteps, increaseOnStep);

			// todo: write results to CSV, with system data (OS, Kernel, CPU, Mem, HD?)
			// should we track memory consumption as well?
		}
	}
}