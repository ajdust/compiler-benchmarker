/* Run the compilation benchmark */

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;

namespace CompilerBenchmarker
{
	class CompilationFailed : Exception
	{
		public CompilationFailed(string message) : base(message) {}
	}

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

	struct CompilerBenchmark
	{
		public Compiler Compiler;
		public TimeSpan TimeToCompile;
		public int NumberFunctions;

		public CompilerBenchmark(Compiler compiler, TimeSpan timeToCompile, int numberFunctions)
		{
			if (timeToCompile == TimeSpan.Zero)
				throw new ArgumentException("Compiling cannot take zero seconds", nameof(timeToCompile));
			if (numberFunctions < 0)
				throw new ArgumentException("Cannot compile zero functions", nameof(numberFunctions));

			Compiler = compiler;
			TimeToCompile = timeToCompile;
			NumberFunctions = numberFunctions;
		}
	}

	static class Benchmarker
	{
		static TimeSpan? RunBenchmark(Compiler compiler, string codeFilePath)
		{
			var watch = new Stopwatch();
			watch.Start();
			var args = string.Join(" ", new[] { codeFilePath }.Concat(compiler.Arguments));
			Console.WriteLine($"- Running: {compiler.Exe} {args}");
			var p = Process.Start(compiler.Exe, args);
			p.WaitForExit(); // todo: pass in compiler timeout option
			if (p.ExitCode != 0)
			{
				// todo: pass in stop on failure option
				watch.Stop();
				Console.WriteLine($"! Compilation failed!");
				throw new CompilationFailed($"Compilation failed for '{compiler.Exe} {args}'");
				// return null;
			}

			watch.Stop();
			Console.WriteLine($"- Took {watch.Elapsed}");
			Console.WriteLine();
			return watch.Elapsed;
		}

		static IEnumerable<CompilerBenchmark> RunBenchmarks(
			List<Compiler> compilers, int numberAtStart, int numberOfSteps, int increaseOnStep)
		{
			foreach (var langCompilers in compilers.GroupBy(x => x.Language))
			{

				for (int numFun = numberAtStart, step = 1;
					step <= numberOfSteps;
					step += 1, numFun += increaseOnStep)
				{
					// generate file
					Console.WriteLine($"Generating code for {langCompilers.Key} with {numFun} functions..");
					var codeFilePath = new CodeGen().WriteLang(langCompilers.Key, numberAtStart, numFun);
					foreach (var compiler in langCompilers)
					{
						// run benchmark
						Console.WriteLine($"- Running benchmark for {langCompilers.Key} with {numFun} functions..");
						var time = RunBenchmark(compiler, codeFilePath);
						if (time.HasValue)
							yield return new CompilerBenchmark(compiler, time.Value, numFun);
					}

					// todo: pass in file cleanup options
				}
			}
		}

		static void WriteResults(IEnumerable<CompilerBenchmark> times, string resultFileName)
		{
			Console.WriteLine("Writing results..");
			var header = "Compiler,Options,NumberFunctions,Time";
			var rows = times.Select(x => string.Join(",", new[] {
				x.Compiler.Exe,
				string.Join(" ", x.Compiler.Arguments),
				x.NumberFunctions.ToString(),
				x.TimeToCompile.TotalSeconds.ToString()
			}));

			File.WriteAllText(resultFileName, header + "\n" + string.Join("\n", rows));
			Console.WriteLine($"Wrote results to {resultFileName}");
		}

		static void Main(string[] args)
		{
			int numberAtStart = 1;
			int numberOfSteps = 1;
			int stepIncreaseNumber = 10;

			// todo: good command-line options library for C#?
			var helpInfo = new Dictionary<string, int>
			{
				{ "numberAtStart", numberAtStart },
				{ "numberOfSteps", numberOfSteps },
				{ "increaseOnStep", stepIncreaseNumber }
				// todo:
				// cleanup on/off (on by default)
				// result file name, + show results to stdout to be on by default
				// csv file name (timestamped default)
				// put results in timestamped CSV file (on by default)
			};

			var compilers = new List<Compiler>
			{
				// native section
				// new Compiler("C", "c", "gcc", "-O2"),
				// new Compiler("C++", "cpp", "g++", "-O2"),
				// new Compiler("C++", "cpp", "clang", "-O2"),
				// new Compiler("Go", "go", "go", "build"),
				// new Compiler("Rust", "rs", "rustc", "opt-level=2"),
				new Compiler("D", "d", "dmd", "-O"),
				new Compiler("D", "d", "gdc", "-O"),
				new Compiler("D", "d", "ldc2", "-O"),
				new Compiler("Haskell", "hs", "ghc"), // optimize?
				new Compiler("OCaml", "ml", "ocamlopt"), // optimize?

				// VM section
				new Compiler("CSharp", "cs", "csc"),
				new Compiler("FSharp", "fs", "fsharpc"),
				// new Compiler("java", "java", "javac"), // todo: generate java
				// new Compiler("scala", "scala", "scalac") // todo: generate scala
				new Compiler("Kotlin", "kt", "kotlinc"),
			};

			try
			{
				var write_to = "testfiles";
				if (!Directory.Exists(write_to))
                	Directory.CreateDirectory(write_to);
            	Directory.SetCurrentDirectory(write_to);
				var benchmarks = RunBenchmarks(compilers, numberAtStart, numberOfSteps, stepIncreaseNumber);

				// todo: write results to CSV, with system data (OS, Kernel, CPU, Mem, HD?)
				// should we track memory consumption as well?
				WriteResults(benchmarks, $"results_{DateTime.Now.ToString("yyyyMMddTHHmmss")}.csv");
			}
			catch (CompilationFailed e)
			{
				Console.WriteLine(e.Message);
			}
		}
	}
}