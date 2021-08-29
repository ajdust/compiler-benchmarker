using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text.Json;
using System.Text.RegularExpressions;
using System.Threading;
using CsvHelper;

namespace CompilerBenchmarker
{
    static class Benchmarker
    {
        static TimingResult? CmdTimeBenchmark(CompilerInfo compiler, TestCase testCase, string args)
        {
            var sout = new List<string>();
            using (var p = new Process())
            {
                // RESULT: %x %e %M means (exit code, elapsed time in seconds, max resident set size)
                p.StartInfo.FileName = "/usr/bin/time";
                p.StartInfo.Arguments = $@"-f ""RESULT: %x %e %M"" {compiler.Compiler} {args}";

                p.StartInfo.UseShellExecute = false;
                p.StartInfo.RedirectStandardOutput = true;
                p.StartInfo.RedirectStandardError = true;
                p.StartInfo.ErrorDialog = false;

                Console.WriteLine($"\"{p.StartInfo.FileName} {p.StartInfo.Arguments}\"");
                foreach (var k in testCase.EnvironmentVariables)
                {
                    Console.Write($" and {k.Key}=\"{k.Value}\"");
                    p.StartInfo.EnvironmentVariables[k.Key] = k.Value;
                }

                // The last line of the output will be from /usr/bin/time
                p.OutputDataReceived += (sender, outputLine) =>
                {
                    if (outputLine.Data != null)
                        sout.Add(outputLine.Data);
                };
                p.ErrorDataReceived += (sender, errorLine) =>
                {
                    if (errorLine.Data != null)
                        sout.Add(errorLine.Data);
                };

                p.Start();
                p.BeginOutputReadLine();
                p.BeginErrorReadLine();
                p.WaitForExit();

                if (p.ExitCode != 0)
                {
                    Thread.Sleep(2500);
                    return null;
                }
            }

            for (var i = sout.Count - 1; i >= 0; --i)
            {
                var line = sout[i];
                if (!line.StartsWith("RESULT:"))
                    continue;

                var results = line.Split(' ');
                var exitCode = results[1];
                if (int.Parse(exitCode) != 0)
                {
                    Console.WriteLine($"  ! Compilation failed for '{compiler.Compiler} {args}'");
                    Thread.Sleep(2500);
                    return null;
                }

                var elapsedSeconds = double.Parse(results[2]);
                var maxResidentSetSize = int.Parse(results[3]);
                var timing = new TimingResult(elapsedSeconds, maxResidentSetSize);
                Console.WriteLine($"  - Took {timing.Elapsed} MRSS {maxResidentSetSize}");
                return timing;
            }

            throw new Exception("Result of /usr/bin/time not found in output of {" + string.Join("\n", sout) + "}");
        }

        static TimingResult? RunBenchmark(CompilerInfo compiler, TestCase testCase, string codeFilePath, int numFun)
        {
            // dotnet cli requires project file
            var isDotnet = compiler.Compiler == "dotnet";
            if (isDotnet)
            {
                using var p = Process.Start(compiler.Compiler, $"restore CB.{compiler.Extension}proj");
                p.WaitForExit();
                if (p.ExitCode != 0)
                {
                    Console.WriteLine($"  ! Compilation failed for '{compiler.Compiler}'");
                    return null;
                }
            }

            var args = isDotnet
                ? $"{testCase.Arguments} CB.{compiler.Extension}proj"
                : $"{testCase.Arguments} {codeFilePath}";

            Console.Write($"  - Running with {numFun}: ");
            return CmdTimeBenchmark(compiler, testCase, args);
        }

        static IEnumerable<TestCase> RunBenchmarks(
            List<CompilerInfo> compilers, List<TestCase> testCases)
        {
            // Which files to not delete after compiling
            var doNotDelete = compilers
                .Select(x => $@"\d\.{x.Extension}$")
                .Distinct()
                .Join("|") + @"|\.csv$|\.txt$";
            var codeGen = new CodeGen.CodeGenerator();

            var failed = new HashSet<CompilerInfo>();
            var requireDotnetProjFile = testCases.Any(x => x.Compiler == "dotnet");
            var byCompiler = compilers.ToDictionary(x => x.Compiler);

            foreach (var testCase in testCases)
            {
                Console.WriteLine($"Benchmarking {testCase}:");

                var compiler = byCompiler[testCase.Compiler];
                var codeFilePath = $"test_{testCase.NumberFunctions}.{compiler.Extension}";

                // Sub test directory
                var testdir = testCase.NumberFunctions.ToString();
                Console.WriteLine($"Stepping into {new DirectoryInfo(testdir).FullName}");
                if (!Directory.Exists(testdir))
                    Directory.CreateDirectory(testdir);

                Directory.SetCurrentDirectory(testdir);
                try
                {
                    Console.Write(
                        $"- Generating {testCase.Language} with {testCase.NumberFunctions} functions to {codeFilePath}");
                    if (!File.Exists(codeFilePath))
                        codeGen.OverwriteLang(testCase.Language, testCase.NumberFunctions, codeFilePath);
                    Console.WriteLine();

                    // dotnet compiler requires project file
                    if (requireDotnetProjFile)
                    {
                        var (csp, fsp) = ("CB.csproj", "CB.fsproj");
                        if (File.Exists(csp)) File.Delete(csp);
                        if (File.Exists(fsp)) File.Delete(fsp);
                        File.WriteAllText(csp, GetCsProj());
                        File.WriteAllText(fsp, GetFsProj(codeFilePath));
                    }

                    if (failed.Contains(compiler))
                    {
                        yield return testCase;
                        continue;
                    }

                    var bench = RunBenchmark(compiler, testCase, codeFilePath, testCase.NumberFunctions);
                    if (!bench.HasValue)
                        failed.Add(compiler);

                    yield return bench.HasValue
                        ? testCase with
                        {
                            TimeSeconds = bench.Value.Elapsed.TotalSeconds,
                            MemoryKB = bench.Value.MaxResidentSetSizeKilobytes
                        }
                        : testCase with { Error = "Failed" };

                    // Cleanup temporary files left by compiler
                    foreach (var file in new DirectoryInfo(Directory.GetCurrentDirectory())
                        .WalkFiles()
                        .Where(file => !Regex.IsMatch(file.FullName, doNotDelete)))
                        File.Delete(file.FullName);
                }
                finally
                {
                    Directory.SetCurrentDirectory("..");
                }
            }
        }

        static void StartBench(List<CompilerInfo> compilers, List<TestCase> testCases)
        {
            // Create and step into 'testfiles' directory
            var writeTo = $"./testfiles";
            if (!Directory.Exists(writeTo))
                Directory.CreateDirectory(writeTo);
            Directory.SetCurrentDirectory(writeTo);

            var now = DateTime.Now;

            // Record system information if it's not there
            var systemFilename = $"./{now:yyyyMMddHHmm}_system.txt";
            if (!File.Exists(systemFilename))
            {
                var info = BasicSystemInfo.Find();
                var infoText = new[] { info.OS, info.CPU, info.Memory }.Join("\n\n");
                File.WriteAllText(systemFilename, infoText);
            }

            // Collect benchmarks!
            using var writer = new StreamWriter($"./{now:yyyyMMddHHmm}_results.csv");
            using var csv = new CsvWriter(writer, CultureInfo.InvariantCulture);
            csv.WriteHeader<TestCase>();

            // Run
            var lazyBenchmarks = RunBenchmarks(compilers, testCases);
            foreach (var b in lazyBenchmarks)
            {
                csv.WriteRecord(b);
            }
        }

        record CompilerInfo(string Language, string Extension, string Compiler, string VersionArgument)
        {
            public string CheckCompilerAndSetVersion()
            {
                using var p = new Process();
                p.StartInfo.FileName = Compiler;
                p.StartInfo.Arguments = VersionArgument;
                p.StartInfo.UseShellExecute = false;
                p.StartInfo.RedirectStandardOutput = true;
                p.StartInfo.RedirectStandardError = true;
                p.Start();
                var sout = p.StandardOutput.ReadToEnd();
                var serr = p.StandardError.ReadToEnd();
                var o = string.IsNullOrWhiteSpace(sout) ? serr : sout;
                if (o == null)
                    return "";

                var line = o
                    .Split('\n')
                    .FirstOrDefault(x => !string.IsNullOrWhiteSpace(x))?.Trim();
                if (line == null)
                    return "";

                var r = new System.Text.RegularExpressions.Regex(@"(v?\d[\d\w.-]+)");
                var m = r.Match(line);
                return line;
            }
        }

        record TestCase(int NumberFunctions,
            string Language, string Compiler,
            string Arguments, string Environment,
            double? TimeSeconds, double? MemoryKB, string Error)
        {
            public Dictionary<string, string> EnvironmentVariables =>
                !string.IsNullOrWhiteSpace(Environment)
                    ? JsonSerializer.Deserialize<Dictionary<string, string>>(Environment) : new();
        }

        static void Main()
        {
            var compilerPath = "./compilers.csv";
            var testCasesPath = "./empty.csv";

            var compilers = new FileInfo(compilerPath).ReadAllCsv<CompilerInfo>();
            var testCases = new FileInfo(testCasesPath).ReadAllCsv<TestCase>();

            try
            {
                StartBench(compilers, testCases);
            }
            catch (Exception e)
            {
                Console.WriteLine(e.ToString());
            }
        }

        static string GetCsProj() =>
            @"<Project Sdk=""Microsoft.NET.Sdk""><PropertyGroup><OutputType>Exe</OutputType>" +
            @"<TargetFramework>netcoreapp5.0</TargetFramework></PropertyGroup></Project>";

        static string GetFsProj(string file) =>
            @"<Project Sdk=""Microsoft.NET.Sdk""><PropertyGroup><OutputType>Exe</OutputType>" +
            @"<TargetFramework>netcoreapp5.0</TargetFramework></PropertyGroup>" +
            @"<ItemGroup><Compile Include=""$FILE"" /></ItemGroup></Project>"
                .Replace("$FILE", file);
    }
}