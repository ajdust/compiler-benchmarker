using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text.Json;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;
using CsvHelper;
using Polly;

namespace CompilerBenchmarker
{
    static class Benchmarker
    {
        static async Task<TimingResult> TimeBenchmarkAsync(CompilerInfo compiler, TestCase testCase, string args)
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

                var cts = new CancellationTokenSource();
                cts.CancelAfter(600_000);
                try
                {
                    await p.WaitForExitAsync(cts.Token);
                    // See https://www.meziantou.net/process-waitforexitasync-doesn-t-behave-like-process-waitforexit.htm
                    // ReSharper disable once MethodHasAsyncOverloadWithCancellation
                    p.WaitForExit();
                    
                    // Avoid "System.InvalidOperationException: Process must exit before requested information can be determined."
                    while (!p.HasExited)
                    {
                        cts.Token.ThrowIfCancellationRequested();
                        await Task.Delay(250, cts.Token);
                    }
                    var exitCode = Policy.Handle<InvalidOperationException>()
                        .WaitAndRetry(10, (i) => TimeSpan.FromSeconds(i))
                        .Execute(() => p.ExitCode);
                    
                    if (exitCode != 0)
                    {
                        Thread.Sleep(2500);
                        return new TimingResult(0, 0,
                            $"{exitCode} {sout.Join(";").Replace("\n", "|")}");
                    }
                }
                catch (TaskCanceledException)
                {
                    sout.Add("[Process cancelled after 10 minute timeout]");
                    p?.Kill();
                    return new TimingResult(0, 0, $"-1 {sout.Join(";").Replace("\n", "|")}");
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
                    return new TimingResult(0, 0,
                        $"{exitCode} {sout.Join(";").Replace("\n", "|")}");
                }

                var elapsedSeconds = double.Parse(results[2]);
                var maxResidentSetSize = int.Parse(results[3]);
                var timing = new TimingResult(elapsedSeconds, maxResidentSetSize);
                Console.WriteLine($"  - Took {timing.ElapsedSeconds} MRSS {maxResidentSetSize}");
                return timing;
            }

            throw new Exception("Result of /usr/bin/time not found in output of {" + string.Join("\n", sout) + "}");
        }

        static async Task<TimingResult> RunBenchmarkAsync(CompilerInfo compiler, TestCase testCase, string codeFilePath,
            int numFun)
        {
            // dotnet cli requires project file
            var isDotnet = compiler.Compiler == "dotnet";
            if (isDotnet)
            {
                using var p = Process.Start(compiler.Compiler, $"restore CB.{compiler.Extension}proj");
                await p.WaitForExitAsync();
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
            return await TimeBenchmarkAsync(compiler, testCase, args);
        }

        static async IAsyncEnumerable<TestCase> RunBenchmarks(
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
            var byCompiler = compilers.ToDictionary(x => (x.Compiler, x.Language));

            foreach (var testCase in testCases)
            {
                Console.WriteLine($"Benchmarking {testCase}:");

                var compiler = byCompiler[(testCase.Compiler, testCase.Language)];
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

                    var (elapsedSeconds, maxResidentSetSizeKb, error) =
                        await RunBenchmarkAsync(compiler, testCase, codeFilePath, testCase.NumberFunctions);
                    if (error is not null)
                        failed.Add(compiler);

                    yield return error is null
                        ? testCase with
                        {
                            TimeSeconds = elapsedSeconds,
                            MemoryKB = maxResidentSetSizeKb
                        }
                        : testCase with { Error = error };

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

        static async Task StartBenchAsync(List<CompilerInfo> compilers, List<TestCase> testCases, string outpath)
        {
            // Create and step into 'testfiles' directory
            var writeTo = $"./testfiles";
            if (!Directory.Exists(writeTo))
                Directory.CreateDirectory(writeTo);
            Directory.SetCurrentDirectory(writeTo);

            // Collect benchmarks!
            await using var writer = new StreamWriter(outpath);
            await using var csv = new CsvWriter(writer, CultureInfo.InvariantCulture);
            csv.WriteHeader<TestCase>();

            // Run
            var lazyBenchmarks = RunBenchmarks(compilers, testCases);
            await foreach (var b in lazyBenchmarks)
            {
                await csv.NextRecordAsync();
                csv.WriteRecord(b);
                await csv.FlushAsync();
            }
        }

        record CompilerInfo(string Language, string Extension, string Compiler, string VersionArgument)
        {
            public CompilerInfo GetVersion()
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
                var line = o
                    .Split('\n')
                    .FirstOrDefault(x => !string.IsNullOrWhiteSpace(x))?.Trim();

                return this with { VersionArgument = line };
            }
        }

        record TestCase(int NumberFunctions,
            string Language, string Compiler,
            string Arguments, string Environment,
            double? TimeSeconds, double? MemoryKB, string Error)
        {
            public Dictionary<string, string> EnvironmentVariables =>
                !string.IsNullOrWhiteSpace(Environment)
                    ? Environment.Split(';').ToDictionary(x => x.Split(':')[0], x => x.Split(':')[1])
                    : new();
        }

        record TimingResult(double ElapsedSeconds, double MaxResidentSetSizeKb, string Error = null);

        static async Task Main()
        {
            var now = DateTime.Now;
            var compilerPath = "./compilers.csv";
            var testCasesPath = "./empty.csv";
            var systemOutPath = $"./{now:yyyyMMddHHmm}_system.txt";
            var compilerOutPath = $"{now:yyyyMMddHHmm}_compilers.csv";
            var testCasesOutPath = $"{now:yyyyMMddHHmm}_results.csv";

            var compilers = new FileInfo(compilerPath).ReadAllCsv<CompilerInfo>();
            Console.WriteLine($"Found {compilers.Count} compilers ({compilerOutPath})");
            {
                Console.WriteLine($"Recording compiler versions");
                compilers = compilers.Select(x => x.GetVersion()).ToList();
                using var writer = new StreamWriter(compilerOutPath);
                using var csv = new CsvWriter(writer, CultureInfo.InvariantCulture);
                csv.WriteHeader<CompilerInfo>();
                csv.WriteRecords(compilers);
            }

            // Record system information if it's not there
            if (!File.Exists(systemOutPath))
            {
                Console.WriteLine($"Getting system information ({systemOutPath})");
                var info = BasicSystemInfo.Find();
                var infoText = new[] { info.OS, info.CPU, info.Memory }.Join("\n\n");
                File.WriteAllText(systemOutPath, infoText);
            }

            var testCases = new FileInfo(testCasesPath).ReadAllCsv<TestCase>();

            try
            {
                await StartBenchAsync(compilers, testCases, testCasesOutPath);
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