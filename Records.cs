using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text.RegularExpressions;

namespace CompilerBenchmarker
{
    class Compiler
    {
        // The name of the language, e.g. "C"
        public string Language;
        // The extension of the files in this language, e.g. "c"
        public string Extension;
        // The compiler executable, e.g. "gcc"
        public string Exe;
        // The arguments to the compiler executabe, e.g. "-O"
        public string OptimizeArguments;
        public string VersionArgument;
        public string MiscArguments;
        public string Version;
        public string VersionTrimmed;
        public Dictionary<string,string> EnvironmentVariables;

        public Compiler(
            string language, string extension, string exe,
            string versionArgument,
            string optimizeArguments = null,
            string miscArguments = null,
            Dictionary<string,string> envVars = null)
        {
            if (string.IsNullOrWhiteSpace(language))
                throw new ArgumentNullException(nameof(language));
            if (string.IsNullOrWhiteSpace(extension))
                throw new ArgumentNullException(nameof(extension));
            if (string.IsNullOrWhiteSpace(exe))
                throw new ArgumentNullException(nameof(exe));
            if (string.IsNullOrWhiteSpace(versionArgument))
                throw new ArgumentNullException(nameof(versionArgument),
                    "Missing option to determine compiler version");

            Language = language;
            Extension = extension;
            Exe = exe;
            VersionArgument = versionArgument;
            OptimizeArguments = optimizeArguments;
            MiscArguments = miscArguments;
            EnvironmentVariables = envVars ?? new Dictionary<string,string>();
            try
            {
                CheckCompilerAndSetVersion();
            }
            catch (Exception e)
            {
                throw new Exception($"{exe} error", e);
            }
        }

        public override string ToString()
        {
            return OptimizeArguments != null
                ? $"{Language} ({Exe} {OptimizeArguments}) [{VersionTrimmed}]"
                : $"{Language} ({Exe}) [{VersionTrimmed}]";
        }

        public string ToVerboseString() => ToString() + " " + Version;

        void CheckCompilerAndSetVersion()
        {
            using (var p = new Process())
            {
                p.StartInfo.FileName = Exe;
                p.StartInfo.Arguments = VersionArgument;
                p.StartInfo.UseShellExecute = false;
                p.StartInfo.RedirectStandardOutput = true;
                p.StartInfo.RedirectStandardError = true;
                p.Start();
                var sout = p.StandardOutput.ReadToEnd();
                var serr = p.StandardError.ReadToEnd();
                var o = string.IsNullOrWhiteSpace(sout) ? serr : sout;
                if (o == null)
                    return;

                var line = o.Split('\n')
                    .Where(x => !string.IsNullOrWhiteSpace(x))
                    .FirstOrDefault()?.Trim();
                if (line == null)
                    return;

                var r = new System.Text.RegularExpressions.Regex(@"(v?\d[\d\w.-]+)");
                var m = r.Match(line);
                Version = line;
                VersionTrimmed = m.Success ? m.Captures.First().Value : o;
            }
        }
    }

    struct TimingResult
    {
        public TimeSpan Elapsed;
        public int MaxResidentSetSizeKilobytes;

        public TimingResult(double elapsedSeconds, int maxResidentSetSizeKilobytes)
        {
            Elapsed = TimeSpan.FromSeconds(elapsedSeconds);
            MaxResidentSetSizeKilobytes = maxResidentSetSizeKilobytes;
        }

        public TimingResult(TimeSpan elapsed)
        {
            Elapsed = elapsed;
            MaxResidentSetSizeKilobytes = -1;
        }
    }

    class CompilerBenchmark
    {
        public Compiler Compiler;
        public TimingResult TimeToCompile;
        public bool Compiled;
        public int NumberFunctions;
        public string SecondsToCompile =>
            Compiled ? TimeToCompile.Elapsed.TotalSeconds.ToString() : "";
        public string MaxMemory =>
            Compiled ? TimeToCompile.MaxResidentSetSizeKilobytes.ToString() : "";

        public static CompilerBenchmark Success(
            Compiler compiler, TimingResult timeToCompile, int numberFunctions)
        {
            if (timeToCompile.Elapsed == TimeSpan.Zero)
            {
                throw new ArgumentException(
                    "Compiling cannot take zero seconds", nameof(timeToCompile));
            }

            return new CompilerBenchmark(compiler, timeToCompile, numberFunctions, true);
        }

        public static CompilerBenchmark Failure(Compiler compiler, int numberFunctions)
            => new CompilerBenchmark(compiler, new TimingResult(TimeSpan.Zero), numberFunctions, false);

        private CompilerBenchmark(
            Compiler compiler, TimingResult timeToCompile, int numberFunctions, bool compiled)
        {
            if (numberFunctions < 0)
            {
                throw new ArgumentException(
                    "Cannot compile zero functions", nameof(numberFunctions));
            }

            Compiler = compiler;
            TimeToCompile = timeToCompile;
            NumberFunctions = numberFunctions;
            Compiled = compiled;
        }
    }

    class CompilerCliComparer : IComparer<Compiler>, IEqualityComparer<Compiler>
    {
        public int Compare(Compiler left, Compiler right)
        {
            var sc = StringComparer.OrdinalIgnoreCase;
            var exeCompared = sc.Compare(left.Exe, right.Exe);
            if (exeCompared == 0)
            {
                var leftArgs = string.Join("", left.OptimizeArguments);
                var rightArgs = string.Join("", right.OptimizeArguments);
                return sc.Compare(leftArgs, rightArgs);
            }

            return exeCompared;
        }

        public bool Equals(Compiler left, Compiler right) =>
            Compare(left, right) == 0;

        public int GetHashCode(Compiler c)
        {
            var h = 4021 ^ c.Language.GetHashCode()
                         ^ c.Exe.GetHashCode()
                         ^ c.VersionTrimmed.GetHashCode();
            return c.OptimizeArguments == null ? h : h ^ c.OptimizeArguments.GetHashCode();
        }
    }
}