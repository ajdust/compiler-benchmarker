using System;
using System.IO;

namespace CompilerBenchmarker.CodeGen
{
    class CodeGenerator
    {
        // Ensure deterministic program creation
        // So every program made with instance/numFuns follows the same formula
        int _seed { get; } = Convert.ToInt32(new Random().NextDouble() * 100000);

        static ILang GetLang(string lang) =>
            lang.ToLower() switch
            {
                "c++" => new CppLang(),
                "c" => new CLang(),
                "d" => new DLang(),
                "go" => new GoLang(),
                "rust" => new RustLang(),
                "ocaml" => new OCamlLang(),
                "fsharp" => new FSharpLang(),
                "csharp" => new CSharpLang(),
                "haskell" => new HaskellLang(),
                "kotlin" => new KotlinLang(),
                "java" => new JavaLang(),
                "scala" => new ScalaLang(),
                "swift" => new SwiftLang(),
                "nim" => new NimLang(),
                "crystal" => new CrystalLang(),
                _ => throw new ArgumentOutOfRangeException(nameof(lang),
                    $"{lang} is not supported by the code generation. So code it up!")
            };

        public void OverwriteLang(string langname, int numFuns, string filepath)
        {
            var lang = GetLang(langname);
            if (File.Exists(filepath))
                File.Delete(filepath);

            var program = ProgramGenerator.RandomProgram(new Random(_seed), numFuns, 10);
            using var f = new StreamWriter(filepath);
            foreach (var line in lang.GetProgramLines(program))
                f.WriteLine(line);
        }
    }
}