using System;
using System.Linq;
using System.Collections.Generic;

namespace CompilerBenchmarker.CodeGen
{
    interface ILang
    {
        string Extension { get; }
        IEnumerable<string> GetProgramLines(Program program);
    }

    /// <summary>
    /// Base C-style language with statements and semicolons to end lines.
    /// </summary>
    abstract class BaseCStyleLang : ILang
    {
        public abstract string Extension { get; }
        protected abstract string Main { get; }
        protected abstract string PrintFunctionFormat { get; }
        protected abstract string FunctionPrefix { get; }
        protected abstract string ConstIntType { get; }
        protected virtual string IntType => "int";
        protected const string IndentSpaces = "    ";
        protected const string P = "p"; // function parameter name

        protected virtual string GetBinaryOperator(BinaryOperator op) =>
            op switch
            {
                BinaryOperator.BitAnd => "&",
                BinaryOperator.Minus => "-",
                BinaryOperator.Multiply => "*",
                BinaryOperator.BitOr => "|",
                BinaryOperator.Plus => "+",
                BinaryOperator.Xor => "^",
                _ => throw new ArgumentOutOfRangeException(nameof(op))
            };

        protected virtual string GetExpression(IExpr expr) =>
            expr switch
            {
                Literal literal => literal.Text,
                Variable variable => variable.VariableName,
                FunctionCall functionCall =>
                    $"{functionCall.FunctionName}({GetExpression(functionCall.Argument)})",
                BinaryOperation binOp =>
                    $"({GetExpression((binOp.LeftOperand))} {GetBinaryOperator(binOp.Operator)} {GetExpression((binOp.RightOperand))})",
                _ => throw new ArgumentOutOfRangeException(nameof(expr))
            };

        protected virtual string GetStatement(IStatement statement, HashSet<Variable> assignedTo) =>
            statement switch
            {
                Assignment assignment =>
                    $"{assignment.Variable.VariableName} = {GetExpression(assignment.AssignedExpression)};",
                VariableDeclaration variableDeclaration => assignedTo.Contains(variableDeclaration.Variable)
                    ? $"{IntType} {variableDeclaration.Variable.VariableName} = {GetExpression(variableDeclaration.Initializer)};"
                    : $"{ConstIntType} {variableDeclaration.Variable.VariableName} = {GetExpression(variableDeclaration.Initializer)};",
                Return ret => $"return {GetExpression(ret.Expr)};",
                Print print => $"{PrintFunctionFormat.Replace("$V", print.Variable.VariableName)};",
                _ => throw new ArgumentOutOfRangeException(nameof(statement))
            };

        protected virtual IEnumerable<string> GetFunctionDeclarationLines(FunctionDeclaration fun)
        {
            var assignedTo = new HashSet<Variable>(
                fun.Statements.OfType<Assignment>().Select(a => a.Variable));

            yield return (fun is MainFunctionDeclaration)
                ? $"{FunctionPrefix}{IntType} {Main} {{"
                : $"{FunctionPrefix}{IntType} {fun.FunctionName}({ConstIntType} {P}) {{";

            foreach (var statement in fun.Statements)
                yield return $"    {GetStatement(statement, assignedTo)}";

            yield return "}";
        }

        public virtual IEnumerable<string> GetProgramCoreLines(Program program)
        {
            foreach (var fun in program.Functions)
            {
                foreach (var line in GetFunctionDeclarationLines(fun))
                    yield return line;
                yield return "";
            }

            foreach (var line in GetFunctionDeclarationLines(program.Main))
                yield return line;
        }

        public abstract IEnumerable<string> GetProgramLines(Program program);
    }

    class CSharpLang : BaseCStyleLang
    {
        public override string Extension => "cs";
        protected override string PrintFunctionFormat => "Console.WriteLine($V)";
        protected override string Main => "Main()";
        protected override string FunctionPrefix => "static ";
        protected override string ConstIntType => IntType;

        public override IEnumerable<string> GetProgramLines(Program program)
        {
            yield return "using System;";
            yield return "";
            yield return "namespace GeneratedCode";
            yield return "{";
            yield return "    static class GeneratedFunctions";
            yield return "    {";
            foreach (var line in GetProgramCoreLines(program))
                yield return $"{IndentSpaces}{IndentSpaces}{line}";
            yield return "    }";
            yield return "}";
        }
    }

    class JavaLang : BaseCStyleLang
    {
        public override string Extension => "java";
        protected override string PrintFunctionFormat => "System.out.println($V)";
        protected override string Main => "Main()";
        protected override string FunctionPrefix => "static ";
        protected override string ConstIntType => IntType;

        public override IEnumerable<string> GetProgramLines(Program program)
        {
            yield return "package GeneratedCode;";
            yield return "";
            yield return "class GeneratedFunctions {";
            foreach (var line in GetProgramCoreLines(program))
                yield return $"{IndentSpaces}{line}";
            yield return "}";
        }
    }

    class CLang : BaseCStyleLang
    {
        public override string Extension => "c";
        protected override string PrintFunctionFormat => @"printf(""%i\n"", $V)";
        protected override string Main => "main(void)";
        protected override string FunctionPrefix => "";
        protected override string ConstIntType => $"const {IntType}";

        public override IEnumerable<string> GetProgramLines(Program program)
        {
            yield return "#include <stdio.h>";
            yield return "";
            foreach (var line in GetProgramCoreLines(program))
                yield return line;
        }
    }

    class CppLang : BaseCStyleLang
    {
        public override string Extension => "cpp";
        protected override string PrintFunctionFormat => "std::cout << $V << std::endl";
        protected override string Main => "main()";
        protected override string FunctionPrefix => "";
        protected override string ConstIntType => $"const {IntType}";

        public override IEnumerable<string> GetProgramLines(Program program)
        {
            yield return "#include <iostream>";
            yield return "";
            foreach (var line in GetProgramCoreLines(program))
                yield return line;
        }
    }

    class DLang : BaseCStyleLang
    {
        public override string Extension => "d";
        protected override string PrintFunctionFormat => "writeln($V)";
        protected override string Main => "main()";
        protected override string FunctionPrefix => "";
        protected override string ConstIntType => $"immutable({IntType})";

        public override IEnumerable<string> GetProgramLines(Program program)
        {
            yield return "import std.stdio;";
            yield return "";
            foreach (var line in GetProgramCoreLines(program))
                yield return line;
        }
    }

    /// <summary>
    /// Base for languages with no semicolon ending or whitespace-indentation semantics such as F# and Haskell
    /// </summary>
    abstract class BaseOtherLang : ILang
    {
        public abstract string Extension { get; }
        protected abstract string IntType { get; }
        protected abstract string Main { get; }
        protected abstract string PrintFunctionFormat { get; }
        protected abstract string FunctionPrefix { get; }
        protected abstract string AssignmentOperator { get; }
        protected abstract string MutableDeclaration { get; }
        protected abstract string ImmutableDeclaration { get; }
        protected abstract bool ML { get; }
        protected virtual string IndentSpaces => "    ";
        protected virtual string FunctionWrapStart => "{";
        protected virtual string FunctionWrapEnd => "}";
        protected virtual string VariableTypeIndicator => ": ";
        protected virtual string FunctionTypeIndicator => ": ";
        protected virtual string P => "p";

        protected virtual string GetBinaryOperator(BinaryOperator op) =>
            op switch
            {
                BinaryOperator.BitAnd => "&",
                BinaryOperator.Minus => "-",
                BinaryOperator.Multiply => "*",
                BinaryOperator.BitOr => "|",
                BinaryOperator.Plus => "+",
                BinaryOperator.Xor => "^",
                _ => throw new ArgumentOutOfRangeException(nameof(op))
            };

        protected virtual string GetExpression(IExpr expr) =>
            expr switch
            {
                Literal literal => literal.Text,
                Variable variable => variable.VariableName,
                FunctionCall functionCall =>
                    $"{functionCall.FunctionName}{(ML ? " " : "")}({GetExpression(functionCall.Argument)})",
                BinaryOperation binOp =>
                    $"({GetExpression(binOp.LeftOperand)} {GetBinaryOperator(binOp.Operator)} {GetExpression(binOp.RightOperand)})",
                _ => throw new ArgumentOutOfRangeException(nameof(expr))
            };

        protected virtual string GetStatement(IStatement statement, HashSet<Variable> assignedTo) =>
            statement switch
            {
                Assignment assignment =>
                    $"{assignment.Variable.VariableName} {AssignmentOperator} {GetExpression(assignment.AssignedExpression)}",
                VariableDeclaration variableDeclaration => assignedTo.Contains(variableDeclaration.Variable)
                    ? $"{MutableDeclaration}{variableDeclaration.Variable.VariableName}{VariableTypeIndicator}{IntType} = {GetExpression(variableDeclaration.Initializer)}"
                    : $"{ImmutableDeclaration}{variableDeclaration.Variable.VariableName}{VariableTypeIndicator}{IntType} = {GetExpression(variableDeclaration.Initializer)}",
                Return ret => GetExpression(ret.Expr),
                Print print => PrintFunctionFormat.Replace("$V", print.Variable.VariableName),
                _ => throw new ArgumentOutOfRangeException(nameof(statement))
            };

        protected virtual IEnumerable<string> GetFunctionDeclarationLines(FunctionDeclaration fun)
        {
            var assignedTo = new HashSet<Variable>(
                fun.Statements.OfType<Assignment>().Select(a => a.Variable));

            var isMain = fun is MainFunctionDeclaration;
            yield return isMain
                ? $"{Main} {FunctionWrapStart}"
                : $"{FunctionPrefix} {fun.FunctionName}({P}{VariableTypeIndicator}{IntType}){FunctionTypeIndicator}{IntType} {FunctionWrapStart}";

            foreach (var statement in fun.Statements)
            {
                if (isMain && statement is Return)
                    continue;

                yield return $"{IndentSpaces}{GetStatement(statement, assignedTo)}";
            }

            if (!string.IsNullOrEmpty(FunctionWrapEnd))
                yield return FunctionWrapEnd;
        }

        protected virtual IEnumerable<string> GetProgramCoreLines(Program program)
        {
            foreach (var fun in program.Functions)
            {
                foreach (var line in GetFunctionDeclarationLines(fun))
                    yield return line;
                yield return "";
            }

            foreach (var line in GetFunctionDeclarationLines(program.Main))
                yield return line;
        }

        public abstract IEnumerable<string> GetProgramLines(Program program);
    }

    class GoLang : BaseOtherLang
    {
        public override string Extension => "go";
        protected override string IntType => "int";
        protected override string Main => "func main()";
        protected override string PrintFunctionFormat => "fmt.Println($V)";
        protected override string FunctionPrefix => "func";
        protected override string AssignmentOperator => "=";
        protected override string MutableDeclaration => "";
        protected override string ImmutableDeclaration => "";
        protected override bool ML => false;
        protected override string IndentSpaces => "\t";
        protected override string FunctionTypeIndicator => " ";
        protected override string VariableTypeIndicator => " ";

        protected override string GetStatement(IStatement statement, HashSet<Variable> assignedTo) =>
            statement switch
            {
                VariableDeclaration variableDeclaration =>
                    $"var {variableDeclaration.Variable.VariableName} {IntType} = {GetExpression(variableDeclaration.Initializer)}",
                Return ret => $"return {GetExpression(ret.Expr)}",
                _ => base.GetStatement(statement, assignedTo)
            };

        public override IEnumerable<string> GetProgramLines(Program program)
        {
            yield return @"package main";
            yield return @"import(""fmt"")";
            yield return "";
            foreach (var line in GetProgramCoreLines(program))
                yield return line;
        }
    }

    class ScalaLang : BaseOtherLang
    {
        public override string Extension => "scala";
        protected override string IntType => "Int";
        protected override string Main => "def main(): Unit";
        protected override string PrintFunctionFormat => "println($V)";
        protected override string FunctionPrefix => "def";
        protected override string AssignmentOperator => "=";
        protected override string MutableDeclaration => "var ";
        protected override string ImmutableDeclaration => "val ";
        protected override bool ML => false;
        protected override string IndentSpaces => "  ";
        protected override string FunctionWrapStart => "= {";

        public override IEnumerable<string> GetProgramLines(Program program)
        {
            yield return "object GeneratedFunctions {";
            foreach (var line in GetProgramCoreLines(program))
                yield return $"{IndentSpaces}{line}";
            yield return "}";
        }
    }

    class KotlinLang : BaseOtherLang
    {
        public override string Extension => "kt";
        protected override string IntType => "Int";
        protected override string Main => "fun main()";
        protected override string PrintFunctionFormat => "println($V)";
        protected override string FunctionPrefix => "fun";
        protected override string AssignmentOperator => "=";
        protected override string MutableDeclaration => "var ";
        protected override string ImmutableDeclaration => "val ";
        protected override bool ML => false;

        protected override string GetBinaryOperator(BinaryOperator op) =>
            op switch
            {
                BinaryOperator.BitAnd => "and",
                BinaryOperator.Minus => "-",
                BinaryOperator.Multiply => "*",
                BinaryOperator.BitOr => "or",
                BinaryOperator.Plus => "+",
                BinaryOperator.Xor => "xor",
                _ => throw new ArgumentOutOfRangeException(nameof(op))
            };

        protected override string GetStatement(IStatement statement, HashSet<Variable> assignedTo) =>
            (statement is Return ret)
                ? $"return {GetExpression(ret.Expr)}"
                : base.GetStatement(statement, assignedTo);

        public override IEnumerable<string> GetProgramLines(Program program)
        {
            yield return "class GeneratedFunctions {";
            foreach (var line in GetProgramCoreLines(program))
                yield return $"{IndentSpaces}{line}";
            yield return "}";
        }
    }

    class OCamlLang : BaseOtherLang
    {
        public override string Extension => "ml";
        protected override string IntType => "int";
        protected override string Main => "let main ()";
        protected override string PrintFunctionFormat => @"Printf.printf ""%i\n"" $V;;";
        protected override string FunctionPrefix => "let";

        protected override string AssignmentOperator => "=";

        // choosing to ignore OCaml mutable ref for simplicity
        protected override string MutableDeclaration => "let ";
        protected override string ImmutableDeclaration => "let ";
        protected override bool ML => true;
        protected override string FunctionWrapStart => "=";
        protected override string FunctionWrapEnd => "";

        protected override string GetBinaryOperator(BinaryOperator op) =>
            op switch
            {
                BinaryOperator.BitAnd => "land",
                BinaryOperator.Minus => "-",
                BinaryOperator.Multiply => "*",
                BinaryOperator.BitOr => "lor",
                BinaryOperator.Plus => "+",
                BinaryOperator.Xor => "lxor",
                _ => throw new ArgumentOutOfRangeException(nameof(op))
            };

        protected override string GetStatement(IStatement statement, HashSet<Variable> assignedTo) =>
            statement switch
            {
                // handle "let <expr> in"
                Print print => PrintFunctionFormat.Replace("$V", GetExpression(print.Variable)),
                Return ret => GetExpression(ret.Expr),
                Assignment assignment =>
                    $"let {assignment.Variable.VariableName} {AssignmentOperator} {GetExpression(assignment.AssignedExpression)} in",
                _ => base.GetStatement(statement, assignedTo) + " in"
            };

        public override IEnumerable<string> GetProgramLines(Program program)
        {
            foreach (var line in GetProgramCoreLines(program))
                yield return line;
            yield return "";
            yield return "main ()";
        }
    }

    class FSharpLang : BaseOtherLang
    {
        public override string Extension => "fs";
        protected override string IntType => "int";
        protected override string Main => "[<EntryPoint>]\nlet main args";
        protected override string PrintFunctionFormat => @"printfn ""%i\n"" $V";
        protected override string FunctionPrefix => "let";
        protected override string AssignmentOperator => "<-";
        protected override string MutableDeclaration => "let mutable ";
        protected override string ImmutableDeclaration => "let ";
        protected override bool ML => true;
        protected override string FunctionWrapStart => "=";
        protected override string FunctionWrapEnd => "";

        protected override string GetBinaryOperator(BinaryOperator op) =>
            op switch
            {
                BinaryOperator.BitAnd => "&&&",
                BinaryOperator.Minus => "-",
                BinaryOperator.Multiply => "*",
                BinaryOperator.BitOr => "|||",
                BinaryOperator.Plus => "+",
                BinaryOperator.Xor => "^^^",
                _ => throw new ArgumentOutOfRangeException(nameof(op))
            };

        public override IEnumerable<string> GetProgramLines(Program program)
        {
            foreach (var line in GetProgramCoreLines(program))
                yield return line;
            yield return $"{IndentSpaces}0";
        }
    }

    class HaskellLang : BaseOtherLang
    {
        public override string Extension => "hs";
        protected override string IntType => "Int32";
        protected override string Main => "main :: IO ()\nmain ";
        protected override string PrintFunctionFormat => @"printf ""%i\n"" $V";
        protected override string FunctionPrefix => "";

        protected override string AssignmentOperator => "=";

        // choose to ignore Haskell's Data.IORef for simplicity
        protected override string MutableDeclaration => "let ";
        protected override string ImmutableDeclaration => "let ";
        protected override bool ML => true;
        protected override string FunctionWrapStart => "=";
        protected override string FunctionWrapEnd => "";

        protected override string GetBinaryOperator(BinaryOperator op) =>
            op switch
            {
                BinaryOperator.BitAnd => "&&&",
                BinaryOperator.Minus => "-",
                BinaryOperator.Multiply => "*",
                BinaryOperator.BitOr => "|||",
                BinaryOperator.Plus => "+",
                BinaryOperator.Xor => "^^^",
                _ => throw new ArgumentOutOfRangeException(nameof(op))
            };

        protected override string GetStatement(IStatement statement, HashSet<Variable> assignedTo) =>
            statement switch
            {
                Assignment assignment =>
                    $"let {assignment.Variable.VariableName} {AssignmentOperator} {GetExpression(assignment.AssignedExpression)} in",
                VariableDeclaration variableDeclaration =>
                    $"{ImmutableDeclaration}({variableDeclaration.Variable.VariableName} :: {IntType}) {AssignmentOperator} {GetExpression(variableDeclaration.Initializer)} in",
                Return ret => GetExpression(ret.Expr),
                Print print => PrintFunctionFormat.Replace("$V", print.Variable.VariableName),
                _ => throw new ArgumentOutOfRangeException(nameof(statement))
            };

        protected override IEnumerable<string> GetFunctionDeclarationLines(FunctionDeclaration fun)
        {
            var assignedTo = new HashSet<Variable>(
                fun.Statements.OfType<Assignment>().Select(a => a.Variable));

            var isMain = fun is MainFunctionDeclaration;
            yield return isMain
                ? $"{Main} {FunctionWrapStart}"
                : $"{fun.FunctionName} :: {IntType} -> {IntType}\n{fun.FunctionName} {P} {FunctionWrapStart}";

            foreach (var statement in fun.Statements)
            {
                if (isMain && statement is Return)
                    continue;

                yield return $"{IndentSpaces}{GetStatement(statement, assignedTo)}";
            }
        }

        public override IEnumerable<string> GetProgramLines(Program program)
        {
            yield return "{-# LANGUAGE ScopedTypeVariables #-}";
            yield return "import GHC.Int (Int32)";
            yield return "import Data.Bits ((.&.), (.|.), xor)";
            yield return "import Text.Printf (printf)";
            yield return "";
            // avoid 'cannot construct the infinite type' error by specifying
            yield return "(&&&) :: Int32 -> Int32 -> Int32";
            yield return "a &&& b = a .&. b";
            yield return "(|||) :: Int32 -> Int32 -> Int32";
            yield return "a ||| b = a .|. b";
            yield return "(^^^) :: Int32 -> Int32 -> Int32";
            yield return "a ^^^ b = a `xor` b";
            yield return "";

            foreach (var line in GetProgramCoreLines(program))
                yield return line;
        }
    }

    class RustLang : BaseOtherLang
    {
        public override string Extension => "rs";
        protected override string IntType => "w32";
        protected override string Main => "fn main()";
        protected override string PrintFunctionFormat => @"println!(""{}"", $V)";
        protected override string FunctionPrefix => "fn";
        protected override string AssignmentOperator => "=";
        protected override string MutableDeclaration => "let mut ";
        protected override string ImmutableDeclaration => "let ";
        protected override string FunctionTypeIndicator => " -> ";
        protected override bool ML => false;

        protected override string GetExpression(IExpr expr) =>
            expr is Literal l ? $"Wrapping({l.Text})" : base.GetExpression(expr);

        protected override string GetStatement(
            IStatement statement, HashSet<Variable> assignedTo) =>
            statement is Return ret ? GetExpression(ret.Expr) : base.GetStatement(statement, assignedTo) + ";";

        public override IEnumerable<string> GetProgramLines(Program program)
        {
            yield return "#![allow(unused_parens)]\n";
            yield return "use std::num::Wrapping;\n";
            yield return "type w32 = std::num::Wrapping<i32>;\n";

            foreach (var line in GetProgramCoreLines(program))
                yield return line;
        }
    }

    class SwiftLang : BaseOtherLang
    {
        public override string Extension => "swift";
        protected override string IntType => "Int";
        protected override string Main => "func main()";
        protected override string PrintFunctionFormat => @"print($V)";
        protected override string FunctionPrefix => "func";
        protected override string AssignmentOperator => "=";
        protected override string MutableDeclaration => "var ";
        protected override string ImmutableDeclaration => "let ";
        protected override string FunctionTypeIndicator => " -> ";
        protected override bool ML => false;

        protected override string GetExpression(IExpr expr) =>
            expr is FunctionCall functionCall
                ? $"{functionCall.FunctionName}({P}: {GetExpression(functionCall.Argument)})"
                : base.GetExpression(expr);

        protected override string GetStatement(
            IStatement statement, HashSet<Variable> assignedTo) =>
            statement is Return ret
                ? $"return {GetExpression(ret.Expr)}"
                : base.GetStatement(statement, assignedTo);

        public override IEnumerable<string> GetProgramLines(Program program)
        {
            foreach (var line in GetProgramCoreLines(program))
                yield return line;
            yield return "";
            yield return "main()";
        }
    }

    class NimLang : BaseOtherLang
    {
        public override string Extension => "nim";
        protected override string IntType => "int32";
        protected override string Main => "proc main()";
        protected override string PrintFunctionFormat => @"echo $V";
        protected override string FunctionPrefix => "proc";
        protected override string AssignmentOperator => "=";
        protected override string MutableDeclaration => "var ";
        protected override string ImmutableDeclaration => "let ";
        protected override string FunctionWrapStart => "=";
        protected override string FunctionWrapEnd => "";
        protected override bool ML => false;

        protected override string GetBinaryOperator(BinaryOperator op) =>
            op switch
            {
                BinaryOperator.BitAnd => "and",
                BinaryOperator.Minus => "-",
                BinaryOperator.Multiply => "*",
                BinaryOperator.BitOr => "or",
                BinaryOperator.Plus => "+",
                BinaryOperator.Xor => "xor",
                _ => throw new ArgumentOutOfRangeException(nameof(op))
            };

        public override IEnumerable<string> GetProgramLines(Program program)
        {
            foreach (var line in GetProgramCoreLines(program))
                yield return line;
            yield return "";
            yield return "main()";
        }
    }

    class CrystalLang : BaseOtherLang
    {
        public override string Extension => "cr";
        protected override string IntType => "Int32";
        protected override string Main => "def main()";
        protected override string PrintFunctionFormat => @"puts $V";
        protected override string FunctionPrefix => "def";
        protected override string AssignmentOperator => "=";
        protected override string MutableDeclaration => "";
        protected override string ImmutableDeclaration => "";
        protected override string FunctionWrapStart => "";
        protected override string FunctionWrapEnd => "end";
        protected override string FunctionTypeIndicator => " : ";
        protected override string P => "p ";
        protected override bool ML => false;

        public override IEnumerable<string> GetProgramLines(Program program)
        {
            foreach (var line in GetProgramCoreLines(program))
                yield return line;
            yield return "";
            yield return "main()";
        }
    }
}