using System;
using System.Linq;
using System.Collections.Generic;
using System.IO;

namespace CompilerBenchmarker2
{
    #region Data

    interface IExpr {}

    interface IStatement {}

    // e.g. '45'
    class Literal : IExpr
    {
        public string Text { get; }
        public Literal(string text) { Text = text; }
        public Literal(int number) : this(number.ToString()) {}
    }

    // e.g. 'myVariableName'
    class Variable : IExpr
    {
        public string VariableName { get; }
        public Variable(string name) { VariableName = name; }
        public override int GetHashCode() => VariableName.GetHashCode();
        public override bool Equals(object obj) =>
            obj is Variable v && v.VariableName == VariableName;
    }

    // e.g. 'myFunction()'
    class FunctionCall : IExpr
    {
        public string FunctionName { get; }
        public FunctionCall(string name) { FunctionName = name; }
    }

    // e.g. '<expr> * <expr>'
    enum BinaryOperationOperator { Plus, Minus, Multiply, BitAnd, BitOr, Xor }
    class BinaryOperation : IExpr
    {
        public static IEnumerable<Variable> EnumerateVariablesUsed(BinaryOperation binOp)
        {
            {
                if (binOp.LeftOperand is Variable v)
                {
                    yield return v;
                }
                else if (binOp.LeftOperand is BinaryOperation subBinOp)
                {
                    foreach (var sv in EnumerateVariablesUsed(subBinOp))
                        yield return sv;
                }
            }
            {
                if (binOp.RightOperand is Variable v)
                {
                    yield return v;
                }
                else if (binOp.RightOperand is BinaryOperation subBinOp)
                {
                    foreach (var sv in EnumerateVariablesUsed(subBinOp))
                        yield return sv;
                }
            }
        }

        public IEnumerable<Variable> GetVariablesUsed() => EnumerateVariablesUsed(this);

        public IExpr LeftOperand { get; }
        public IExpr RightOperand { get; }
        public BinaryOperationOperator Operator { get; }

        public BinaryOperation(IExpr left, BinaryOperationOperator op, IExpr right)
        {
            LeftOperand = left;
            Operator = op;
            RightOperand = right;
        }
    }

    // e.g. 'myVariableName = <expr>'
    class Assignment : IStatement
    {
        public Variable Variable { get; }
        public IExpr AssignedExpression { get; }
        public Assignment(Variable variable, IExpr assignedExpression)
        {
            Variable = variable;
            AssignedExpression = assignedExpression;
        }
    }

    // e.g. 'int myVariableName = <expr>'
    class VariableDeclaration : IStatement
    {
        public Variable Variable { get; }
        public IExpr Initializer { get; }
        public VariableDeclaration(Variable variable, IExpr initializer)
        {
            Variable = variable;
            Initializer = initializer;
        }
    }

    // e.g. 'return myVariableName'
    class Return : IStatement
    {
        public IExpr Expr { get; }
        public Return(IExpr expr) { Expr = expr; }
    }

    // e.g. 'print(myVariableName)'
    class Print : IStatement
    {
        public Variable Variable { get; }
        public Print(Variable variable) { Variable = variable; }
    }

    interface IFunctionDeclaration {}

    class FunctionDeclaration : IFunctionDeclaration
    {
        public string FunctionName { get; }
        public IList<IStatement> Statements { get; }
        public FunctionDeclaration(string functionName, IList<IStatement> statements)
        {
            FunctionName = functionName;
            Statements = statements;
        }

        public override int GetHashCode() => FunctionName.GetHashCode();
        public override bool Equals(object obj) =>
            obj is FunctionDeclaration f && f.FunctionName == FunctionName;
    }

    class MainFunctionDeclaration : FunctionDeclaration
    {
        public MainFunctionDeclaration(string functionName, IList<IStatement> statements)
            : base(functionName, statements)
        {
        }
    }

    class Program
    {
        public MainFunctionDeclaration Main { get; }
        public ICollection<FunctionDeclaration> Functions { get; }
        public Program(MainFunctionDeclaration main, ICollection<FunctionDeclaration> functions)
        {
            Main = main;
            Functions = functions;
        }
    }

    #endregion Data

    #region Program Generator

    // todo: how to incorporate const/mut (C++, C, Rust, F#, Scala, etc.)
    // todo: how to incorporate Rust borrow checker (no reusing variable)
    static class ProgramGenerator
    {
        public static bool TrueOrFalse(this Random r) => r.Next(1, 3) == 1;
        public static T From<T>(this Random r, IReadOnlyCollection<T> collection) =>
            collection.ElementAt(r.Next(0, collection.Count));

        public static BinaryOperationOperator Operator(this Random random)
        {
            switch (random.Next(1, 6))
            {
                case 1: return BinaryOperationOperator.BitAnd;
                case 2: return BinaryOperationOperator.Minus;
                case 3: return BinaryOperationOperator.Multiply;
                case 4: return BinaryOperationOperator.BitOr;
                case 5: return BinaryOperationOperator.Plus;
                default: return BinaryOperationOperator.Xor;
            }
        }

        public static IExpr Expression(
            this Random random,
            IReadOnlyCollection<Variable> declaredVariables,
            IReadOnlyCollection<FunctionDeclaration> declaredFunctions)
        {
            switch (random.Next(1, 5))
            {
                case 1:
                    return new Literal(random.Next(0, 1000));
                case 2:
                    if (declaredVariables.Any())
                        return random.From(declaredVariables);
                    else
                        return new Literal(random.Next(0, 1000));
                case 3:
                    if (declaredFunctions.Any())
                        return new FunctionCall(random.From(declaredFunctions).FunctionName);
                    else
                        return new Literal(random.Next(0, 1000));
                default: return new BinaryOperation(
                    random.Expression(declaredVariables, declaredFunctions),
                    random.Operator(),
                    random.Expression(declaredVariables, declaredFunctions));
            }
        }

        public static Assignment Assignment(
            this Random random,
            IReadOnlyCollection<Variable> declaredVariables,
            IReadOnlyCollection<FunctionDeclaration> declaredFunctions)
        {
            var to = random.From(declaredVariables);
            var expr = random.Expression(declaredVariables, declaredFunctions);

            // prevent variable self-assignments e.g. 'x0 = x0'
            while (expr is Variable v && v.VariableName == to.VariableName)
            {
                expr = random.Expression(declaredVariables, declaredFunctions);
            }
            return new Assignment(to, expr);
        }

        public static VariableDeclaration MainVariableDeclaration(
            this Random random,
            IReadOnlyCollection<Variable> declaredVariables,
            IReadOnlyCollection<FunctionDeclaration> declaredFunctions)
        {
            IExpr expr = random.Expression(declaredVariables, declaredFunctions);

            foreach (var v in declaredVariables)
                expr = new BinaryOperation(expr, random.Operator(), v);

            return new VariableDeclaration(new Variable("m"), expr);
        }

        public static VariableDeclaration VariableDeclaration(
            this Random random,
            IReadOnlyCollection<Variable> declaredVariables,
            IReadOnlyCollection<FunctionDeclaration> declaredFunctions) =>
            new VariableDeclaration(
                new Variable($"x{declaredVariables.Count}"),
                random.Expression(declaredVariables, declaredFunctions));

        public static Print Print(
            this Random random,
            IReadOnlyCollection<Variable> declaredVariables) =>
            new Print(random.From(declaredVariables));

        public static Return Return(
            this Random random,
            IReadOnlyCollection<Variable> declaredVariables,
            IReadOnlyCollection<FunctionDeclaration> declaredFunctions)
        {
            IExpr expr = random.Expression(declaredVariables, declaredFunctions);

            // Use all variables to avoid unused variable warnings
            foreach (var v in declaredVariables)
                expr = new BinaryOperation(expr, random.Operator(), v);

            return new Return(expr);
        }

        public static FunctionDeclaration FunctionDeclaration(
            this Random random,
            IReadOnlyCollection<FunctionDeclaration> decFuns,
            int maxStatementsPerFunction, bool isMain = false)
        {
            var numStatements = random.Next(1, maxStatementsPerFunction);
            var statements = new List<IStatement>();
            var decVars = new List<Variable>();

            // Variable declaration to start off every function
            var firstDecVar = random.VariableDeclaration(decVars, decFuns);
            statements.Add(firstDecVar);
            decVars.Add(firstDecVar.Variable);

            // Random statements
            for (var _ = 0; _ < numStatements; _ += 1)
            {
                if (random.TrueOrFalse())
                {
                    var decVar = random.VariableDeclaration(decVars, decFuns);
                    statements.Add(decVar);
                    decVars.Add(decVar.Variable);
                }
                else
                {
                    var assignment = random.Assignment(decVars, decFuns);
                    statements.Add(assignment);
                }
            }

            if (isMain)
            {
                // the main declaration prevents unused variable warnings in main
                var m = random.MainVariableDeclaration(decVars, decFuns);
                statements.Add(m);
                statements.Add(new Print(m.Variable));
                statements.Add(new Return(new Literal(0)));
                return new MainFunctionDeclaration($"main", statements);
            }
            else
            {
                statements.Add(random.Return(decVars, decFuns));
                return new FunctionDeclaration($"f{decFuns.Count}", statements);
            }
        }

        public static MainFunctionDeclaration MainFunction(
            this Random random,
            IReadOnlyCollection<FunctionDeclaration> decFuns,
            int maxStatementsPerFunction) =>
            (MainFunctionDeclaration)FunctionDeclaration(
                random, decFuns, maxStatementsPerFunction, true);

        public static Program RandomProgram(int functionCount, int maxStatementsPerFunction)
        {
            var random = new Random();
            var functions = new List<FunctionDeclaration>();
            for (var _ = 0; _ < functionCount; _ += 1)
                functions.Add(random.FunctionDeclaration(functions, maxStatementsPerFunction));

            var main = random.MainFunction(functions, maxStatementsPerFunction);
            return new Program(main, functions);
        }
    }

    #endregion

    #region Languages

    interface ILang
    {
        string Extension { get; }
        IEnumerable<string> GetProgramLines(Program program);
    }

    enum ReturnStyle { Expression, RequiredReturn }

    abstract class BaseImperativeLang : ILang
    {
        public abstract string Extension { get; }
        protected virtual string EndStatement => ";";
        protected virtual string IntType => "int";

        // Rust, Kotlin, Scala, Swift, F# don't need "return"
        protected virtual ReturnStyle ReturnStyle => ReturnStyle.RequiredReturn;
        protected abstract string Main { get; }
        protected abstract string PrintFunctionName { get; }
        protected abstract string MethodPrefix { get; }

        protected virtual string GetBinaryOperator(BinaryOperationOperator op)
        {
            switch (op)
            {
                case BinaryOperationOperator.BitAnd: return "&";
                case BinaryOperationOperator.Minus: return "-";
                case BinaryOperationOperator.Multiply: return "*";
                case BinaryOperationOperator.BitOr: return "|";
                case BinaryOperationOperator.Plus: return "+";
                case BinaryOperationOperator.Xor: return "^";
                default: throw new ArgumentOutOfRangeException(nameof(op));
            }
        }

        protected virtual string GetExpression(IExpr expr)
        {
            switch (expr)
            {
                case Literal literal: return literal.Text;
                case Variable variable: return variable.VariableName;
                case FunctionCall functionCall:
                    return $"{functionCall.FunctionName}()";
                case BinaryOperation binOp:
                    var left = GetExpression(binOp.LeftOperand);
                    var right = GetExpression(binOp.RightOperand);
                    return $"({left} {GetBinaryOperator(binOp.Operator)} {right})";
                default: throw new ArgumentOutOfRangeException(nameof(expr));
            }
        }

        protected virtual string GetStatement(IStatement statement)
        {
            switch (statement)
            {
                case Assignment assignment:
                    var aname = assignment.Variable.VariableName;
                    var aexpr = GetExpression(assignment.AssignedExpression);
                    return $"{aname} = {aexpr}{EndStatement}";
                case VariableDeclaration variableDeclaration:
                    var vname = variableDeclaration.Variable.VariableName;
                    var vexpr = GetExpression(variableDeclaration.Initializer);
                    return $"{IntType} {vname} = {vexpr}{EndStatement}";
                case Return ret:
                    switch (ReturnStyle)
                    {
                        case ReturnStyle.Expression: return GetExpression(ret.Expr);
                        default: return $"return {GetExpression(ret.Expr)}{EndStatement}";
                    }
                case Print print:
                    var pvname = print.Variable.VariableName;
                        return $"{PrintFunctionName}({pvname}){EndStatement}";
                default: throw new ArgumentOutOfRangeException(nameof(statement));
            }
        }

        protected virtual IEnumerable<string> GetFunctionDeclarationLines(FunctionDeclaration fun)
        {
            if (fun is MainFunctionDeclaration main)
                yield return $"{MethodPrefix}{IntType} {Main} {{";
            else
                yield return $"{MethodPrefix}{IntType} {fun.FunctionName}() {{";
            foreach (var statement in fun.Statements)
                yield return $"    {GetStatement(statement)}";
            yield return "}";
        }

        public abstract IEnumerable<string> GetProgramLines(Program program);
    }

    class CSharpLang : BaseImperativeLang
    {
        public override string Extension => "cs";

        protected override string PrintFunctionName => "Console.WriteLine";

        protected override string Main => "Main()";

        protected override string MethodPrefix => "static ";

        public override IEnumerable<string> GetProgramLines(Program program)
        {
            yield return "using System;";
            yield return "";
            yield return "namespace GeneratedCode";
            yield return "{";
            yield return "    static class GeneratedFunctions";
            yield return "    {";
            foreach (var fun in program.Functions)
            {
                foreach (var line in GetFunctionDeclarationLines(fun))
                    yield return $"        {line}";
                yield return "";
            }

            foreach (var line in GetFunctionDeclarationLines(program.Main))
                yield return $"        {line}";

            yield return "    }";
            yield return "}";
        }
    }

    class JavaLang : BaseImperativeLang
    {
        public override string Extension => "java";

        protected override string PrintFunctionName => "System.out.println";

        protected override string Main => "Main()";

        protected override string MethodPrefix => "static ";

        public override IEnumerable<string> GetProgramLines(Program program)
        {
            yield return "package GeneratedCode;";
            yield return "";
            yield return "class GeneratedFunctions {";
            foreach (var fun in program.Functions)
            {
                foreach (var line in GetFunctionDeclarationLines(fun))
                    yield return $"    {line}";
                yield return "";
            }

            foreach (var line in GetFunctionDeclarationLines(program.Main))
                yield return $"    {line}";

            yield return "}";
        }
    }

    class CLang : BaseImperativeLang
    {
        public override string Extension => "c";

        protected override string PrintFunctionName => "CUSTOM_PRINT";

        protected override string Main => "main(void)";

        protected override string MethodPrefix => "";

        public override IEnumerable<string> GetProgramLines(Program program)
        {
            yield return "#include <stdio.h>";
            yield return "";
            yield return @"#define CUSTOM_PRINT(m) printf(""%i"",m)";
            yield return "";
            foreach (var fun in program.Functions)
            {
                foreach (var line in GetFunctionDeclarationLines(fun))
                    yield return line;
                yield return "";
            }

            foreach (var line in GetFunctionDeclarationLines(program.Main))
                yield return line;
        }
    }

    class CppLang : BaseImperativeLang
    {
        public override string Extension => "cpp";

        protected override string PrintFunctionName => "std::cout << ";

        protected override string Main => "main()";

        protected override string MethodPrefix => "";

        public override IEnumerable<string> GetProgramLines(Program program)
        {
            yield return "#include <iostream>";
            yield return "";
            foreach (var fun in program.Functions)
            {
                foreach (var line in GetFunctionDeclarationLines(fun))
                    yield return line;
                yield return "";
            }

            foreach (var line in GetFunctionDeclarationLines(program.Main))
                yield return line;
        }
    }

    class DLang : BaseImperativeLang
    {
        public override string Extension => "d";

        protected override string PrintFunctionName => "writeln";

        protected override string Main => "main()";

        protected override string MethodPrefix => "";

        public override IEnumerable<string> GetProgramLines(Program program)
        {
            yield return "import std.stdio;";
            yield return "";
            foreach (var fun in program.Functions)
            {
                foreach (var line in GetFunctionDeclarationLines(fun))
                    yield return line;
                yield return "";
            }

            foreach (var line in GetFunctionDeclarationLines(program.Main))
                yield return line;
        }
    }

    class GoLang : BaseImperativeLang
    {
        public override string Extension => "go";

        protected override string PrintFunctionName => "fmt.Println";

        protected override string Main => "main()";

        protected override string MethodPrefix => "func ";

        protected override string EndStatement => "";

        public override IEnumerable<string> GetProgramLines(Program program)
        {
            yield return @"package main";
            yield return @"import(""fmt"")";
            yield return "";
            foreach (var fun in program.Functions)
            {
                foreach (var line in GetFunctionDeclarationLines(fun))
                    yield return line;
                yield return "";
            }

            foreach (var line in GetFunctionDeclarationLines(program.Main))
                yield return line;
        }

        protected override string GetStatement(IStatement statement)
        {
            if (statement is VariableDeclaration variableDeclaration)
            {
                var vname = variableDeclaration.Variable.VariableName;
                var vexpr = GetExpression(variableDeclaration.Initializer);
                return $"{vname} := {vexpr}{EndStatement}";
            }
            else
            {
                return base.GetStatement(statement);
            }
        }

        protected override IEnumerable<string> GetFunctionDeclarationLines(FunctionDeclaration fun)
        {
            if (fun is MainFunctionDeclaration main)
            {
                yield return $"{MethodPrefix}{Main} {{";
                foreach (var statement in fun.Statements.Where(x => !(x is Return)))
                    yield return $"    {GetStatement(statement)}";
                yield return "}";
            }
            else
            {
                yield return $"{MethodPrefix}{fun.FunctionName}() {IntType} {{";
                foreach (var statement in fun.Statements)
                    yield return $"    {GetStatement(statement)}";
                yield return "}";
            }
        }
    }

    // todo add:
    // round 2: scala, kotlin, ocaml,
    // round 3: fsharp, haskell, rust

    #endregion

    public class CodeGen2
    {
        ILang GetLang(string lang)
        {
            switch (lang.ToLower())
            {
                case "c++": return new CppLang();
                case "c": return new CLang();
                case "d": return new DLang();
                case "go": return new GoLang();
                // case "rust": return new RustLang();
                // case "ocaml": return new OCamlLang();
                // case "fsharp": return new FSharpLang();
                case "csharp": return new CSharpLang();
                // case "haskell": return new HaskellLang();
                // case "kotlin": return new KotlinLang();
                case "java": return new JavaLang();
                // case "scala": return new ScalaLang();
                default:
                    throw new ArgumentOutOfRangeException(nameof(lang),
                        $"{lang} is not supported by the code generation. So code it up!");
            }
        }

        public void WriteLang(string langname, int numFuns, string filename)
        {
            var lang = GetLang(langname);
            if (File.Exists(filename))
                File.Delete(filename);

            var program = ProgramGenerator.RandomProgram(numFuns, 10);
            using (var f = new StreamWriter(filename))
            {
                foreach (var line in lang.GetProgramLines(program))
                    f.WriteLine(line);
            }
        }
    }
}