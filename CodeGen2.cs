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
        public FunctionCall(string name) => FunctionName = name;
    }

    // e.g. '<expr> * <expr>'
    enum BinaryOperator { Plus, Minus, Multiply, BitAnd, BitOr, Xor }
    class BinaryOperation : IExpr
    {
        public IExpr LeftOperand { get; }
        public IExpr RightOperand { get; }
        public BinaryOperator Operator { get; }

        public BinaryOperation(IExpr left, BinaryOperator op, IExpr right)
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
        public Return(IExpr expr) => Expr = expr;
    }

    // e.g. 'print(myVariableName)'
    class Print : IStatement
    {
        public Variable Variable { get; }
        public Print(Variable variable) => Variable = variable;
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

    static class ProgramGenerator
    {
        public static bool TrueOrFalse(this Random r) => r.Next(1, 3) == 1;
        public static T From<T>(this Random r, IReadOnlyCollection<T> collection) =>
            collection.ElementAt(r.Next(0, collection.Count));

        public static BinaryOperator Operator(this Random random)
        {
            switch (random.Next(1, 6))
            {
                case 1: return BinaryOperator.BitAnd;
                case 2: return BinaryOperator.Minus;
                case 3: return BinaryOperator.Multiply;
                case 4: return BinaryOperator.BitOr;
                case 5: return BinaryOperator.Plus;
                default: return BinaryOperator.Xor;
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

            // ensure all initializers are used; e.g. no 'int a = 10; a = 20;'
            expr = new BinaryOperation(to, random.Operator(), expr);
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

    abstract class BaseImperativeLang : ILang
    {
        public abstract string Extension { get; }
        protected virtual string EndStatement => ";";
        protected virtual string IntType => "int";

        // Rust, Kotlin, Scala, Swift, F# don't need "return"
        protected abstract string Main { get; }
        protected abstract string PrintFunctionName { get; }
        protected abstract string MethodPrefix { get; }

        protected virtual string GetBinaryOperator(BinaryOperator op)
        {
            switch (op)
            {
                case BinaryOperator.BitAnd: return "&";
                case BinaryOperator.Minus: return "-";
                case BinaryOperator.Multiply: return "*";
                case BinaryOperator.BitOr: return "|";
                case BinaryOperator.Plus: return "+";
                case BinaryOperator.Xor: return "^";
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
                    return $"return {GetExpression(ret.Expr)}{EndStatement}";
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

    abstract class BaseExpressionLang : ILang
    {
        public abstract string Extension { get; }
        protected abstract string IntType { get; }
        protected abstract string Main { get; }
        protected abstract string PrintFunctionName { get; }
        protected abstract string MethodPrefix { get; }
        protected abstract string AssignmentOperator { get; }
        protected abstract string MutableDeclaration { get; }
        protected abstract string ImmutableDeclaration { get; }
        protected abstract bool ML { get; }
        protected virtual string IndentSpaces => "    ";
        protected virtual string FunctionWrapStart => "{";
        protected virtual string FunctionWrapEnd => "}";

        protected virtual string GetBinaryOperator(BinaryOperator op)
        {
            switch (op)
            {
                case BinaryOperator.BitAnd: return "&";
                case BinaryOperator.Minus: return "-";
                case BinaryOperator.Multiply: return "*";
                case BinaryOperator.BitOr: return "|";
                case BinaryOperator.Plus: return "+";
                case BinaryOperator.Xor: return "^";
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
                    return $"{functionCall.FunctionName}{(ML ? " " : "")}()";
                case BinaryOperation binOp:
                    var left = GetExpression(binOp.LeftOperand);
                    var right = GetExpression(binOp.RightOperand);
                    return $"({left} {GetBinaryOperator(binOp.Operator)} {right})";
                default: throw new ArgumentOutOfRangeException(nameof(expr));
            }
        }

        protected virtual string GetStatement(IStatement statement, HashSet<Variable> assignedTo)
        {
            switch (statement)
            {
                case Assignment assignment:
                    var aname = assignment.Variable.VariableName;
                    var aexpr = GetExpression(assignment.AssignedExpression);
                    return $"{aname} {AssignmentOperator} {aexpr}";
                case VariableDeclaration variableDeclaration:
                    var vname = variableDeclaration.Variable.VariableName;
                    var vexpr = GetExpression(variableDeclaration.Initializer);
                    return (assignedTo.Contains(variableDeclaration.Variable))
                        ? $"{MutableDeclaration} {vname}: {IntType} = {vexpr}"
                        : $"{ImmutableDeclaration} {vname}: {IntType} = {vexpr}";
                case Return ret:
                    return GetExpression(ret.Expr);
                case Print print:
                    var pvname = print.Variable.VariableName;
                    return ML
                        ? $"{PrintFunctionName} {pvname}"
                        : $"{PrintFunctionName}({pvname})";
                default: throw new ArgumentOutOfRangeException(nameof(statement));
            }
        }

        protected virtual IEnumerable<string> GetFunctionDeclarationLines(FunctionDeclaration fun)
        {
            var assignedTo = new HashSet<Variable>(
                fun.Statements.OfType<Assignment>().Select(a => a.Variable));

            var isMain = fun is MainFunctionDeclaration;
            if (isMain)
                yield return $"{MethodPrefix} {Main} {FunctionWrapStart}";
            else
                yield return $"{MethodPrefix} {fun.FunctionName}(): {IntType} {FunctionWrapStart}";
            foreach (var statement in fun.Statements)
            {
                if (isMain && statement is Return)
                    continue;

                yield return $"{IndentSpaces}{GetStatement(statement, assignedTo)}";
            }
            yield return FunctionWrapEnd;
        }

        public abstract IEnumerable<string> GetProgramLines(Program program);
    }

    class ScalaLang : BaseExpressionLang
    {
        public override string Extension => "scala";
        protected override string IntType => "Int";
        protected override string Main => "main(): Unit";
        protected override string PrintFunctionName => "println";
        protected override string MethodPrefix => "def";
        protected override string AssignmentOperator => "=";
        protected override string MutableDeclaration => "var";
        protected override string ImmutableDeclaration => "val";
        protected override bool ML => false;
        protected override string IndentSpaces => "  ";
        protected override string FunctionWrapStart => "= {";

        public override IEnumerable<string> GetProgramLines(Program program)
        {
            yield return "object GeneratedFunctions {";
            foreach (var fun in program.Functions)
            {
                foreach (var line in GetFunctionDeclarationLines(fun))
                    yield return $"{IndentSpaces}{line}";
                yield return "";
            }

            foreach (var line in GetFunctionDeclarationLines(program.Main))
                yield return $"{IndentSpaces}{line}";

            yield return "}";
        }
    }

    class KotlinLang : BaseExpressionLang
    {
        public override string Extension => "kt";
        protected override string IntType => "Int";
        protected override string Main => "main()";
        protected override string PrintFunctionName => "println";
        protected override string MethodPrefix => "fun";
        protected override string AssignmentOperator => "=";
        protected override string MutableDeclaration => "var";
        protected override string ImmutableDeclaration => "val";
        protected override bool ML => false;

        protected override string GetBinaryOperator(BinaryOperator op)
        {
            switch (op)
            {
                case BinaryOperator.BitAnd: return "and";
                case BinaryOperator.Minus: return "-";
                case BinaryOperator.Multiply: return "*";
                case BinaryOperator.BitOr: return "or";
                case BinaryOperator.Plus: return "+";
                case BinaryOperator.Xor: return "xor";
                default: throw new ArgumentOutOfRangeException(nameof(op));
            }
        }

        protected override string GetStatement(IStatement statement, HashSet<Variable> assignedTo)
        {
            return (statement is Return ret)
                ? $"return {GetExpression(ret.Expr)}"
                : base.GetStatement(statement, assignedTo);
        }

        public override IEnumerable<string> GetProgramLines(Program program)
        {
            yield return "class GeneratedFunctions {";
            foreach (var fun in program.Functions)
            {
                foreach (var line in GetFunctionDeclarationLines(fun))
                    yield return $"{IndentSpaces}{line}";
                yield return "";
            }

            foreach (var line in GetFunctionDeclarationLines(program.Main))
                yield return $"{IndentSpaces}{line}";

            yield return "}";
        }
    }

    class OCamlLang : BaseExpressionLang
    {
        public override string Extension => "ml";
        protected override string IntType => "int";
        protected override string Main => "main ()";
        protected override string PrintFunctionName => @"Printf.printf ""%i\n""";
        protected override string MethodPrefix => "let";
        protected override string AssignmentOperator => "=";
        // choosing to ignore OCaml ref; ref int is not int
        protected override string MutableDeclaration => "let";
        protected override string ImmutableDeclaration => "let";
        protected override bool ML => true;
        protected override string FunctionWrapStart => "=";
        protected override string FunctionWrapEnd => "";

        protected override string GetBinaryOperator(BinaryOperator op)
        {
            switch (op)
            {
                case BinaryOperator.BitAnd: return "land";
                case BinaryOperator.Minus: return "-";
                case BinaryOperator.Multiply: return "*";
                case BinaryOperator.BitOr: return "lor";
                case BinaryOperator.Plus: return "+";
                case BinaryOperator.Xor: return "lxor";
                default: throw new ArgumentOutOfRangeException(nameof(op));
            }
        }

        protected override string GetStatement(IStatement statement, HashSet<Variable> assignedTo)
        {
            // handle "let <expr> in"
            if (statement is Print print)
                return $"{PrintFunctionName} {GetExpression(print.Variable)};;";
            else if (statement is Return ret)
                return GetExpression(ret.Expr);
            else if (statement is Assignment assignment)
            {
                var aname = assignment.Variable.VariableName;
                var aexpr = GetExpression(assignment.AssignedExpression);
                return $"let {aname} {AssignmentOperator} {aexpr} in";
            }
            else
                return base.GetStatement(statement, assignedTo) + " in";
        }

        public override IEnumerable<string> GetProgramLines(Program program)
        {
            foreach (var fun in program.Functions)
            {
                foreach (var line in GetFunctionDeclarationLines(fun))
                    yield return line;
                yield return "";
            }

            foreach (var line in GetFunctionDeclarationLines(program.Main))
                yield return line;
            yield return "";
            yield return "main ()";
        }
    }

    class FSharpLang : BaseExpressionLang
    {
        public override string Extension => "fs";
        protected override string IntType => "int";
        protected override string Main => "main args";
        protected override string PrintFunctionName => @"printfn ""%i\n""";
        protected override string MethodPrefix => "let";
        protected override string AssignmentOperator => "<-";
        protected override string MutableDeclaration => "let mutable ";
        protected override string ImmutableDeclaration => "let";
        protected override bool ML => true;
        protected override string FunctionWrapStart => "=";
        protected override string FunctionWrapEnd => "";

        protected override string GetBinaryOperator(BinaryOperator op)
        {
            switch (op)
            {
                case BinaryOperator.BitAnd: return "&&&";
                case BinaryOperator.Minus: return "-";
                case BinaryOperator.Multiply: return "*";
                case BinaryOperator.BitOr: return "|||";
                case BinaryOperator.Plus: return "+";
                case BinaryOperator.Xor: return "^^^";
                default: throw new ArgumentOutOfRangeException(nameof(op));
            }
        }

        public override IEnumerable<string> GetProgramLines(Program program)
        {
            foreach (var fun in program.Functions)
            {
                foreach (var line in GetFunctionDeclarationLines(fun))
                    yield return line;
                yield return "";
            }

            yield return "[<EntryPoint>]";
            foreach (var line in GetFunctionDeclarationLines(program.Main))
                yield return line;
            yield return $"{IndentSpaces}0";
        }
    }
    // round 3: haskell, rust

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
                case "ocaml": return new OCamlLang();
                case "fsharp": return new FSharpLang();
                case "csharp": return new CSharpLang();
                // case "haskell": return new HaskellLang();
                case "kotlin": return new KotlinLang();
                case "java": return new JavaLang();
                case "scala": return new ScalaLang();
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