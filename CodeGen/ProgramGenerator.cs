using System;
using System.Collections.Generic;
using System.Linq;

namespace CompilerBenchmarker.CodeGen
{
    static class ProgramGenerator
    {
        public static bool TrueOrFalse(this Random r) => r.Next(1, 3) == 1;
        public static T From<T>(this Random r, IReadOnlyCollection<T> collection) =>
            collection.ElementAt(r.Next(0, collection.Count));

        public static BinaryOperator Operator(this Random random)
        {
            switch (random.Next(1, 7))
            {
                case 1: return BinaryOperator.BitAnd;
                case 2: return BinaryOperator.Minus;
                case 3: return BinaryOperator.Multiply;
                case 4: return BinaryOperator.BitOr;
                case 5: return BinaryOperator.Plus;
                case 6: return BinaryOperator.Xor;
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
                    return new Literal(random.Next(0, 100));
                case 2:
                    return declaredVariables.Any()
                        ? random.From(declaredVariables)
                        : (IExpr)new Literal(random.Next(0, 100));
                case 3:
                    if (declaredFunctions.Any())
                    {
                        return declaredVariables.Any()
                            ? new FunctionCall(
                                random.From(declaredFunctions).FunctionName,
                                random.From(declaredVariables))
                            : new FunctionCall(
                                random.From(declaredFunctions).FunctionName,
                                new Literal(random.Next(0, 100)));
                    }
                    else
                    {
                        return new Literal(random.Next(0, 100));
                    }
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

            // prevent assigning to function parameter
            while (to.IsFunctionParameter)
            {
                to = random.From(declaredVariables);
            }

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

            // the main print prevents unused variable warnings
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

            // function return prevents unused variable warnings
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

            // Add variable for argument
            if (!isMain)
                decVars.Add(new Variable("p", true));

            // Variable declaration to start off every function
            // If possible, used the last function declaration to ensure all are used
            var firstDecVar = decFuns.Any()
                ? new VariableDeclaration(
                    new Variable($"x{decVars.Count}"),
                    new FunctionCall(
                        decFuns.Last().FunctionName,
                        new Literal(random.Next(0, 100))))
                : random.VariableDeclaration(decVars, decFuns);

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

        public static Program RandomProgram(Random random, int functionCount, int maxStatementsPerFunction)
        {
            var functions = new List<FunctionDeclaration>();
            for (var _ = 0; _ < functionCount; _ += 1)
                functions.Add(random.FunctionDeclaration(functions, maxStatementsPerFunction));

            var main = random.MainFunction(functions, maxStatementsPerFunction);
            return new Program(main, functions);
        }
    }
}