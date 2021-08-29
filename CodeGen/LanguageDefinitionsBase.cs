using System.Collections.Generic;

namespace CompilerBenchmarker.CodeGen
{
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
        public bool IsFunctionParameter { get; }
        public Variable(string name, bool isFunctionParameter = false)
        {
            VariableName = name;
            IsFunctionParameter = isFunctionParameter;
        }
        public override int GetHashCode() => VariableName.GetHashCode();
        public override bool Equals(object obj) =>
            obj is Variable v && v.VariableName == VariableName;
    }

    // e.g. 'myFunction(x0)'
    class FunctionCall : IExpr
    {
        public string FunctionName { get; }
        public IExpr Argument { get; }
        public FunctionCall(string name, IExpr argument)
        {
            FunctionName = name;
            Argument = argument;
        }
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

    // e.g. 'int f0(int x0) { ... }'
    class FunctionDeclaration : IFunctionDeclaration
    {
        public const string Parameter = "p";
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

    // e.g. 'int main(void) { ... }'
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
}