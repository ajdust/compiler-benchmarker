using System;
using System.IO;
using System.Text;
using System.Linq;
using System.Collections.Generic;

namespace CompilerBenchmarker
{
    public static class RandomExtensions
    {
    	public static T Choice<T>(this Random r, IList<T> of) => of[r.Next(0, of.Count)];
    }

    public interface IHasUsed
    {
        bool Used {get;set;}
        string Name {get;set;}
    }

    abstract class Expr
    {
        public virtual string Name {get;set;}
    }

    class ConstExpr : Expr
    {
    	public string Val {get;set;}
    	public ConstExpr(string val)
    	{
            if (val == null)
                throw new ArgumentNullException(nameof(val));

            this.Val = val;
        }
    }

    class VarExpr : Expr
    {
    	public VarExpr(string name)
    	{
            if (name == null)
                throw new ArgumentNullException(nameof(name));

            this.Name = name;
        }
    }

    class BinOp : Expr
    {
    	public string Op {get;set;}
    	public Expr Left {get;set;}
    	public Expr Right {get;set;}
    	public BinOp(string op, Expr left, Expr right)
    	{
            if (op == null)
                throw new ArgumentNullException(nameof(op));
            if (left == null)
                throw new ArgumentNullException(nameof(left));
            if (right == null)
                throw new ArgumentNullException(nameof(right));


            this.Op = op;
            this.Left = left;
            this.Right = right;
        }
    }

    class FunCallExpr : Expr
    {
    	public override string Name {get;set;}
    	public FunCallExpr(string name)
    	{
            if (name == null)
                throw new ArgumentNullException(nameof(name));

            this.Name = name;
        }
    }

    abstract class Statement
    {
        public Expr Expr {get;set;}
    }

    class Assignment : Statement
    {
    	public string Lval {get;set;}
    	public Assignment(string lval, Expr expr)
    	{
            if (expr == null)
                throw new ArgumentNullException(nameof(expr));
            if (lval == null)
                throw new ArgumentNullException(nameof(lval));

            this.Lval = lval;
            this.Expr = expr;
        }
    }

    class VarDecl : Statement, IHasUsed
    {
    	public string Name {get;set;}
    	public bool Mut {get;set;}
    	public bool Used {get;set;}
    	public VarDecl(string name, Expr expr)
    	{
            if (expr == null)
                throw new ArgumentNullException(nameof(expr));
            if (name == null)
                throw new ArgumentNullException(nameof(name));

            this.Name = name;
            this.Expr = expr;
            this.Mut = false;
            this.Used = false;
        }
    }

    class Return : Statement
    {
    	public Return(Expr expr)
    	{
            if (expr == null)
                throw new ArgumentNullException(nameof(expr));

            this.Expr = expr;
        }
    }

    class Print : Statement
    {
    	public Print(Expr expr)
    	{
            if (expr == null)
                throw new ArgumentNullException(nameof(expr));

            this.Expr = expr;
        }
    }

    class FunDecl : IHasUsed
    {
    	public string Name {get;set;}
    	public IList<Statement> Statements {get;set;}
    	public string ReturnType {get;set;}
        public bool Used {get;set;}
    	public FunDecl(string name, IList<Statement> statements, string returnType)
    	{
            if (name == null)
                throw new ArgumentNullException(nameof(name));
            if (statements == null)
                throw new ArgumentNullException(nameof(statements));
            if (returnType == null)
                throw new ArgumentNullException(nameof(returnType));

            this.Name = name;
            this.Statements = statements;
            this.ReturnType = returnType;
            this.Used = false;
        }
    }

    class Program
    {
    	public FunDecl Main {get;set;}
    	public IList<FunDecl> Functions {get;set;}
    	public Program(FunDecl main, IList<FunDecl> functions)
    	{
            if (main == null)
                throw new ArgumentNullException(nameof(main));
            if (functions == null)
                throw new ArgumentNullException(nameof(functions));

            this.Main = main;
            this.Functions = functions;
        }
    }

    class Context
    {
    	Dictionary<string, IHasUsed> Env {get;set;} = new Dictionary<string, IHasUsed>();
    	int id {get;set;} = 0;
    	Context parent {get;set;}
        Random random {get;set;}

    	public Context(Context parent, Random random)
    	{
            this.parent = parent;
            this.random = random;
    	}

        string name(string prefix, int i)
        {
            return $"{prefix}{i}";
    	}

        string newName(string prefix)
        {
            id += 1;
            return name(prefix, id);
    	}

        string randomName(string prefix)
        {
            var biasedMin = random.Next(1, id);
            var i = random.Next(biasedMin, id);
            return name(prefix, i);
    	}

        Expr randomExpr()
        {
        	switch (random.Next(1, 5))
        	{
                case 1: return randomConstExpr();
                case 2: return randomVarExpr();
                case 3: return randomBinaryOp();
                case 4: return randomFunCall();
                default: throw new ArgumentOutOfRangeException();
            };
    	}

        IHasUsed findUnused()
        {
            foreach (var decl in Env)
            {
                if (!decl.Value.Used)
                    return decl.Value;
            }
            return null;
    	}

        Expr forceUseExpr()
        {
            Expr expr = randomConstExpr();
            var decl = findUnused();
            while (decl != null)
            {
                var left = forcedVarExpr(decl.Name);
                expr = forcedRandomBinaryOp(left, expr);
                decl = findUnused();
            }

            decl = parent.findUnused();
            while (decl != null)
            {
                var left = forcedFunCall(decl.Name);
                expr = forcedRandomBinaryOp(left, expr);
                decl = parent.findUnused();
            }
            return expr;
    	}

        ConstExpr randomConstExpr()
        {
            return new ConstExpr(random.Next(1, 1000).ToString());
    	}

        VarExpr forcedVarExpr(string name)
        {
            var decl = Env[name];
            decl.Used = true;
            return new VarExpr(name);
    	}

        Expr randomVarExpr()
        {
            if (id == 0)
                return randomConstExpr();
            var name = randomName("x");
            return forcedVarExpr(name);
    	}

        BinOp forcedRandomBinaryOp(Expr left, Expr right)
        {
            //op = random.choice(["+", "-", "*", "|", "&", "^"]);
            var op = random.Choice(new[] {"|", "&", "^"});
            return new BinOp(op, left, right);
    	}

        BinOp randomBinaryOp()
        {
            var left = randomExpr();
            var right = randomExpr();
            return forcedRandomBinaryOp(left, right);
    	}

        FunCallExpr forcedFunCall(string name)
        {
            var decl = parent.Env[name];
            decl.Used = true;
            return new FunCallExpr(name);
    	}

        Expr randomFunCall()
        {
            if (parent.id == 0)
                return randomConstExpr();
            var name = parent.randomName("f");
            return forcedFunCall(name);
    	}

        Statement randomStatement()
        {
        	switch (random.Next(1, 3))
        	{
                case 1: return randomAssignment();
                case 2: return randomVarDecl();
                default: throw new ArgumentOutOfRangeException();
        	}
    	}

        Assignment randomAssignment()
        {
            var name = randomName("x");
            var decl = Env[name];
            var expr = randomExpr();
            if (!decl.Used)
            {
                var left = forcedVarExpr(name);
                expr = forcedRandomBinaryOp(left, expr);
            }
            decl.Used = false;
            if (decl is VarDecl)
                (decl as VarDecl).Mut = true;
            return new Assignment(name, expr);
    	}

        Return randomReturnStatement()
        {
            return new Return(forceUseExpr());
    	}

        Print randomPrintStatement()
        {
            return new Print(forceUseExpr());
    	}

        VarDecl randomVarDecl()
        {
            var expr = randomExpr();
            var name = newName("x");
            var decl = new VarDecl(name, expr);
            Env[name] = decl;
            return decl;
    	}

        FunDecl randomFunDecl(int numStatements, string returnType)
        {
            var local = new Context(this, this.random);
            var statements = new List<Statement>();
            statements.Add(local.randomVarDecl());
            foreach (var i in Enumerable.Range(0, numStatements))
                statements.Add(local.randomStatement());
            if (returnType != "")
                statements.Add(local.randomReturnStatement());
            else
                statements.Add(local.randomPrintStatement());
            var name = newName("f");
            var decl = new FunDecl(name, statements, returnType);
            Env[name] = decl;
            return decl;
    	}

        public Program RandomProgram(int numFuns, int maxStatementsPerFun)
        {
            var functions = new List<FunDecl>();
            int numStatements;
            foreach (var i in Enumerable.Range(0, numFuns))
            {
                numStatements = random.Next(1, maxStatementsPerFun);
                var funDecl = randomFunDecl(numStatements, "int");
                functions.Add(funDecl);
            }
            numStatements = random.Next(1, maxStatementsPerFun);
            var main = randomFunDecl(numStatements, "");
            return new Program(main, functions);
        }
    }

    abstract class Lang
    {
        protected virtual string ext {get;}
    	protected int indent {get;set;} = 0;
    	protected int extraIndent {get;set;} = 0;

        protected virtual Dictionary<string, string> operators => new Dictionary<string, string> {
            {"&", "&"},
            {"|", "|"},
            {"^", "^"}
        };

        protected virtual void writeIndent(StreamWriter f)
        {
            var num = 4 * indent + extraIndent;
            string s = "";
            for (; num > 0; num--)
            {
                s += " ";
            }
            f.Write(s);
    	}

        abstract protected void writeFunDecl(StreamWriter f, FunDecl s, bool main = false);

        abstract protected void writeVarDecl(StreamWriter f, VarDecl s);

        abstract protected void writeAssignment(StreamWriter f, Assignment s);

        abstract protected void writeReturn(StreamWriter f, Return s);

        abstract protected void writePrint(StreamWriter f, Print s);

        abstract public void WriteProgram(StreamWriter f, Program s);

        protected virtual void writeStatement(StreamWriter f, Statement statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            if (statement is VarDecl)
                writeVarDecl(f, statement as VarDecl);
            else if (statement is Assignment)
                writeAssignment(f, statement as Assignment);
            else if (statement is Return)
                writeReturn(f, statement as Return);
            else if (statement is Print)
                writePrint(f, statement as Print);
            else
                throw new Exception("Unknown kind of statement");
    	}

        protected virtual void writeLval(StreamWriter f, string lval)
        {
            if (lval == null)
                throw new ArgumentNullException(nameof(lval));

            f.Write(lval);
    	}

        protected virtual void writeExpr(StreamWriter f, Expr expr, bool needsParens = false)
        {
            if (expr == null)
                throw new ArgumentNullException(nameof(expr));

            if (expr is ConstExpr)
                writeConstExpr(f, expr as ConstExpr, needsParens);
            else if (expr is VarExpr)
                writeVarExpr(f, expr as VarExpr, needsParens);
            else if (expr is BinOp)
                writeBinOp(f, expr as BinOp, true);
            else if (expr is FunCallExpr)
                writeFunCall(f, expr as FunCallExpr, needsParens);
            else
                throw new Exception("Unknown kind of expr");
    	}

        protected virtual void writeConstExpr(StreamWriter f, ConstExpr expr, bool needsParens)
        {
            if (expr == null)
                throw new ArgumentNullException(nameof(expr));

            f.Write(expr.Val);
    	}

        protected virtual void writeVarExpr(StreamWriter f, VarExpr expr, bool needsParens)
        {
            if (expr == null)
                throw new ArgumentNullException(nameof(expr));

            f.Write(expr.Name);
    	}

        protected virtual void writeBinOp(StreamWriter f, BinOp expr, bool needsParens)
        {
            if (expr == null)
                throw new ArgumentNullException(nameof(expr));

            if (needsParens)
                f.Write("(");
            writeExpr(f, expr.Left, needsParens);
            f.Write($" {operators[expr.Op]} ");
            writeExpr(f, expr.Right, needsParens);
            if (needsParens)
                f.Write(")");
    	}

        protected virtual void writeFunCall(StreamWriter f, FunCallExpr expr, bool needsParens)
        {
            if (expr == null)
                throw new ArgumentNullException(nameof(expr));

            f.Write($"{expr.Name}()");
        }
    }

    class CppLang : Lang
    {
        protected override string ext => "cpp";

        public Dictionary<string, string> typeNames => new Dictionary<string, string> {
            ["int"] = "int"
        };

        public override void WriteProgram(StreamWriter f, Program program)
        {
            if (program == null)
                throw new ArgumentNullException(nameof(program));

            f.Write("#include <cstdio>\n\n");
            foreach (var funDecl in program.Functions)
            {
                writeFunDecl(f, funDecl);
                f.Write("\n");
            }
            writeFunDecl(f, program.Main, true);
    	}

        protected override void writeFunDecl(StreamWriter f, FunDecl funDecl, bool main = false)
        {
            if (funDecl == null)
                throw new ArgumentNullException(nameof(funDecl));

            string optionalResult, typeName;
            if (funDecl.ReturnType == "")
            {
                optionalResult = "int ";
            }
            else
            {
                typeNames.TryGetValue(funDecl.ReturnType, out typeName);
                optionalResult = $"{typeName} ";
            }
            var funName = main ? "main" : funDecl.Name;
            f.Write($"{optionalResult} {funName}() {{\n");
            indent += 1;
            foreach (var statement in funDecl.Statements)
                writeStatement(f, statement);
            indent -= 1;
            f.Write("}\n");
    	}

        protected override void writeVarDecl(StreamWriter f, VarDecl varDecl)
        {
            if (varDecl == null)
                throw new ArgumentNullException(nameof(varDecl));

            writeIndent(f);
            f.Write("int ");
            writeLval(f, varDecl.Name);
            f.Write(" = ");
            writeExpr(f, varDecl.Expr);
            f.Write(";\n");
    	}

        protected override void writeAssignment(StreamWriter f, Assignment assignment)
        {
            if (assignment == null)
                throw new ArgumentNullException(nameof(assignment));

            writeIndent(f);
            writeLval(f, assignment.Lval);
            f.Write(" = ");
            writeExpr(f, assignment.Expr);
            f.Write(";\n");
    	}

        protected override void writeReturn(StreamWriter f, Return statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            f.Write("return ");
            writeExpr(f, statement.Expr);
            f.Write(";\n");
    	}

        protected override void writePrint(StreamWriter f, Print statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            f.Write("printf(\"%i\\n\", ");
            writeExpr(f, statement.Expr);
            f.Write(");\n");
        }
    }

    class CLang : CppLang
    {
        protected override string ext => "c";

        public override void WriteProgram(StreamWriter f, Program program)
        {
            if (program == null)
                throw new ArgumentNullException(nameof(program));

            f.Write("#include <stdio.h>\n\n");
            foreach (var funDecl in program.Functions)
            {
                writeFunDecl(f, funDecl);
                f.Write("\n");
            }
            writeFunDecl(f, program.Main, true);
        }
    }

    class DLang : Lang
    {
        protected override string ext => "d";

        public Dictionary<string, string> typeNames => new Dictionary<string, string> {
            ["int"] = "int",
        };

        public override void WriteProgram(StreamWriter f, Program program)
        {
            if (program == null)
                throw new ArgumentNullException(nameof(program));

            f.Write("import std.stdio;\n\n");
            foreach (var funDecl in program.Functions)
            {
                writeFunDecl(f, funDecl);
                f.Write("\n");
            }
            writeFunDecl(f, program.Main, true);
    	}

        protected override void writeFunDecl(StreamWriter f, FunDecl funDecl, bool main = false)
        {
            if (funDecl == null)
                throw new ArgumentNullException(nameof(funDecl));

            string optionalResult, typeName;
            if (funDecl.ReturnType == "")
            {
                optionalResult = "void ";
            }
            else
            {
                typeNames.TryGetValue(funDecl.ReturnType, out typeName);
                optionalResult = $"{typeName} ";
            }
            var funName = main ? "main" : funDecl.Name;
            f.Write($"{optionalResult} {funName}() {{\n");
            indent += 1;
            foreach (var statement in funDecl.Statements)
                writeStatement(f, statement);
            indent -= 1;
            f.Write("}\n");
    	}

        protected override void writeVarDecl(StreamWriter f, VarDecl varDecl)
        {
            if (varDecl == null)
                throw new ArgumentNullException(nameof(varDecl));

            writeIndent(f);
            f.Write("int ");
            writeLval(f, varDecl.Name);
            f.Write(" = ");
            writeExpr(f, varDecl.Expr);
            f.Write(";\n");
    	}

        protected override void writeAssignment(StreamWriter f, Assignment assignment)
        {
            if (assignment == null)
                throw new ArgumentNullException(nameof(assignment));

            writeIndent(f);
            writeLval(f, assignment.Lval);
            f.Write(" = ");
            writeExpr(f, assignment.Expr);
            f.Write(";\n");
    	}

        protected override void writeReturn(StreamWriter f, Return statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            f.Write("return ");
            writeExpr(f, statement.Expr);
            f.Write(";\n");
    	}

        protected override void writePrint(StreamWriter f, Print statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            f.Write("writefln(\"%d\", ");
            writeExpr(f, statement.Expr);
            f.Write(");\n");
        }
    }

    class GoLang : Lang
    {
        protected override string ext => "go";

        public Dictionary<string, string> typeNames => new Dictionary<string, string> {
            ["int"] = "int",
        };

        public override void WriteProgram(StreamWriter f, Program program)
        {
            if (program == null)
                throw new ArgumentNullException(nameof(program));

            f.Write("package main\n\n");
            f.Write("import \"fmt\"\n\n");
            foreach (var funDecl in program.Functions)
            {
                writeFunDecl(f, funDecl);
                f.Write("\n");
            }
            writeFunDecl(f, program.Main, true);
    	}

        protected override void writeFunDecl(StreamWriter f, FunDecl funDecl, bool main = false)
        {
            if (funDecl == null)
                throw new ArgumentNullException(nameof(funDecl));

            string optionalResult, typeName;
            if (funDecl.ReturnType == "")
            {
                optionalResult = "";
            }
            else
            {
                typeNames.TryGetValue(funDecl.ReturnType, out typeName);
                optionalResult = " " + typeName;
            }
            var funName = main ? "main" : funDecl.Name;
            f.Write($"func {funName}(){optionalResult} {{\n");
            indent += 1;
            foreach (var statement in funDecl.Statements)
                writeStatement(f, statement);
            indent -= 1;
            f.Write("}\n");
    	}

        protected override void writeVarDecl(StreamWriter f, VarDecl varDecl)
        {
            if (varDecl == null)
                throw new ArgumentNullException(nameof(varDecl));

            writeIndent(f);
            writeLval(f, varDecl.Name);
            f.Write(" := ");
            writeExpr(f, varDecl.Expr);
            f.Write("\n");
    	}

        protected override void writeAssignment(StreamWriter f, Assignment assignment)
        {
            if (assignment == null)
                throw new ArgumentNullException(nameof(assignment));

            writeIndent(f);
            writeLval(f, assignment.Lval);
            f.Write(" = ");
            writeExpr(f, assignment.Expr);
            f.Write("\n");
    	}

        protected override void writeReturn(StreamWriter f, Return statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            f.Write("return ");
            writeExpr(f, statement.Expr);
            f.Write("\n");
    	}

        protected override void writePrint(StreamWriter f, Print statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            f.Write("fmt.Printf(\"%d\\n\", ");
            writeExpr(f, statement.Expr);
            f.Write(")\n");
        }
    }

    class PascalLang : Lang
    {
        protected override string ext => "pas";

        public Dictionary<string, string> typeNames => new Dictionary<string, string> {
            ["int"] = "integer",
        };

        protected override Dictionary<string, string> operators => new Dictionary<string, string> {
            ["&"] = "and",
            ["|"] = "or",
            ["^"] = "xor",
        };

        public override void WriteProgram(StreamWriter f, Program program)
        {
            if (program == null)
                throw new ArgumentNullException(nameof(program));

            f.Write("program main;\n\n");
            foreach (var funDecl in program.Functions)
            {
                writeFunDecl(f, funDecl);
                f.Write("\n");
            }
            writeFunDecl(f, program.Main, true);
    	}

        protected override void writeFunDecl(StreamWriter f, FunDecl funDecl, bool main = false)
        {
            if (funDecl == null)
                throw new ArgumentNullException(nameof(funDecl));

            string funName;
            string typeName;
            if (!main)
            {
                funName = funDecl.Name;
                typeNames.TryGetValue(funDecl.ReturnType, out typeName);
                f.Write($"function {funName}() : {typeName};\n");
            }
            var vars = funDecl.Statements.OfType<VarDecl>();
            if (vars != null && vars.Any())
            {
                f.Write("var\n");
                foreach (var v in vars)
                {
                    typeName = typeNames["int"];
                    f.Write($"  {v.Name} : {typeName};\n");
                }
            }
            f.Write("begin\n");
            indent += 1;
            foreach (var statement in funDecl.Statements)
                writeStatement(f, statement);
            indent -= 1;
            f.Write("end{(main ? '.' : ';')}\n");
    	}

        protected override void writeVarDecl(StreamWriter f, VarDecl varDecl)
        {
            if (varDecl == null)
                throw new ArgumentNullException(nameof(varDecl));

            writeIndent(f);
            writeLval(f, varDecl.Name);
            f.Write(" := ");
            writeExpr(f, varDecl.Expr);
            f.Write(";\n");
    	}

        protected override void writeAssignment(StreamWriter f, Assignment assignment)
        {
            if (assignment == null)
                throw new ArgumentNullException(nameof(assignment));

            writeIndent(f);
            writeLval(f, assignment.Lval);
            f.Write(" := ");
            writeExpr(f, assignment.Expr);
            f.Write(";\n");
    	}

        protected override void writeReturn(StreamWriter f, Return statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            writeLval(f, statement.Expr.Name);
            f.Write(" := ");
            writeExpr(f, statement.Expr);
            f.Write(";\n");
    	}

        protected override void writePrint(StreamWriter f, Print statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            f.Write("writeln(");
            writeExpr(f, statement.Expr);
            f.Write(");\n");
        }
    }

    class RustLang : Lang
    {
        protected override string ext => "rs";

        public Dictionary<string, string> typeNames => new Dictionary<string, string> {
            ["int"] = "i32",
        };

        public override void WriteProgram(StreamWriter f, Program program)
        {
            if (program == null)
                throw new ArgumentNullException(nameof(program));

            f.Write("#![allow(unused_parens)]\n\n");
            foreach (var funDecl in program.Functions)
            {
                writeFunDecl(f, funDecl);
                f.Write("\n");
            }
            writeFunDecl(f, program.Main, true);
    	}

        protected override void writeFunDecl(StreamWriter f, FunDecl funDecl, bool main = false)
        {
            if (funDecl == null)
                throw new ArgumentNullException(nameof(funDecl));

            string optionalResult;
            string funName, typeName;
            if (funDecl.ReturnType == "")
            {
                optionalResult = "";
            }
            else
            {
                typeNames.TryGetValue(funDecl.ReturnType, out typeName);
                optionalResult = $" -> {typeName}";
            }
            funName = main ? "main" : funDecl.Name;
            f.Write($"fn {funName}(){optionalResult} {{\n");
            indent += 1;
            foreach (var statement in funDecl.Statements)
                writeStatement(f, statement);
            indent -= 1;
            f.Write("}\n");
    	}

        protected override void writeVarDecl(StreamWriter f, VarDecl varDecl)
        {
            if (varDecl == null)
                throw new ArgumentNullException(nameof(varDecl));

            writeIndent(f);
            f.Write("let ");
            if (varDecl.Mut)
                f.Write("mut ");
            writeLval(f, varDecl.Name);
            f.Write(": i32");
            f.Write(" = ");
            writeExpr(f, varDecl.Expr);
            f.Write(";\n");
    	}

        protected override void writeAssignment(StreamWriter f, Assignment assignment)
        {
            if (assignment == null)
                throw new ArgumentNullException(nameof(assignment));

            writeIndent(f);
            writeLval(f, assignment.Lval);
            f.Write(" = ");
            writeExpr(f, assignment.Expr);
            f.Write(";\n");
    	}

        protected override void writeReturn(StreamWriter f, Return statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            writeExpr(f, statement.Expr);
            f.Write("\n");
    	}

        protected override void writePrint(StreamWriter f, Print statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            f.Write("println!(\"{}\", ");
            writeExpr(f, statement.Expr);
            f.Write(")\n");
        }
    }

    class OCamlLang : Lang
    {
        protected override string ext => "ml";

        public Dictionary<string, string> typeNames => new Dictionary<string, string> {
            ["int"] = "int"
        };

        protected override Dictionary<string, string> operators => new Dictionary<string, string> {
            ["&"] = "land",
            ["|"] = "lor",
            ["^"] = "lxor",
        };

        public override void WriteProgram(StreamWriter f, Program program)
        {
            if (program == null)
                throw new ArgumentNullException(nameof(program));

            foreach (var funDecl in program.Functions)
            {
                writeFunDecl(f, funDecl);
                f.Write("\n");
            }
            writeFunDecl(f, program.Main, true);
    	}

        protected override void writeFunDecl(StreamWriter f, FunDecl funDecl, bool main = false)
        {
            if (funDecl == null)
                throw new ArgumentNullException(nameof(funDecl));

            string optionalResult, typeName;
            if (funDecl.ReturnType == "")
            {
                optionalResult = "";
            }
            else
            {
                typeNames.TryGetValue(funDecl.ReturnType, out typeName);
                optionalResult = $":{typeName} ";
            }
            var funName = main ? "main" : funDecl.Name;
            f.Write($"let {funName} (){optionalResult} = \n");
            indent += 1;
            foreach (var statement in funDecl.Statements)
                writeStatement(f, statement);
            indent -= 1;
            if (main)
                f.Write("\nmain ()");
    	}

        protected override void writeVarDecl(StreamWriter f, VarDecl varDecl)
        {
            if (varDecl == null)
                throw new ArgumentNullException(nameof(varDecl));

            writeIndent(f);
            f.Write("let ");
            writeLval(f, varDecl.Name);
            f.Write(" = ");
            writeExpr(f, varDecl.Expr);
            f.Write(" in \n");
    	}

        protected override void writeAssignment(StreamWriter f, Assignment assignment)
        {
            if (assignment == null)
                throw new ArgumentNullException(nameof(assignment));

            writeIndent(f);
            f.Write("let ");
            writeLval(f, assignment.Lval);
            f.Write(" = ");
            writeExpr(f, assignment.Expr);
            f.Write(" in \n");
    	}

        protected override void writeReturn(StreamWriter f, Return statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            writeExpr(f, statement.Expr);
            f.Write("\n");
    	}

        protected override void writePrint(StreamWriter f, Print statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            f.Write("Printf.printf \"%i\\n\" (");
            writeExpr(f, statement.Expr);
            f.Write(");;\n");
        }
    }

    class FSharpLang : Lang
    {
        protected override string ext => "fs";

        public Dictionary<string, string> typeNames => new Dictionary<string, string> {
            ["int"] = "int"
        };

        protected override Dictionary<string, string> operators => new Dictionary<string, string> {
            ["&"] = "&&&",
            ["|"] = "|||",
            ["^"] = "^^^",
    	};

        public override void WriteProgram(StreamWriter f, Program program)
        {
            if (program == null)
                throw new ArgumentNullException(nameof(program));

            foreach (var funDecl in program.Functions)
            {
                writeFunDecl(f, funDecl);
                f.Write("\n");
            }
            writeFunDecl(f, program.Main, true);
    	}

        protected override void writeFunDecl(StreamWriter f, FunDecl funDecl, bool main = false)
        {
            if (funDecl == null)
                throw new ArgumentNullException(nameof(funDecl));

            string optionalResult, typeName;
            if (funDecl.ReturnType == "")
            {
                optionalResult = "";
            }
            else
            {
                typeNames.TryGetValue(funDecl.ReturnType, out typeName);
                optionalResult = ":" + typeName + " ";
            }
            var funName = main ? "main" : funDecl.Name;
            f.Write($"let {funName} (){optionalResult} = \n");
            indent += 1;
            foreach (var statement in funDecl.Statements)
                writeStatement(f, statement);
            indent -= 1;
            if (main)
                f.Write("\nmain ()");
    	}

        protected override void writeVarDecl(StreamWriter f, VarDecl varDecl)
        {
            if (varDecl == null)
                throw new ArgumentNullException(nameof(varDecl));

            writeIndent(f);
            f.Write("let ");
            writeLval(f, varDecl.Name);
            f.Write(" = ");
            writeExpr(f, varDecl.Expr);
            f.Write(" in \n");
    	}

        protected override void writeAssignment(StreamWriter f, Assignment assignment)
        {
            if (assignment == null)
                throw new ArgumentNullException(nameof(assignment));

            writeIndent(f);
            f.Write("let ");
            writeLval(f, assignment.Lval);
            f.Write(" = ");
            writeExpr(f, assignment.Expr);
            f.Write(" in \n");
    	}

        protected override void writeReturn(StreamWriter f, Return statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            writeExpr(f, statement.Expr);
            f.Write("\n");
    	}

        protected override void writePrint(StreamWriter f, Print statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            f.Write("printfn \"%i\\n\" (");
            writeExpr(f, statement.Expr);
            f.Write(");;\n");
        }

    }

    class HaskellLang : Lang
    {
        protected override string ext => "hs";

        public Dictionary<string, string> typeNames => new Dictionary<string, string> {
            ["int"] = "Int64"
        };

        protected override Dictionary<string, string> operators => new Dictionary<string, string> {
            ["&"] = "&&&",
            ["|"] = "|||",
            ["^"] = "^^^",
    	};

        public override void WriteProgram(StreamWriter f, Program program)
        {
            if (program == null)
                throw new ArgumentNullException(nameof(program));

            f.Write("import GHC.Int\n"
                    + "import Data.Bits\n"
                    + "import Text.Printf\n\n");
            // removes ambiguity and forces 32-bit integers
            f.Write(  "(&&&) :: Int32 -> Int32 -> Int32\n"
                    + "a &&& b = a .&. b\n"
                    + "(|||) :: Int32 -> Int32 -> Int32\n"
                    + "a ||| b = a .|. b\n"
                    + "(^^^) :: Int32 -> Int32 -> Int32\n"
                    + "a ^^^ b = a `xor` b\n\n");
            foreach (var funDecl in program.Functions)
            {
                writeFunDecl(f, funDecl);
                f.Write("\n");
            }
            writeFunDecl(f, program.Main, true);
    	}

        protected override void writeFunDecl(StreamWriter f, FunDecl funDecl, bool main = false)
        {
            if (funDecl == null)
                throw new ArgumentNullException(nameof(funDecl));

            string fund;
            if (main)
            {
                fund = "main = ";
                f.Write("main :: IO ()\n");
                f.Write(fund);
            }
            else
            {
                fund = $"{funDecl.Name} = ";
                f.Write(funDecl.Name + " :: Int32\n");
                f.Write(fund);
            }

            if (funDecl.Statements.Count > 0)
                writeStatement(f, funDecl.Statements[0]);
            extraIndent = fund.Length;
            foreach (var statement in funDecl.Statements.Skip(1))
                writeStatement(f, statement);
            extraIndent = 0;
    	}

        protected override void writeVarDecl(StreamWriter f, VarDecl varDecl)
        {
            if (varDecl == null)
                throw new ArgumentNullException(nameof(varDecl));

            writeIndent(f);
            f.Write("let ");
            writeLval(f, varDecl.Name);
            f.Write(" = ");
            writeExpr(f, varDecl.Expr);
            f.Write(" in \n");
    	}

        protected override void writeAssignment(StreamWriter f, Assignment assignment)
        {
            if (assignment == null)
                throw new ArgumentNullException(nameof(assignment));

            if (assignment.Expr is VarExpr && assignment.Lval == assignment.Expr.Name)
                return;

            writeIndent(f);
            f.Write("let ");
            writeLval(f, assignment.Lval);
            f.Write(" = ");
            writeExpr(f, assignment.Expr);
            f.Write(" in \n");
    	}

        protected override void writeReturn(StreamWriter f, Return statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            writeExpr(f, statement.Expr);
            f.Write("\n");
    	}

        protected override void writePrint(StreamWriter f, Print statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            f.Write("printf \"%i\\n\" (");
            writeExpr(f, statement.Expr);
            f.Write(")\n");
    	}

        protected override void writeFunCall(StreamWriter f, FunCallExpr expr, bool needsParens)
        {
            f.Write(expr.Name);
        }
    }

    class CSharpLang : CppLang
    {
        protected override string ext => "cs";

        public override void WriteProgram(StreamWriter f, Program program)
        {
            if (program == null)
                throw new ArgumentNullException(nameof(program));

            f.Write("using System;\nnamespace CompilationSpeedTest\n{\n");
            indent += 1;
            writeIndent(f);
            f.Write("static class Program\n");
    	    writeIndent(f);
            f.Write("{\n");
            indent += 1;
            foreach (var funDecl in program.Functions)
            {
                writeFunDecl(f, funDecl);
                f.Write("\n");
            }
            writeFunDecl(f, program.Main, true);
            f.Write("\n");
            indent -= 1;
            writeIndent(f);
            f.Write("}\n");
            indent -= 1;
            f.Write("}\n");
    	}

        protected override void writeFunDecl(StreamWriter f, FunDecl funDecl, bool main = false)
        {
            if (funDecl == null)
                throw new ArgumentNullException(nameof(funDecl));

            string optionalResult, typeName;
            if (funDecl.ReturnType == "")
            {
                optionalResult = "static int";
            }
            else
            {
                typeNames.TryGetValue(funDecl.ReturnType, out typeName);
                optionalResult = "static " + typeName;
            }
            var funName = main ? "Main" : funDecl.Name;
            writeIndent(f);
            f.Write($"{optionalResult} {funName}()\n");
            writeIndent(f);
            f.Write("{\n");
            indent += 1;
            foreach (var statement in funDecl.Statements)
                writeStatement(f, statement);
            if (main)
            {
                writeIndent(f);
                f.Write("return 0;\n");
            }
            indent -= 1;
            writeIndent(f);
            f.Write("}\n");
    	}

        protected override void writePrint(StreamWriter f, Print statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            f.Write("Console.WriteLine(\"{i}\\n\", ");
            writeExpr(f, statement.Expr);
            f.Write(");\n");
        }
    }

    class JavaLang : CSharpLang
    {
        protected override string ext => "java";

        public override void WriteProgram(StreamWriter f, Program program)
        {
            if (program == null)
                throw new ArgumentNullException(nameof(program));

            f.Write("package CompilationSpeedTest;\n\n");
            writeIndent(f);
            f.Write("class Program\n");
            writeIndent(f);
            f.Write("{\n");
            indent += 1;
            foreach (var funDecl in program.Functions)
            {
                writeFunDecl(f, funDecl);
                f.Write("\n");
            }
            writeFunDecl(f, program.Main, true);
            f.Write("\n");
            indent -= 1;
            writeIndent(f);
            f.Write("}\n");
        }

        protected override void writePrint(StreamWriter f, Print statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            f.Write("System.out.format(\"%d\\n\", ");
            writeExpr(f, statement.Expr);
            f.Write(");\n");
        }
    }

    class KotlinLang : Lang
    {
        protected override string ext => "kt";

        public Dictionary<string, string> typeNames => new Dictionary<string, string> {
            ["int"] = "Int",
        };

        protected override Dictionary<string, string> operators => new Dictionary<string, string> {
            ["&"] = "and",
            ["|"] = "or",
            ["^"] = "xor"
        };

        public override void WriteProgram(StreamWriter f, Program program)
        {
            if (program == null)
                throw new ArgumentNullException(nameof(program));

            foreach (var funDecl in program.Functions)
            {
                writeFunDecl(f, funDecl);
                f.Write("\n");
            }
            writeFunDecl(f, program.Main, true);
    	}

        protected override void writeFunDecl(StreamWriter f, FunDecl funDecl, bool main = false)
        {
            if (funDecl == null)
                throw new ArgumentNullException(nameof(funDecl));

            string optionalResult, typeName;
            if (funDecl.ReturnType == "")
            {
                optionalResult = "";
            }
            else
            {
                typeNames.TryGetValue(funDecl.ReturnType, out typeName);
                optionalResult = ": " + typeName;
            }
            var funName = main ? "main" : funDecl.Name;
            f.Write($"fun {funName}(){optionalResult} {{\n");
            indent += 1;
            foreach (var statement in funDecl.Statements)
                writeStatement(f, statement);
            indent -= 1;
            f.Write("}\n");
    	}

        protected override void writeVarDecl(StreamWriter f, VarDecl varDecl)
        {
            if (varDecl == null)
                throw new ArgumentNullException(nameof(varDecl));

            writeIndent(f);
            if (varDecl.Mut)
                f.Write("var ");
            else
                f.Write("val ");
            writeLval(f, varDecl.Name);
            f.Write(": Int");
            f.Write(" = ");
            writeExpr(f, varDecl.Expr);
            f.Write("\n");
    	}

        protected override void writeConstExpr(StreamWriter f, ConstExpr expr, bool needsParens)
        {
            f.Write(expr.Val);
    	}

        protected override void writeAssignment(StreamWriter f, Assignment assignment)
        {
            if (assignment == null)
                throw new ArgumentNullException(nameof(assignment));

            writeIndent(f);
            writeLval(f, assignment.Lval);
            f.Write(" = ");
            writeExpr(f, assignment.Expr);
            f.Write("\n");
    	}

        protected override void writeReturn(StreamWriter f, Return statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            f.Write("return ");
            writeExpr(f, statement.Expr);
            f.Write("\n");
    	}

        protected override void writePrint(StreamWriter f, Print statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            f.Write("print(");
            writeExpr(f, statement.Expr);
            f.Write(")\n");
        }
    }

    class ScalaLang : KotlinLang
    {
        protected override string ext => "scala";

        protected override Dictionary<string, string> operators => new Dictionary<string, string> {
            ["&"] = "&",
            ["|"] = "|",
            ["^"] = "^"
        };

        public override void WriteProgram(StreamWriter f, Program program)
        {
            if (program == null)
                throw new ArgumentNullException(nameof(program));

            f.Write("object CompilerBenchmarker {\n");
            writeIndent(f);
            f.Write("\n");
            indent += 1;
            foreach (var funDecl in program.Functions)
            {
                writeIndent(f);
                writeFunDecl(f, funDecl);
                f.Write("\n");
            }
            writeFunDecl(f, program.Main, true);
            indent -= 1;
            f.Write("}");
        }

        protected override void writeFunDecl(StreamWriter f, FunDecl funDecl, bool main = false)
        {
            if (funDecl == null)
                throw new ArgumentNullException(nameof(funDecl));

            string optionalResult, typeName;
            if (funDecl.ReturnType == "")
            {
                optionalResult = "";
            }
            else
            {
                typeNames.TryGetValue(funDecl.ReturnType, out typeName);
                optionalResult = ": " + typeName;
            }
            var funName = main ? "main" : funDecl.Name;
            f.Write($"def {funName}(){optionalResult} = {{\n");
            indent += 1;
            foreach (var statement in funDecl.Statements)
                writeStatement(f, statement);
            indent -= 1;
            writeIndent(f);
            f.Write("}\n");
        }
    }


    //----------------------------------------------------------

    public class CodeGen
    {
        int randomSeed { get; set; } = Convert.ToInt32(new Random().NextDouble() * 100000);

        Lang GetLang(string lang)
        {
            switch (lang.ToLower())
            {
                case "c++": return new CppLang();
                case "c": return new CLang();
                case "d": return new DLang();
                case "go": return new GoLang();
                case "pascal": return new PascalLang();
                case "rust": return new RustLang();
                case "ocaml": return new OCamlLang();
                case "fsharp": return new FSharpLang();
                case "csharp": return new CSharpLang();
                case "haskell": return new HaskellLang();
                case "kotlin": return new KotlinLang();
                case "java": return new JavaLang();
                case "scala": return new ScalaLang();
                default:
                    throw new ArgumentOutOfRangeException(nameof(lang),
                        $"{lang} is not supported by the code generation. So code it up!");
            }
        }

        public void WriteLang(string lang, int numFuns, string filename)
        {
            var langwriter = GetLang(lang);
            if (File.Exists(filename))
                File.Delete(filename);

            using (var f = new StreamWriter(filename))
            {
                langwriter.WriteProgram(f,
                    new Context(null, new Random(randomSeed)).RandomProgram(
                        numFuns: numFuns, maxStatementsPerFun: 20));
            }
        }
    }
}