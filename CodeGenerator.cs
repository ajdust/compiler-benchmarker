using System;
using System.IO;
using System.Text;
using System.Linq;
using System.Collections.Generic;

namespace CompilerBenchmarker
{
    public static class RandomExtensions
    {
    	public static T Choice<T>(this Random r, IList<T> of)
    	{
    		return of[r.Next(0, of.Count)];
    	}
    }

    public interface IHasUsed
    {
        bool used {get;set;}
        string name {get;set;}
    }

    abstract class Expr
    {
        public virtual string name {get;set;}
    }

    class ConstExpr : Expr
    {
    	public string val {get;set;}
    	public ConstExpr(string val)
    	{
            if (val == null)
                throw new ArgumentNullException(nameof(val));

            this.val = val;
        }
    }

    class VarExpr : Expr
    {
    	public VarExpr(string name)
    	{
            if (name == null)
                throw new ArgumentNullException(nameof(name));

            this.name = name;
        }
    }

    class BinOp : Expr
    {
    	public string op {get;set;}
    	public Expr left {get;set;}
    	public Expr right {get;set;}
    	public BinOp(string op, Expr left, Expr right)
    	{
            if (op == null)
                throw new ArgumentNullException(nameof(op));
            if (left == null)
                throw new ArgumentNullException(nameof(left));
            if (right == null)
                throw new ArgumentNullException(nameof(right));


            this.op = op;
            this.left = left;
            this.right = right;
        }
    }

    class FunCallExpr : Expr
    {
    	public override string name {get;set;}
    	public FunCallExpr(string name)
    	{
            if (name == null)
                throw new ArgumentNullException(nameof(name));

            this.name = name;
        }
    }

    abstract class Statement
    {
        public Expr expr {get;set;}
    }

    class Assignment : Statement
    {
    	public string lval {get;set;}
    	public Assignment(string lval, Expr expr)
    	{
            if (expr == null)
                throw new ArgumentNullException(nameof(expr));
            if (lval == null)
                throw new ArgumentNullException(nameof(lval));

            this.lval = lval;
            this.expr = expr;
        }
    }

    class VarDecl : Statement, IHasUsed
    {
    	public string name {get;set;}
    	public bool mut {get;set;}
    	public bool used {get;set;}
    	public VarDecl(string name, Expr expr)
    	{
            if (expr == null)
                throw new ArgumentNullException(nameof(expr));
            if (name == null)
                throw new ArgumentNullException(nameof(name));

            this.name = name;
            this.expr = expr;
            this.mut = false;
            this.used = false;
        }
    }

    class Return : Statement
    {
    	public Return(Expr expr)
    	{
            if (expr == null)
                throw new ArgumentNullException(nameof(expr));

            this.expr = expr;
        }
    }

    class Print : Statement
    {
    	public Print(Expr expr)
    	{
            if (expr == null)
                throw new ArgumentNullException(nameof(expr));

            this.expr = expr;
        }
    }

    class FunDecl : IHasUsed
    {
    	public string name {get;set;}
    	public IList<Statement> statements {get;set;}
    	public string returnType {get;set;}
        public bool used {get;set;}
    	public FunDecl(string name, IList<Statement> statements, string returnType)
    	{
            if (name == null)
                throw new ArgumentNullException(nameof(name));
            if (statements == null)
                throw new ArgumentNullException(nameof(statements));
            if (returnType == null)
                throw new ArgumentNullException(nameof(returnType));

            this.name = name;
            this.statements = statements;
            this.returnType = returnType;
            this.used = false;
        }
    }

    class Program
    {
    	public FunDecl main {get;set;}
    	public IList<FunDecl> functions {get;set;}
    	public Program(FunDecl main, IList<FunDecl> functions)
    	{
            if (main == null)
                throw new ArgumentNullException(nameof(main));
            if (functions == null)
                throw new ArgumentNullException(nameof(functions));

            this.main = main;
            this.functions = functions;
        }
    }

    class Context
    {
    	public Dictionary<string, IHasUsed> env {get;set;} = new Dictionary<string, IHasUsed>();
    	public int id {get;set;} = 0;
    	public Context parent {get;set;}
    	public string declName {get;set;}
        public Random random {get;set;}

    	public Context(Context parent = null, string declName = null)
    	{
            this.parent = parent;
            this.declName = declName;
            this.random = new Random();
    	}

        public string name(string prefix, int i)
        {
            return $"{prefix}{i}";
    	}

        public string newName(string prefix)
        {
            id += 1;
            return name(prefix, id);
    	}

        public string randomName(string prefix)
        {
            var biasedMin = random.Next(1, id);
            var i = random.Next(biasedMin, id);
            return name(prefix, i);
    	}

        public Expr randomExpr()
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

        public IHasUsed findUnused()
        {
            foreach (var decl in env)
            {
                if (!decl.Value.used)
                    return decl.Value;
            }
            return null;
    	}

        public Expr forceUseExpr()
        {
            Expr expr = randomConstExpr();
            var decl = findUnused();
            while (decl != null)
            {
                var left = forcedVarExpr(decl.name);
                expr = forcedRandomBinaryOp(left, expr);
                decl = findUnused();
            }

            decl = parent.findUnused();
            while (decl != null)
            {
                var left = forcedFunCall(decl.name);
                expr = forcedRandomBinaryOp(left, expr);
                decl = parent.findUnused();
            }
            return expr;
    	}

        public ConstExpr randomConstExpr()
        {
            return new ConstExpr(random.Next(1, 1000).ToString());
    	}

        public VarExpr forcedVarExpr(string name)
        {
            var decl = env[name];
            decl.used = true;
            return new VarExpr(name);
    	}

        public Expr randomVarExpr()
        {
            if (id == 0)
                return randomConstExpr();
            var name = randomName("x");
            return forcedVarExpr(name);
    	}

        public BinOp forcedRandomBinaryOp(Expr left, Expr right)
        {
            //op = random.choice(["+", "-", "*", "|", "&", "^"]);
            var op = random.Choice(new[] {"|", "&", "^"});
            return new BinOp(op, left, right);
    	}

        public BinOp randomBinaryOp()
        {
            var left = randomExpr();
            var right = randomExpr();
            return forcedRandomBinaryOp(left, right);
    	}

        public FunCallExpr forcedFunCall(string name)
        {
            var decl = parent.env[name];
            decl.used = true;
            return new FunCallExpr(name);
    	}

        public Expr randomFunCall()
        {
            if (parent.id == 0)
                return randomConstExpr();
            var name = parent.randomName("f");
            return forcedFunCall(name);
    	}

        public Statement randomStatement()
        {
        	switch (random.Next(1, 3))
        	{
                case 1: return randomAssignment();
                case 2: return randomVarDecl();
                default: throw new ArgumentOutOfRangeException();
        	}
    	}

        public Assignment randomAssignment()
        {
            var name = randomName("x");
            var decl = env[name];
            var expr = randomExpr();
            if (!decl.used)
            {
                var left = forcedVarExpr(name);
                expr = forcedRandomBinaryOp(left, expr);
            }
            decl.used = false;
            if (decl is VarDecl)
                (decl as VarDecl).mut = true;
            return new Assignment(name, expr);
    	}

        public Return randomReturnStatement()
        {
            return new Return(forceUseExpr());
    	}

        public Print randomPrintStatement()
        {
            return new Print(forceUseExpr());
    	}

        public VarDecl randomVarDecl()
        {
            var expr = randomExpr();
            var name = newName("x");
            var decl = new VarDecl(name, expr);
            env[name] = decl;
            return decl;
    	}

        public FunDecl randomFunDecl(int numStatements, string returnType)
        {
            var local = new Context(this);
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
            // local.decl = decl;
            env[name] = decl;
            return decl;
    	}

        public Program randomProgram(int numFuns, int maxStatementsPerFun)
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
    	public int indent {get;set;} = 0;
    	public int extraIndent {get;set;} = 0;

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

            f.Write(expr.val);
    	}

        protected virtual void writeVarExpr(StreamWriter f, VarExpr expr, bool needsParens)
        {
            if (expr == null)
                throw new ArgumentNullException(nameof(expr));

            f.Write(expr.name);
    	}

        protected virtual void writeBinOp(StreamWriter f, BinOp expr, bool needsParens)
        {
            if (expr == null)
                throw new ArgumentNullException(nameof(expr));

            if (needsParens)
                f.Write("(");
            writeExpr(f, expr.left, needsParens);
            f.Write($" {operators[expr.op]} ");
            writeExpr(f, expr.right, needsParens);
            if (needsParens)
                f.Write(")");
    	}

        protected virtual void writeFunCall(StreamWriter f, FunCallExpr expr, bool needsParens)
        {
            if (expr == null)
                throw new ArgumentNullException(nameof(expr));

            f.Write($"{expr.name}()");
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
            foreach (var funDecl in program.functions)
                writeFunDecl(f, funDecl);
                f.Write("\n");
            writeFunDecl(f, program.main, true);
    	}

        protected override void writeFunDecl(StreamWriter f, FunDecl funDecl, bool main = false)
        {
            if (funDecl == null)
                throw new ArgumentNullException(nameof(funDecl));

            string optionalResult, typeName;
            if (funDecl.returnType == "")
                optionalResult = "int ";
            else
            {
                typeNames.TryGetValue(funDecl.returnType, out typeName);
                optionalResult = $"{typeName} ";
            }
            var funName = main ? "main" : funDecl.name;
            f.Write($"{optionalResult} {funName}() {{\n");
            indent += 1;
            foreach (var statement in funDecl.statements)
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
            writeLval(f, varDecl.name);
            f.Write(" = ");
            writeExpr(f, varDecl.expr);
            f.Write(";\n");
    	}

        protected override void writeAssignment(StreamWriter f, Assignment assignment)
        {
            if (assignment == null)
                throw new ArgumentNullException(nameof(assignment));

            writeIndent(f);
            writeLval(f, assignment.lval);
            f.Write(" = ");
            writeExpr(f, assignment.expr);
            f.Write(";\n");
    	}

        protected override void writeReturn(StreamWriter f, Return statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            f.Write("return ");
            writeExpr(f, statement.expr);
            f.Write(";\n");
    	}

        protected override void writePrint(StreamWriter f, Print statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            f.Write("printf(\"%i\\n\", ");
            writeExpr(f, statement.expr);
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
            foreach (var funDecl in program.functions)
            {
                writeFunDecl(f, funDecl);
                f.Write("\n");
            }
            writeFunDecl(f, program.main, true);
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
            foreach (var funDecl in program.functions)
                writeFunDecl(f, funDecl);
                f.Write("\n");
            writeFunDecl(f, program.main, true);
    	}

        protected override void writeFunDecl(StreamWriter f, FunDecl funDecl, bool main = false)
        {
            if (funDecl == null)
                throw new ArgumentNullException(nameof(funDecl));

            string optionalResult, typeName;
            if (funDecl.returnType == "")
                optionalResult = "void ";
            else
            {
                typeNames.TryGetValue(funDecl.returnType, out typeName);
                optionalResult = $"{typeName} ";
            }
            var funName = main ? "main" : funDecl.name;
            f.Write($"{optionalResult} {funName}() {{\n");
            indent += 1;
            foreach (var statement in funDecl.statements)
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
            writeLval(f, varDecl.name);
            f.Write(" = ");
            writeExpr(f, varDecl.expr);
            f.Write(";\n");
    	}

        protected override void writeAssignment(StreamWriter f, Assignment assignment)
        {
            if (assignment == null)
                throw new ArgumentNullException(nameof(assignment));

            writeIndent(f);
            writeLval(f, assignment.lval);
            f.Write(" = ");
            writeExpr(f, assignment.expr);
            f.Write(";\n");
    	}

        protected override void writeReturn(StreamWriter f, Return statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            f.Write("return ");
            writeExpr(f, statement.expr);
            f.Write(";\n");
    	}

        protected override void writePrint(StreamWriter f, Print statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            f.Write("writefln(\"%d\", ");
            writeExpr(f, statement.expr);
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
            foreach (var funDecl in program.functions)
            {
                writeFunDecl(f, funDecl);
                f.Write("\n");
            }
            writeFunDecl(f, program.main, true);
    	}

        protected override void writeFunDecl(StreamWriter f, FunDecl funDecl, bool main = false)
        {
            if (funDecl == null)
                throw new ArgumentNullException(nameof(funDecl));

            string optionalResult, typeName;
            if (funDecl.returnType == "")
                optionalResult = "";
            else
            {
                typeNames.TryGetValue(funDecl.returnType, out typeName);
                optionalResult = " " + typeName;
            }
            var funName = main ? "main" : funDecl.name;
            f.Write($"func {funName}(){optionalResult} {{\n");
            indent += 1;
            foreach (var statement in funDecl.statements)
                writeStatement(f, statement);
            indent -= 1;
            f.Write("}\n");
    	}

        protected override void writeVarDecl(StreamWriter f, VarDecl varDecl)
        {
            if (varDecl == null)
                throw new ArgumentNullException(nameof(varDecl));

            writeIndent(f);
            writeLval(f, varDecl.name);
            f.Write(" := ");
            writeExpr(f, varDecl.expr);
            f.Write("\n");
    	}

        protected override void writeAssignment(StreamWriter f, Assignment assignment)
        {
            if (assignment == null)
                throw new ArgumentNullException(nameof(assignment));

            writeIndent(f);
            writeLval(f, assignment.lval);
            f.Write(" = ");
            writeExpr(f, assignment.expr);
            f.Write("\n");
    	}

        protected override void writeReturn(StreamWriter f, Return statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            f.Write("return ");
            writeExpr(f, statement.expr);
            f.Write("\n");
    	}

        protected override void writePrint(StreamWriter f, Print statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            f.Write("fmt.Printf(\"%d\\n\", ");
            writeExpr(f, statement.expr);
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
            foreach (var funDecl in program.functions)
            {
                writeFunDecl(f, funDecl);
                f.Write("\n");
            }
            writeFunDecl(f, program.main, true);
    	}

        protected override void writeFunDecl(StreamWriter f, FunDecl funDecl, bool main = false)
        {
            if (funDecl == null)
                throw new ArgumentNullException(nameof(funDecl));

            string funName;
            string typeName;
            if (!main)
            {
                funName = funDecl.name;
                typeNames.TryGetValue(funDecl.returnType, out typeName);
                f.Write($"function {funName}() : {typeName};\n");
            }
            var vars = funDecl.statements.OfType<VarDecl>();
            if (vars != null && vars.Any())
            {
                f.Write("var\n");
                foreach (var v in vars)
                {
                    typeName = typeNames["int"];
                    f.Write($"  {v.name} : {typeName};\n");
                }
            }
            f.Write("begin\n");
            indent += 1;
            foreach (var statement in funDecl.statements)
                writeStatement(f, statement);
            indent -= 1;
            f.Write("end{(main ? '.' : ';')}\n");
    	}

        protected override void writeVarDecl(StreamWriter f, VarDecl varDecl)
        {
            if (varDecl == null)
                throw new ArgumentNullException(nameof(varDecl));

            writeIndent(f);
            writeLval(f, varDecl.name);
            f.Write(" := ");
            writeExpr(f, varDecl.expr);
            f.Write(";\n");
    	}

        protected override void writeAssignment(StreamWriter f, Assignment assignment)
        {
            if (assignment == null)
                throw new ArgumentNullException(nameof(assignment));

            writeIndent(f);
            writeLval(f, assignment.lval);
            f.Write(" := ");
            writeExpr(f, assignment.expr);
            f.Write(";\n");
    	}

        protected override void writeReturn(StreamWriter f, Return statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            writeLval(f, statement.expr.name);
            f.Write(" := ");
            writeExpr(f, statement.expr);
            f.Write(";\n");
    	}

        protected override void writePrint(StreamWriter f, Print statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            f.Write("writeln(");
            writeExpr(f, statement.expr);
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

            f.Write("#![allow(unusedParens)]\n\n");
            foreach (var funDecl in program.functions)
            {
                writeFunDecl(f, funDecl);
                f.Write("\n");
            }
            writeFunDecl(f, program.main, true);
    	}

        protected override void writeFunDecl(StreamWriter f, FunDecl funDecl, bool main = false)
        {
            if (funDecl == null)
                throw new ArgumentNullException(nameof(funDecl));

            string optionalResult;
            string funName, typeName;
            if (funDecl.returnType == "")
                optionalResult = "";
            else
            {
                typeNames.TryGetValue(funDecl.returnType, out typeName);
                optionalResult = $" -> {typeName}";
            }
            funName = main ? "main" : funDecl.name;
            f.Write($"fn {funName}(){optionalResult} {{\n");
            indent += 1;
            foreach (var statement in funDecl.statements)
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
            if (varDecl.mut)
                f.Write("mut ");
            writeLval(f, varDecl.name);
            f.Write(": i32");
            f.Write(" = ");
            writeExpr(f, varDecl.expr);
            f.Write(";\n");
    	}

        protected override void writeAssignment(StreamWriter f, Assignment assignment)
        {
            if (assignment == null)
                throw new ArgumentNullException(nameof(assignment));

            writeIndent(f);
            writeLval(f, assignment.lval);
            f.Write(" = ");
            writeExpr(f, assignment.expr);
            f.Write(";\n");
    	}

        protected override void writeReturn(StreamWriter f, Return statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            writeExpr(f, statement.expr);
            f.Write("\n");
    	}

        protected override void writePrint(StreamWriter f, Print statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            f.Write("println!(\"{}\", ");
            writeExpr(f, statement.expr);
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

            foreach (var funDecl in program.functions)
            {
                writeFunDecl(f, funDecl);
                f.Write("\n");
            }
            writeFunDecl(f, program.main, true);
    	}

        protected override void writeFunDecl(StreamWriter f, FunDecl funDecl, bool main = false)
        {
            if (funDecl == null)
                throw new ArgumentNullException(nameof(funDecl));

            string optionalResult, typeName;
            if (funDecl.returnType == "")
                optionalResult = "";
            else
            {
                typeNames.TryGetValue(funDecl.returnType, out typeName);
                optionalResult = $":{typeName} ";
            }
            var funName = main ? "main" : funDecl.name;
            f.Write($"let {funName} (){optionalResult} = \n");
            indent += 1;
            foreach (var statement in funDecl.statements)
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
            writeLval(f, varDecl.name);
            f.Write(" = ");
            writeExpr(f, varDecl.expr);
            f.Write(" in \n");
    	}

        protected override void writeAssignment(StreamWriter f, Assignment assignment)
        {
            if (assignment == null)
                throw new ArgumentNullException(nameof(assignment));

            writeIndent(f);
            f.Write("let ");
            writeLval(f, assignment.lval);
            f.Write(" = ");
            writeExpr(f, assignment.expr);
            f.Write(" in \n");
    	}

        protected override void writeReturn(StreamWriter f, Return statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            writeExpr(f, statement.expr);
            f.Write("\n");
    	}

        protected override void writePrint(StreamWriter f, Print statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            f.Write("Printf.printf \"%i\\n\" (");
            writeExpr(f, statement.expr);
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

            foreach (var funDecl in program.functions)
            {
                writeFunDecl(f, funDecl);
                f.Write("\n");
            }
            writeFunDecl(f, program.main, true);
    	}

        protected override void writeFunDecl(StreamWriter f, FunDecl funDecl, bool main = false)
        {
            if (funDecl == null)
                throw new ArgumentNullException(nameof(funDecl));

            string optionalResult, typeName;
            if (funDecl.returnType == "")
                optionalResult = "";
            else
            {
                typeNames.TryGetValue(funDecl.returnType, out typeName);
                optionalResult = ":" + typeName + " ";
            }
            var funName = main ? "main" : funDecl.name;
            f.Write($"let {funName} (){optionalResult} = \n");
            indent += 1;
            foreach (var statement in funDecl.statements)
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
            writeLval(f, varDecl.name);
            f.Write(" = ");
            writeExpr(f, varDecl.expr);
            f.Write(" in \n");
    	}

        protected override void writeAssignment(StreamWriter f, Assignment assignment)
        {
            if (assignment == null)
                throw new ArgumentNullException(nameof(assignment));

            writeIndent(f);
            f.Write("let ");
            writeLval(f, assignment.lval);
            f.Write(" = ");
            writeExpr(f, assignment.expr);
            f.Write(" in \n");
    	}

        protected override void writeReturn(StreamWriter f, Return statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            writeExpr(f, statement.expr);
            f.Write("\n");
    	}

        protected override void writePrint(StreamWriter f, Print statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            f.Write("printfn \"%i\\n\" (");
            writeExpr(f, statement.expr);
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
            foreach (var funDecl in program.functions)
            {
                writeFunDecl(f, funDecl);
                f.Write("\n");
            }
            writeFunDecl(f, program.main, true);
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
                fund = $"{funDecl.name} = ";
                f.Write(funDecl.name + " :: Int32\n");
                f.Write(fund);
            }

            if (funDecl.statements.Count > 0)
                writeStatement(f, funDecl.statements[0]);
            extraIndent = fund.Length;
            foreach (var statement in funDecl.statements.Skip(1))
                writeStatement(f, statement);
            extraIndent = 0;
    	}

        protected override void writeVarDecl(StreamWriter f, VarDecl varDecl)
        {
            if (varDecl == null)
                throw new ArgumentNullException(nameof(varDecl));

            writeIndent(f);
            f.Write("let ");
            writeLval(f, varDecl.name);
            f.Write(" = ");
            writeExpr(f, varDecl.expr);
            f.Write(" in \n");
    	}

        protected override void writeAssignment(StreamWriter f, Assignment assignment)
        {
            if (assignment == null)
                throw new ArgumentNullException(nameof(assignment));

            if (assignment.expr is VarExpr && assignment.lval == assignment.expr.name)
                return;

            writeIndent(f);
            f.Write("let ");
            writeLval(f, assignment.lval);
            f.Write(" = ");
            writeExpr(f, assignment.expr);
            f.Write(" in \n");
    	}

        protected override void writeReturn(StreamWriter f, Return statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            writeExpr(f, statement.expr);
            f.Write("\n");
    	}

        protected override void writePrint(StreamWriter f, Print statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            f.Write("printf \"%i\\n\" (");
            writeExpr(f, statement.expr);
            f.Write(")\n");
    	}

        protected override void writeFunCall(StreamWriter f, FunCallExpr expr, bool needsParens)
        {
            f.Write(expr.name);
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
            foreach (var funDecl in program.functions)
            {
                writeFunDecl(f, funDecl);
                f.Write("\n");
            }
            writeFunDecl(f, program.main, true);
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
            if (funDecl.returnType == "")
                optionalResult = "static int";
            else
            {
                typeNames.TryGetValue(funDecl.returnType, out typeName);
                optionalResult = "static " + typeName;
            }
            var funName = main ? "Main" : funDecl.name;
            writeIndent(f);
            f.Write($"{optionalResult} {funName}()\n");
            writeIndent(f);
            f.Write("{\n");
            indent += 1;
            foreach (var statement in funDecl.statements)
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
            writeExpr(f, statement.expr);
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
            foreach (var funDecl in program.functions)
            {
                writeFunDecl(f, funDecl);
                f.Write("\n");
            }
            writeFunDecl(f, program.main, true);
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
            writeExpr(f, statement.expr);
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

            foreach (var funDecl in program.functions)
            {
                writeFunDecl(f, funDecl);
                f.Write("\n");
            }
            writeFunDecl(f, program.main, true);
    	}

        protected override void writeFunDecl(StreamWriter f, FunDecl funDecl, bool main = false)
        {
            if (funDecl == null)
                throw new ArgumentNullException(nameof(funDecl));

            string optionalResult, typeName;
            if (funDecl.returnType == "")
                optionalResult = "";
            else
            {
                typeNames.TryGetValue(funDecl.returnType, out typeName);
                optionalResult = ": " + typeName;
            }
            var funName = main ? "main" : funDecl.name;
            f.Write($"fun {funName}(){optionalResult} {{\n");
            indent += 1;
            foreach (var statement in funDecl.statements)
                writeStatement(f, statement);
            indent -= 1;
            f.Write("}\n");
    	}

        protected override void writeVarDecl(StreamWriter f, VarDecl varDecl)
        {
            if (varDecl == null)
                throw new ArgumentNullException(nameof(varDecl));

            writeIndent(f);
            if (varDecl.mut)
                f.Write("var ");
            else
                f.Write("val ");
            writeLval(f, varDecl.name);
            f.Write(": Int");
            f.Write(" = ");
            writeExpr(f, varDecl.expr);
            f.Write("\n");
    	}

        protected override void writeConstExpr(StreamWriter f, ConstExpr expr, bool needsParens)
        {
            f.Write(expr.val);
    	}

        protected override void writeAssignment(StreamWriter f, Assignment assignment)
        {
            if (assignment == null)
                throw new ArgumentNullException(nameof(assignment));

            writeIndent(f);
            writeLval(f, assignment.lval);
            f.Write(" = ");
            writeExpr(f, assignment.expr);
            f.Write("\n");
    	}

        protected override void writeReturn(StreamWriter f, Return statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            f.Write("return ");
            writeExpr(f, statement.expr);
            f.Write("\n");
    	}

        protected override void writePrint(StreamWriter f, Print statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            writeIndent(f);
            f.Write("print(");
            writeExpr(f, statement.expr);
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
            foreach (var funDecl in program.functions)
            {
                writeIndent(f);
                writeFunDecl(f, funDecl);
                f.Write("\n");
            }
            writeFunDecl(f, program.main, true);
            indent -= 1;
            f.Write("}");
        }

        protected override void writeFunDecl(StreamWriter f, FunDecl funDecl, bool main = false)
        {
            if (funDecl == null)
                throw new ArgumentNullException(nameof(funDecl));

            string optionalResult, typeName;
            if (funDecl.returnType == "")
                optionalResult = "";
            else
            {
                typeNames.TryGetValue(funDecl.returnType, out typeName);
                optionalResult = ": " + typeName;
            }
            var funName = main ? "main" : funDecl.name;
            f.Write($"def {funName}(){optionalResult} = {{\n");
            indent += 1;
            foreach (var statement in funDecl.statements)
                writeStatement(f, statement);
            indent -= 1;
            f.Write("}\n");
        }
    }


    //----------------------------------------------------------

    public class CodeGen
    {
        private Lang GetLang(string lang)
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
                    throw new ArgumentOutOfRangeException(nameof(lang), $"{lang} is not supported by the code generation. So code it for us all to enjoy!");
            }
        }

        public void WriteLang(string lang, int numFuns, string filename)
        {
            var langwriter = GetLang(lang);
            if (File.Exists(filename))
                File.Delete(filename);

            // todo: rename to be C# standard style naming
            // todo: write language files identically
            using (var f = new StreamWriter(filename))
            {
                langwriter.WriteProgram(f,
                    new Context().randomProgram(
                        numFuns: numFuns, maxStatementsPerFun: 20));
            }
        }
    }
}