using System;
using System.IO;
using System.Text;
using System.Linq;
using System.Collections.Generic;

namespace CompilerBenchmarker
{
    public class Writer
    {
        private StreamWriter s;

        public Writer(StreamWriter s)
        {
            this.s = s;
        }

    	public void write(string text)
        {
            s.Write(text);
        }
    }

    public static class random
    {
    	public static int randint(int a, int b)
    	{
    		return new Random().Next(a, b);
    	}

    	public static T choice<T>(IList<T> of)
    	{
    		return of[new Random().Next(0, of.Count)];
    	}
    }


    public interface IHasUsed
    {
        bool used { get; set; }
        string name { get; set; }
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
    	public string return_type {get;set;}
        public bool used {get;set;}
    	public FunDecl(string name, IList<Statement> statements, string return_type)
    	{
            if (name == null)
                throw new ArgumentNullException(nameof(name));
            if (statements == null)
                throw new ArgumentNullException(nameof(statements));
            if (return_type == null)
                throw new ArgumentNullException(nameof(return_type));

            this.name = name;
            this.statements = statements;
            this.return_type = return_type;
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
    	public string decl_name {get;set;}
    	public Context(Context parent=null, string decl_name=null)
    	{
            this.parent = parent;
            this.decl_name = decl_name;
    	}

        public string name(string _base, int i)
        {
            return $"{_base}{i}";
    	}

        public string new_name(string _base)
        {
            id += 1;
            return name(_base, id);
    	}

        public string random_name(string _base)
        {
            var biased_min = random.randint(1, id);
            var i = random.randint(biased_min, id);
            return name(_base, i);
    	}

        public Expr random_expr()
        {
        	switch (random.randint(1, 5))
        	{
                case 1: return random_const_expr();
                case 2: return random_var_expr();
                case 3: return random_binary_op();
                case 4: return random_fun_call();
                default: throw new ArgumentOutOfRangeException();
            };
    	}

        public IHasUsed find_unused()
        {
            foreach (var decl in env)
            {
                if (!decl.Value.used)
                    return decl.Value;
            }
            return null;
    	}

        public Expr force_use_expr()
        {
            Expr expr = random_const_expr();
            var decl = find_unused();
            while (decl != null)
            {
                var left = forced_var_expr(decl.name);
                expr = forced_random_binary_op(left, expr);
                decl = find_unused();
            }

            decl = parent.find_unused();
            while (decl != null)
            {
                var left = forced_fun_call(decl.name);
                expr = forced_random_binary_op(left, expr);
                decl = parent.find_unused();
            }
            return expr;
    	}

        public ConstExpr random_const_expr()
        {
            return new ConstExpr(random.randint(1, 1000).ToString());
    	}

        public VarExpr forced_var_expr(string name)
        {
            var decl = env[name];
            decl.used = true;
            return new VarExpr(name);
    	}

        public Expr random_var_expr()
        {
            if (id == 0)
                return random_const_expr();
            var name = random_name("x");
            return forced_var_expr(name);
    	}

        public BinOp forced_random_binary_op(Expr left, Expr right)
        {
            //op = random.choice(["+", "-", "*", "|", "&", "^"]);
            var op = random.choice(new[] {"|", "&", "^"});
            return new BinOp(op, left, right);
    	}

        public BinOp random_binary_op()
        {
            var left = random_expr();
            var right = random_expr();
            return forced_random_binary_op(left, right);
    	}

        public FunCallExpr forced_fun_call(string name)
        {
            var decl = parent.env[name];
            decl.used = true;
            return new FunCallExpr(name);
    	}

        public Expr random_fun_call()
        {
            if (parent.id == 0)
                return random_const_expr();
            var name = parent.random_name("f");
            return forced_fun_call(name);
    	}

        public Statement random_statement()
        {
        	switch (random.randint(1, 3))
        	{
                case 1: return random_assignment();
                case 2: return random_var_decl();
                default: throw new ArgumentOutOfRangeException();
        	}
    	}

        public Assignment random_assignment()
        {
            var name = random_name("x");
            var decl = env[name];
            var expr = random_expr();
            if (!decl.used)
            {
                var left = forced_var_expr(name);
                expr = forced_random_binary_op(left, expr);
            }
            decl.used = false;
            if (decl is VarDecl)
                (decl as VarDecl).mut = true;
            return new Assignment(name, expr);
    	}

        public Return random_return_statement()
        {
            return new Return(force_use_expr());
    	}

        public Print random_print_statement()
        {
            return new Print(force_use_expr());
    	}

        public VarDecl random_var_decl()
        {
            var expr = random_expr();
            var name = new_name("x");
            var decl = new VarDecl(name, expr);
            env[name] = decl;
            return decl;
    	}

        public FunDecl random_fun_decl(int num_statements, string return_type)
        {
            var local = new Context(this);
            var statements = new List<Statement>();
            statements.Add(local.random_var_decl());
            foreach (var i in Enumerable.Range(0, num_statements))
                statements.Add(local.random_statement());
            if (return_type != "")
                statements.Add(local.random_return_statement());
            else
                statements.Add(local.random_print_statement());
            var name = new_name("f");
            var decl = new FunDecl(name, statements, return_type);
            // local.decl = decl;
            env[name] = decl;
            return decl;
    	}

        public Program random_program(int num_funs, int max_statements_per_fun)
        {
            var functions = new List<FunDecl>();
            int num_statements;
            foreach (var i in Enumerable.Range(0, num_funs))
            {
                num_statements = random.randint(1, max_statements_per_fun);
                var fun_decl = random_fun_decl(num_statements, "int");
                functions.Add(fun_decl);
            }
            num_statements = random.randint(1, max_statements_per_fun);
            var main = random_fun_decl(num_statements, "");
            return new Program(main, functions);
        }
    }

    class Lang
    {
        public virtual string ext { get; }
    	public int indent {get;set;} = 0;
    	public int extra_indent {get;set;} = 0;

        public virtual Dictionary<string, string> operators => new Dictionary<string,string> {
            {"&", "&"},
            {"|", "|"},
            {"^", "^"}
        };

        public virtual void write_indent(Writer f)
        {
            var num = 4 * indent + extra_indent;
            string s = "";
            for (; num > 0; num--)
            {
                s += " ";
            }
            f.write(s);
    	}

        public virtual void write_fun_decl(Writer f, FunDecl s, bool main=false)
        {
            throw new NotImplementedException();
        }
        public virtual void write_var_decl(Writer f, VarDecl s)
        {
            throw new NotImplementedException();
        }
        public virtual void write_assignment(Writer f, Assignment s)
        {
            throw new NotImplementedException();
        }
        public virtual void write_return(Writer f, Return s)
        {
            throw new NotImplementedException();
        }
        public virtual void write_print(Writer f, Print s)
        {
            throw new NotImplementedException();
        }
        public virtual void write_program(Writer f, Program s)
        {
            throw new NotImplementedException();
        }

        public virtual void write_statement(Writer f, Statement statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            if (statement is VarDecl)
                write_var_decl(f, statement as VarDecl);
            else if (statement is Assignment)
                write_assignment(f, statement as Assignment);
            else if (statement is Return)
                write_return(f, statement as Return);
            else if (statement is Print)
                write_print(f, statement as Print);
            else
                throw new Exception("Unknown kind of statement");
    	}

        public virtual void write_lval(Writer f, string lval)
        {
            if (lval == null)
                throw new ArgumentNullException(nameof(lval));

            f.write(lval);
    	}

        public virtual void write_expr(Writer f, Expr expr, bool needs_parens=false)
        {
            if (expr == null)
                throw new ArgumentNullException(nameof(expr));

            if (expr is ConstExpr)
                write_const_expr(f, expr as ConstExpr, needs_parens);
            else if (expr is VarExpr)
                write_var_expr(f, expr as VarExpr, needs_parens);
            else if (expr is BinOp)
                write_bin_op(f, expr as BinOp, true);
            else if (expr is FunCallExpr)
                write_fun_call(f, expr as FunCallExpr, needs_parens);
            else
                throw new Exception("Unknown kind of expr");
    	}

        public virtual void write_const_expr(Writer f, ConstExpr expr, bool needs_parens)
        {
            if (expr == null)
                throw new ArgumentNullException(nameof(expr));

            f.write(expr.val);
    	}

        public virtual void write_var_expr(Writer f, VarExpr expr, bool needs_parens)
        {
            if (expr == null)
                throw new ArgumentNullException(nameof(expr));

            f.write(expr.name);
    	}

        public virtual void write_bin_op(Writer f, BinOp expr, bool needs_parens)
        {
            if (expr == null)
                throw new ArgumentNullException(nameof(expr));

            if (needs_parens)
                f.write("(");
            write_expr(f, expr.left, needs_parens);
            f.write($" {operators[expr.op]} ");
            write_expr(f, expr.right, needs_parens);
            if (needs_parens)
                f.write(")");
    	}

        public virtual void write_fun_call(Writer f, FunCallExpr expr, bool needs_parens)
        {
            if (expr == null)
                throw new ArgumentNullException(nameof(expr));

            f.write($"{expr.name}()");
        }
    }

    class CppLang : Lang
    {
        public override string ext => "cpp";

        public Dictionary<string, string> type_names => new Dictionary<string, string> {
            ["int"] = "int"
        };

        public override void write_program(Writer f, Program program)
        {
            if (program == null)
                throw new ArgumentNullException(nameof(program));

            f.write("#include <cstdio>\n\n");
            foreach (var  fun_decl in program.functions)
                write_fun_decl(f, fun_decl);
                f.write("\n");
            write_fun_decl(f, program.main, true);
    	}

        public override void write_fun_decl(Writer f, FunDecl fun_decl, bool main=false)
        {
            if (fun_decl == null)
                throw new ArgumentNullException(nameof(fun_decl));

            string optional_result, type_name;
            if (fun_decl.return_type == "")
                optional_result = "int ";
            else
            {
                type_names.TryGetValue(fun_decl.return_type, out type_name);
                optional_result = $"{type_name} ";
            }
            var fun_name = main ? "main" : fun_decl.name;
            f.write($"{optional_result} {fun_name}() {{\n");
            indent += 1;
            foreach (var statement in fun_decl.statements)
                write_statement(f, statement);
            indent -= 1;
            f.write("}\n");
    	}

        public override void write_var_decl(Writer f, VarDecl var_decl)
        {
            if (var_decl == null)
                throw new ArgumentNullException(nameof(var_decl));

            write_indent(f);
            f.write("int ");
            write_lval(f, var_decl.name);
            f.write(" = ");
            write_expr(f, var_decl.expr);
            f.write(";\n");
    	}

        public override void write_assignment(Writer f, Assignment assignment)
        {
            if (assignment == null)
                throw new ArgumentNullException(nameof(assignment));

            write_indent(f);
            write_lval(f, assignment.lval);
            f.write(" = ");
            write_expr(f, assignment.expr);
            f.write(";\n");
    	}

        public override void write_return(Writer f, Return statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            write_indent(f);
            f.write("return ");
            write_expr(f, statement.expr);
            f.write(";\n");
    	}

        public override void write_print(Writer f, Print statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            write_indent(f);
            f.write("printf(\"%i\\n\", ");
            write_expr(f, statement.expr);
            f.write(");\n");
        }
    }

    class CLang : CppLang
    {
        public override string ext => "c";

        public override void write_program(Writer f, Program program)
        {
            if (program == null)
                throw new ArgumentNullException(nameof(program));

            f.write("#include <stdio.h>\n\n");
            foreach (var  fun_decl in program.functions)
            {
                write_fun_decl(f, fun_decl);
                f.write("\n");
            }
            write_fun_decl(f, program.main, true);
        }
    }

    class DLang : Lang
    {
        public override string ext => "d";

        public Dictionary<string, string> type_names => new Dictionary<string, string> {
            ["int"] = "int",
        };

        public override void write_program(Writer f, Program program)
        {
            if (program == null)
                throw new ArgumentNullException(nameof(program));

            f.write("import std.stdio;\n\n");
            foreach (var  fun_decl in program.functions)
                write_fun_decl(f, fun_decl);
                f.write("\n");
            write_fun_decl(f, program.main, true);
    	}

        public override void write_fun_decl(Writer f, FunDecl fun_decl, bool main=false)
        {
            if (fun_decl == null)
                throw new ArgumentNullException(nameof(fun_decl));

            string optional_result, type_name;
            if (fun_decl.return_type == "")
                optional_result = "void ";
            else
            {
                type_names.TryGetValue(fun_decl.return_type, out type_name);
                optional_result = $"{type_name} ";
            }
            var fun_name = main ? "main" : fun_decl.name;
            f.write($"{optional_result} {fun_name}() {{\n");
            indent += 1;
            foreach (var statement in fun_decl.statements)
                write_statement(f, statement);
            indent -= 1;
            f.write("}\n");
    	}

        public override void write_var_decl(Writer f, VarDecl var_decl)
        {
            if (var_decl == null)
                throw new ArgumentNullException(nameof(var_decl));

            write_indent(f);
            f.write("int ");
            write_lval(f, var_decl.name);
            f.write(" = ");
            write_expr(f, var_decl.expr);
            f.write(";\n");
    	}

        public override void write_assignment(Writer f, Assignment assignment)
        {
            if (assignment == null)
                throw new ArgumentNullException(nameof(assignment));

            write_indent(f);
            write_lval(f, assignment.lval);
            f.write(" = ");
            write_expr(f, assignment.expr);
            f.write(";\n");
    	}

        public override void write_return(Writer f, Return statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            write_indent(f);
            f.write("return ");
            write_expr(f, statement.expr);
            f.write(";\n");
    	}

        public override void write_print(Writer f, Print statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            write_indent(f);
            f.write("writefln(\"%d\", ");
            write_expr(f, statement.expr);
            f.write(");\n");
        }
    }

    class GoLang : Lang
    {
        public override string ext => "go";

        public Dictionary<string, string> type_names => new Dictionary<string, string> {
            ["int"] = "int",
        };

        public override void write_program(Writer f, Program program)
        {
            if (program == null)
                throw new ArgumentNullException(nameof(program));

            f.write("package main\n\n");
            f.write("import \"fmt\"\n\n");
            foreach (var fun_decl in program.functions)
            {
                write_fun_decl(f, fun_decl);
                f.write("\n");
            }
            write_fun_decl(f, program.main, true);
    	}

        public override void write_fun_decl(Writer f, FunDecl fun_decl, bool main=false)
        {
            if (fun_decl == null)
                throw new ArgumentNullException(nameof(fun_decl));

            string optional_result, type_name;
            if (fun_decl.return_type == "")
                optional_result = "";
            else
            {
                type_names.TryGetValue(fun_decl.return_type, out type_name);
                optional_result = " " + type_name;
            }
            var fun_name = main ? "main" : fun_decl.name;
            f.write($"func {fun_name}(){optional_result} {{\n");
            indent += 1;
            foreach (var statement in fun_decl.statements)
                write_statement(f, statement);
            indent -= 1;
            f.write("}\n");
    	}

        public override void write_var_decl(Writer f, VarDecl var_decl)
        {
            if (var_decl == null)
                throw new ArgumentNullException(nameof(var_decl));

            write_indent(f);
            write_lval(f, var_decl.name);
            f.write(" := ");
            write_expr(f, var_decl.expr);
            f.write("\n");
    	}

        public override void write_assignment(Writer f, Assignment assignment)
        {
            if (assignment == null)
                throw new ArgumentNullException(nameof(assignment));

            write_indent(f);
            write_lval(f, assignment.lval);
            f.write(" = ");
            write_expr(f, assignment.expr);
            f.write("\n");
    	}

        public override void write_return(Writer f, Return statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            write_indent(f);
            f.write("return ");
            write_expr(f, statement.expr);
            f.write("\n");
    	}

        public override void write_print(Writer f, Print statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            write_indent(f);
            f.write("fmt.Printf(\"%d\\n\", ");
            write_expr(f, statement.expr);
            f.write(")\n");
        }
    }

    class PascalLang : Lang
    {
        public override string ext => "pas";

        public Dictionary<string, string> type_names => new Dictionary<string, string> {
            ["int"] = "integer",
        };

        public override Dictionary<string, string> operators => new Dictionary<string, string> {
            ["&"] = "and",
            ["|"] = "or",
            ["^"] = "xor",
        };

        public override void write_program(Writer f, Program program)
        {
            if (program == null)
                throw new ArgumentNullException(nameof(program));

            f.write("program main;\n\n");
            foreach (var  fun_decl in program.functions)
            {
                write_fun_decl(f, fun_decl);
                f.write("\n");
            }
            write_fun_decl(f, program.main, true);
    	}

        public override void write_fun_decl(Writer f, FunDecl fun_decl, bool main=false)
        {
            if (fun_decl == null)
                throw new ArgumentNullException(nameof(fun_decl));

            string fun_name;
            string type_name;
            if (!main)
            {
                fun_name = fun_decl.name;
                type_names.TryGetValue(fun_decl.return_type, out type_name);
                f.write($"function {fun_name}() : {type_name};\n");
            }
            var vars = fun_decl.statements.OfType<VarDecl>();
            if (vars != null && vars.Any())
            {
                f.write("var\n");
                foreach (var v in vars)
                {
                    type_name = type_names["int"];
                    f.write($"  {v.name} : {type_name};\n");
                }
            }
            f.write("begin\n");
            indent += 1;
            foreach (var statement in fun_decl.statements)
                write_statement(f, statement);
            indent -= 1;
            f.write("end{(main ? '.' : ';')}\n");
    	}

        public override void write_var_decl(Writer f, VarDecl var_decl)
        {
            if (var_decl == null)
                throw new ArgumentNullException(nameof(var_decl));

            write_indent(f);
            write_lval(f, var_decl.name);
            f.write(" := ");
            write_expr(f, var_decl.expr);
            f.write(";\n");
    	}

        public override void write_assignment(Writer f, Assignment assignment)
        {
            if (assignment == null)
                throw new ArgumentNullException(nameof(assignment));

            write_indent(f);
            write_lval(f, assignment.lval);
            f.write(" := ");
            write_expr(f, assignment.expr);
            f.write(";\n");
    	}

        public override void write_return(Writer f, Return statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            write_indent(f);
            write_lval(f, statement.expr.name);
            f.write(" := ");
            write_expr(f, statement.expr);
            f.write(";\n");
    	}

        public override void write_print(Writer f, Print statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            write_indent(f);
            f.write("writeln(");
            write_expr(f, statement.expr);
            f.write(");\n");
        }
    }

    class RustLang : Lang
    {
        public override string ext => "rs";

        public Dictionary<string, string> type_names => new Dictionary<string, string> {
            ["int"] = "i32",
        };

        public override void write_program(Writer f, Program program)
        {
            if (program == null)
                throw new ArgumentNullException(nameof(program));

            foreach (var  fun_decl in program.functions)
            {
                write_fun_decl(f, fun_decl);
                f.write("\n");
            }
            write_fun_decl(f, program.main, true);
    	}

        public override void write_fun_decl(Writer f, FunDecl fun_decl, bool main=false)
        {
            if (fun_decl == null)
                throw new ArgumentNullException(nameof(fun_decl));

            string optional_result;
            string fun_name, type_name;
            if (fun_decl.return_type == "")
                optional_result = "";
            else
            {
                type_names.TryGetValue(fun_decl.return_type, out type_name);
                optional_result = $" -> {type_name}";
            }
            fun_name = main ? "main" : fun_decl.name;
            f.write($"fn {fun_name}(){optional_result} {{\n");
            indent += 1;
            foreach (var statement in fun_decl.statements)
                write_statement(f, statement);
            indent -= 1;
            f.write("}\n");
    	}

        public override void write_var_decl(Writer f, VarDecl var_decl)
        {
            if (var_decl == null)
                throw new ArgumentNullException(nameof(var_decl));

            write_indent(f);
            f.write("let ");
            if (var_decl.mut)
                f.write("mut ");
            write_lval(f, var_decl.name);
            f.write(": i32");
            f.write(" = ");
            write_expr(f, var_decl.expr);
            f.write(";\n");
    	}

        public override void write_const_expr(Writer f, ConstExpr expr, bool needs_parens)
        {
            f.write(expr.val + "i32");
    	}

        public override void write_assignment(Writer f, Assignment assignment)
        {
            if (assignment == null)
                throw new ArgumentNullException(nameof(assignment));

            write_indent(f);
            write_lval(f, assignment.lval);
            f.write(" = ");
            write_expr(f, assignment.expr);
            f.write(";\n");
    	}

        public override void write_return(Writer f, Return statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            write_indent(f);
            write_expr(f, statement.expr);
            f.write("\n");
    	}

        public override void write_print(Writer f, Print statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            write_indent(f);
            f.write("println!(\"{}\", ");
            write_expr(f, statement.expr);
            f.write(")\n");
        }
    }

    class OCamlLang : Lang
    {
        public override string ext => "ml";

        public Dictionary<string, string> type_names => new Dictionary<string, string> {
            ["int"] = "int"
        };

        public override Dictionary<string, string> operators => new Dictionary<string, string> {
            ["&"] = "land",
            ["|"] = "lor",
            ["^"] = "lxor",
        };

        public override void write_program(Writer f, Program program)
        {
            if (program == null)
                throw new ArgumentNullException(nameof(program));

            foreach (var  fun_decl in program.functions)
            {
                write_fun_decl(f, fun_decl);
                f.write("\n");
            }
            write_fun_decl(f, program.main, true);
    	}

        public override void write_fun_decl(Writer f, FunDecl fun_decl, bool main=false)
        {
            if (fun_decl == null)
                throw new ArgumentNullException(nameof(fun_decl));

            string optional_result, type_name;
            if (fun_decl.return_type == "")
                optional_result = "";
            else
            {
                type_names.TryGetValue(fun_decl.return_type, out type_name);
                optional_result = $":{type_name} ";
            }
            var fun_name = main ? "main" : fun_decl.name;
            f.write($"let {fun_name} (){optional_result} = \n");
            indent += 1;
            foreach (var statement in fun_decl.statements)
                write_statement(f, statement);
            indent -= 1;
            if (main)
                f.write("\nmain ()");
    	}

        public override void write_var_decl(Writer f, VarDecl var_decl)
        {
            if (var_decl == null)
                throw new ArgumentNullException(nameof(var_decl));

            write_indent(f);
            f.write("let ");
            write_lval(f, var_decl.name);
            f.write(" = ");
            write_expr(f, var_decl.expr);
            f.write(" in \n");
    	}

        public override void write_assignment(Writer f, Assignment assignment)
        {
            if (assignment == null)
                throw new ArgumentNullException(nameof(assignment));

            write_indent(f);
            f.write("let ");
            write_lval(f, assignment.lval);
            f.write(" = ");
            write_expr(f, assignment.expr);
            f.write(" in \n");
    	}

        public override void write_return(Writer f, Return statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            write_indent(f);
            write_expr(f, statement.expr);
            f.write("\n");
    	}

        public override void write_print(Writer f, Print statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            write_indent(f);
            f.write("Printf.printf \"%i\\n\" (");
            write_expr(f, statement.expr);
            f.write(");;\n");
        }
    }

    class FSharpLang : Lang
    {
        public override string ext => "fs";

        public Dictionary<string, string> type_names => new Dictionary<string, string> {
            ["int"] = "int"
        };

        public override Dictionary<string, string> operators => new Dictionary<string, string> {
            ["&"] = "&&&",
            ["|"] = "|||",
            ["^"] = "^^^",
    	};

        public override void write_program(Writer f, Program program)
        {
            if (program == null)
                throw new ArgumentNullException(nameof(program));

            foreach (var  fun_decl in program.functions)
            {
                write_fun_decl(f, fun_decl);
                f.write("\n");
            }
            write_fun_decl(f, program.main, true);
    	}

        public override void write_fun_decl(Writer f, FunDecl fun_decl, bool main=false)
        {
            if (fun_decl == null)
                throw new ArgumentNullException(nameof(fun_decl));

            string optional_result, type_name;
            if (fun_decl.return_type == "")
                optional_result = "";
            else
            {
                type_names.TryGetValue(fun_decl.return_type, out type_name);
                optional_result = ":" + type_name + " ";
            }
            var fun_name = main ? "main" : fun_decl.name;
            f.write($"let {fun_name} (){optional_result} = \n");
            indent += 1;
            foreach (var statement in fun_decl.statements)
                write_statement(f, statement);
            indent -= 1;
            if (main)
                f.write("\nmain ()");
    	}

        public override void write_var_decl(Writer f, VarDecl var_decl)
        {
            if (var_decl == null)
                throw new ArgumentNullException(nameof(var_decl));

            write_indent(f);
            f.write("let ");
            write_lval(f, var_decl.name);
            f.write(" = ");
            write_expr(f, var_decl.expr);
            f.write(" in \n");
    	}

        public override void write_assignment(Writer f, Assignment assignment)
        {
            if (assignment == null)
                throw new ArgumentNullException(nameof(assignment));

            write_indent(f);
            f.write("let ");
            write_lval(f, assignment.lval);
            f.write(" = ");
            write_expr(f, assignment.expr);
            f.write(" in \n");
    	}

        public override void write_return(Writer f, Return statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            write_indent(f);
            write_expr(f, statement.expr);
            f.write("\n");
    	}

        public override void write_print(Writer f, Print statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            write_indent(f);
            f.write("printfn \"%i\\n\" (");
            write_expr(f, statement.expr);
            f.write(");;\n");
        }

    }

    class HaskellLang : Lang
    {
        public override string ext => "hs";

        public Dictionary<string, string> type_names => new Dictionary<string, string> {
            ["int"] = "Int64"
        };

        public override Dictionary<string, string> operators => new Dictionary<string, string> {
            ["&"] = "&&&",
            ["|"] = "|||",
            ["^"] = "^^^",
    	};

        public override void write_program(Writer f, Program program)
        {
            if (program == null)
                throw new ArgumentNullException(nameof(program));

            f.write("import GHC.Int\n"
                    + "import Data.Bits\n"
                    + "import Text.Printf\n\n");
            // removes ambiguity and forces 32-bit integers
            f.write(  "(&&&) :: Int32 -> Int32 -> Int32\n"
                    + "a &&& b = a .&. b\n"
                    + "(|||) :: Int32 -> Int32 -> Int32\n"
                    + "a ||| b = a .|. b\n"
                    + "(^^^) :: Int32 -> Int32 -> Int32\n"
                    + "a ^^^ b = a `xor` b\n\n");
            foreach (var fun_decl in program.functions)
            {
                write_fun_decl(f, fun_decl);
                f.write("\n");
            }
            write_fun_decl(f, program.main, true);
    	}

        public override void write_fun_decl(Writer f, FunDecl fun_decl, bool main=false)
        {
            if (fun_decl == null)
                throw new ArgumentNullException(nameof(fun_decl));

            string fund;
            if (main)
            {
                fund = "main = ";
                f.write("main :: IO ()\n");
                f.write(fund);
            }
            else
            {
                fund = $"{fun_decl.name} = ";
                f.write(fun_decl.name + " :: Int32\n");
                f.write(fund);
            }

            if (fun_decl.statements.Count > 0)
                write_statement(f, fun_decl.statements[0]);
            extra_indent = fund.Length;
            foreach (var statement in fun_decl.statements.Skip(1))
                write_statement(f, statement);
            extra_indent = 0;
    	}

        public override void write_var_decl(Writer f, VarDecl var_decl)
        {
            if (var_decl == null)
                throw new ArgumentNullException(nameof(var_decl));

            write_indent(f);
            f.write("let ");
            write_lval(f, var_decl.name);
            f.write(" = ");
            write_expr(f, var_decl.expr);
            f.write(" in \n");
    	}

        public override void write_assignment(Writer f, Assignment assignment)
        {
            if (assignment == null)
                throw new ArgumentNullException(nameof(assignment));

            if (assignment.expr is VarExpr && assignment.lval == assignment.expr.name)
                return;

            write_indent(f);
            f.write("let ");
            write_lval(f, assignment.lval);
            f.write(" = ");
            write_expr(f, assignment.expr);
            f.write(" in \n");
    	}

        public override void write_return(Writer f, Return statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            write_indent(f);
            write_expr(f, statement.expr);
            f.write("\n");
    	}

        public override void write_print(Writer f, Print statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            write_indent(f);
            f.write("printf \"%i\\n\" (");
            write_expr(f, statement.expr);
            f.write(")\n");
    	}

        public override void write_fun_call(Writer f, FunCallExpr expr, bool needs_parens)
        {
            f.write(expr.name);
        }
    }

    class CSharpLang : CppLang
    {
        public override string ext => "cs";

        public override void write_program(Writer f, Program program)
        {
            if (program == null)
                throw new ArgumentNullException(nameof(program));

            f.write("using System;\nnamespace CompilationSpeedTest\n{\n");
            indent += 1;
            write_indent(f);
            f.write("static class Program\n");
    	    write_indent(f);
            f.write("{\n");
            indent += 1;
            foreach (var  fun_decl in program.functions)
            {
                write_fun_decl(f, fun_decl);
                f.write("\n");
            }
            write_fun_decl(f, program.main, true);
            f.write("\n");
            indent -= 1;
            write_indent(f);
            f.write("}\n");
            indent -= 1;
            f.write("}\n");
    	}

        public override void write_fun_decl(Writer f, FunDecl fun_decl, bool main=false)
        {
            if (fun_decl == null)
                throw new ArgumentNullException(nameof(fun_decl));

            string optional_result, type_name;
            if (fun_decl.return_type == "")
                optional_result = "static int";
            else
            {
                type_names.TryGetValue(fun_decl.return_type, out type_name);
                optional_result = "static " + type_name;
            }
            var fun_name = main ? "Main" : fun_decl.name;
            write_indent(f);
            f.write($"{optional_result} {fun_name}()\n");
            write_indent(f);
            f.write("{\n");
            indent += 1;
            foreach (var statement in fun_decl.statements)
                write_statement(f, statement);
            if (main)
            {
                write_indent(f);
                f.write("return 0;\n");
            }
            indent -= 1;
            write_indent(f);
            f.write("}\n");
    	}

        public override void write_print(Writer f, Print statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            write_indent(f);
            f.write("Console.WriteLine(\"{i}\\n\", ");
            write_expr(f, statement.expr);
            f.write(");\n");
        }
    }

    class KotlinLang : Lang
    {
        public override string ext => "kt";

        public Dictionary<string, string> type_names => new Dictionary<string, string> {
            ["int"] = "Int",
        };

        public override Dictionary<string, string> operators => new Dictionary<string, string> {
            ["&"] = "and",
            ["|"] = "or",
            ["^"] = "xor"
        };

        public override void write_program(Writer f, Program program)
        {
            if (program == null)
                throw new ArgumentNullException(nameof(program));

            foreach (var  fun_decl in program.functions)
            {
                write_fun_decl(f, fun_decl);
                f.write("\n");
            }
            write_fun_decl(f, program.main, true);
    	}

        public override void write_fun_decl(Writer f, FunDecl fun_decl, bool main=false)
        {
            if (fun_decl == null)
                throw new ArgumentNullException(nameof(fun_decl));

            string optional_result, type_name;
            if (fun_decl.return_type == "")
                optional_result = "";
            else
            {
                type_names.TryGetValue(fun_decl.return_type, out type_name);
                optional_result = ": " + type_name;
            }
            var fun_name = main ? "main" : fun_decl.name;
            f.write($"fun {fun_name}(){optional_result} {{\n");
            indent += 1;
            foreach (var statement in fun_decl.statements)
                write_statement(f, statement);
            indent -= 1;
            f.write("}\n");
    	}

        public override void write_var_decl(Writer f, VarDecl var_decl)
        {
            if (var_decl == null)
                throw new ArgumentNullException(nameof(var_decl));

            write_indent(f);
            if (var_decl.mut)
                f.write("var ");
            else
                f.write("val ");
            write_lval(f, var_decl.name);
            f.write(": Int");
            f.write(" = ");
            write_expr(f, var_decl.expr);
            f.write("\n");
    	}

        public override void write_const_expr(Writer f, ConstExpr expr, bool needs_parens)
        {
            f.write(expr.val);
    	}

        public override void write_assignment(Writer f, Assignment assignment)
        {
            if (assignment == null)
                throw new ArgumentNullException(nameof(assignment));

            write_indent(f);
            write_lval(f, assignment.lval);
            f.write(" = ");
            write_expr(f, assignment.expr);
            f.write("\n");
    	}

        public override void write_return(Writer f, Return statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            write_indent(f);
            f.write("return ");
            write_expr(f, statement.expr);
            f.write("\n");
    	}

        public override void write_print(Writer f, Print statement)
        {
            if (statement == null)
                throw new ArgumentNullException(nameof(statement));

            write_indent(f);
            f.write("print(");
            write_expr(f, statement.expr);
            f.write(")\n");
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
                default:
                    throw new ArgumentOutOfRangeException(nameof(lang), $"{lang} is not supported by the code generation. So code it for us all to enjoy!");
            }
        }

        public string WriteLang(string lang, int num_funs)
        {
            var langwriter = GetLang(lang);
            var filename = $"test_{num_funs}.{langwriter.ext}";
            File.Delete(filename);
            using (var f = new StreamWriter(filename))
            {
                langwriter.write_program(
                    new Writer(f),
                    new Context().random_program(
                        num_funs: num_funs, max_statements_per_fun: 20));
            }

            return filename;
        }
    }
}