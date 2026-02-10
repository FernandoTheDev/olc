module middle.hir.cc;

import middle.hir.hir;
import frontend.types.type;
import std.stdio;
import std.conv;
import std.string;
import std.array;
import std.algorithm;
import std.format;

class CCodegen
{
    private Appender!string output;
    private Appender!string declarations;
    private Appender!string forwardDecls;
    private int indentLevel;
    private string[string] structDecls;
    private string[string] unionDecls;
    private string[string] enumDecls;
    private bool[string] generatedFunctions;
    
    this()
    {
        output = appender!string();
        declarations = appender!string();
        forwardDecls = appender!string();
        indentLevel = 0;
    }
    
    string generate(HirProgram program)
    {
        // Headers essenciais e comuns
        declarations.put("#include <stdint.h>\n");
        declarations.put("#include <stdbool.h>\n");
        declarations.put("#include <stddef.h>\n");
        
        declarations.put("\n#ifndef NULL\n#define NULL ((void*)0)\n#endif\n\n");
        
        // 2. Forward Declarations (Funções)
        foreach (node; program.globals)
        {
            if (HirFunction func = cast(HirFunction)node)
            {
                if (func.name == "main") continue;
                forwardDecls.put(generateFunctionDecl(func));
                forwardDecls.put(";\n");
            }
        }
        forwardDecls.put("\n");
        
        // 3. Definição de Tipos (Structs/Unions/Enums)
        foreach (node; program.globals)
        {
            if (auto sd = cast(HirStructDecl)node) generateStructDecl(sd);
            else if (auto ud = cast(HirUnionDecl)node) generateUnionDecl(ud);
            else if (auto ed = cast(HirEnumDecl)node) generateEnumDecl(ed);
        }
        
        // 4. Variáveis Globais
        foreach (node; program.globals)
        {
            if (auto varDecl = cast(HirVarDecl)node)
                if (varDecl.isGlobal) generateGlobalVar(varDecl);
        }
        
        // 5. Implementação de Funções
        foreach (node; program.globals)
        {
            if (auto func = cast(HirFunction)node)
                generateFunction(func);
        }
        
        return declarations.data ~ forwardDecls.data ~ output.data;
    }
    
    // --- Lógica de Geração de Tipos ---

    private void generateStructDecl(HirStructDecl node)
    {
        if (node.name in structDecls) return;
        declarations.put("struct " ~ node.name ~ " {\n");
        
        if (node.fieldNames.length == 0)
            declarations.put("    char _dummy;\n");
        else
        {
            foreach (i, fieldName; node.fieldNames)
            {
                declarations.put("    " ~ typeToC(node.fieldTypes[i]) ~ " " ~ sanitizeName(fieldName) ~ ";\n");
            }
        }
        declarations.put("};\n\n");
        declarations.put("typedef struct " ~ node.name ~ " " ~ node.name ~ ";\n\n");
        structDecls[node.name] = node.name;
    }
    
    private void generateUnionDecl(HirUnionDecl node)
    {
        if (node.name in unionDecls) return;
        declarations.put("union " ~ node.name ~ " {\n");
        
        if (node.fieldNames.length == 0)
            declarations.put("    char _dummy;\n");
        else
        {
            foreach (i, fieldName; node.fieldNames)
                declarations.put("    " ~ typeToC(node.fieldTypes[i]) ~ " " ~ sanitizeName(fieldName) ~ ";\n");
        }
        declarations.put("};\n\n");
        declarations.put("typedef union " ~ node.name ~ " " ~ node.name ~ ";\n\n");
        unionDecls[node.name] = node.name;
    }
    
    private void generateEnumDecl(HirEnumDecl node)
    {
        if (node.name in enumDecls) return;
        declarations.put("enum " ~ node.name ~ " {\n");
        
        foreach (i, fieldName; node.fieldNames)
        {
            declarations.put("    " ~ sanitizeName(fieldName));
            if (i < cast(int)node.fieldNames.length - 1) declarations.put(",");
            declarations.put("\n");
        }
        if (node.fieldNames.length == 0) declarations.put("    _DUMMY_ENUM = 0\n");
        
        declarations.put("};\n\n");
        declarations.put("typedef enum " ~ node.name ~ " " ~ node.name ~ ";\n\n");
        enumDecls[node.name] = node.name;
    }

    private void generateGlobalVar(HirVarDecl node)
    {
        declarations.put(typeToC(node.type) ~ " " ~ sanitizeName(node.name));
        declarations.put(" = " ~ (node.initValue ? generateExpr(node.initValue) : getZeroInitializer(node.type)));
        declarations.put(";\n");
    }
    
    private string generateFunctionDecl(HirFunction func)
    {
        auto res = appender!string();
        if (func.body !is null && shouldBeInline(func)) res.put("static inline ");
        
        res.put(typeToC(func.returnType) ~ " " ~ sanitizeName(func.name) ~ "(");
        
        if (func.argNames.length == 0 && !func.isVarArg)
            res.put("void");
        else
        {
            foreach (i, argName; func.argNames)
            {
                if (i > 0) res.put(", ");
                if (func.isVarArg && i == func.isVarArgAt) break; // Para processamento manual de varargs se necessário
                res.put(typeToC(func.argTypes[i]) ~ " " ~ sanitizeName(argName));
            }
            if (func.isVarArg)
                 res.put("...");
        }
        res.put(")");
        return res.data;
    }
    
    private bool shouldBeInline(HirFunction func)
    {
        if (func.body is null || func.name == "main") return false;
        // Heurística simples: inlina se tiver menos de 5 statements
        return func.body.stmts.length <= 5; 
    }
    
    private void generateFunction(HirFunction func)
    {
        if (func.name in generatedFunctions) return;
        generatedFunctions[func.name] = true;
        
        if (func.body !is null)
        {
            output.put("\n" ~ generateFunctionDecl(func) ~ "\n");
            generateBlock(func.body);
            output.put("\n");
        }
    }
    
    // --- Statements ---

    private void generateBlock(HirBlock block)
    {
        indent(); output.put("{\n");
        indentLevel++;
        foreach (stmt; block.stmts) generateStmt(stmt);
        indentLevel--;
        indent(); output.put("}\n");
    }
    
    private void generateStmt(HirNode node)
    {
        if (node is null) return;
        
        switch (node.kind)
        {
            case HirNodeKind.VarDecl:
                auto varDecl = cast(HirVarDecl)node;
                indent();
                if (auto arr = cast(ArrayType)varDecl.type)
                {
                    output.put(typeToC(arr.elementType) ~ " " ~ sanitizeName(varDecl.name));
                    output.put("[" ~ (arr.length > 0 ? to!string(arr.length) : "") ~ "]");
                }
                else
                {
                    output.put(typeToC(varDecl.type) ~ " " ~ sanitizeName(varDecl.name));
                }

                if (varDecl.initValue)
                {
                    output.put(" = ");
                    output.put(generateExpr(varDecl.initValue));
                }
                else
                {
                    // Inicialização segura com {0} funciona para primitivos e structs em C moderno
                    output.put(" = " ~ getZeroInitializer(varDecl.type));
                }
                output.put(";\n");
                break;
                
            case HirNodeKind.Store:
                auto store = cast(HirStore)node;
                indent();
                // AQUI ESTÁ A CORREÇÃO PRINCIPAL:
                output.put(generateLValue(store.ptr)); 
                output.put(" = ");
                output.put(generateExpr(store.value));
                output.put(";\n");
                break;
            
            case HirNodeKind.AssignDecl:
                auto assign = cast(HirAssignDecl)node;
                indent();
                // AQUI TAMBÉM:
                output.put(generateLValue(assign.target));
                output.put(" " ~ assign.op ~ " ");
                output.put(generateExpr(assign.value));
                output.put(";\n");
                break;
                
            case HirNodeKind.Return:
                auto ret = cast(HirReturn)node;
                indent();
                output.put("return");
                if (ret.value) output.put(" " ~ generateExpr(ret.value));
                output.put(";\n");
                break;
                
            case HirNodeKind.If:
                auto ifStmt = cast(HirIf)node;
                indent();
                output.put("if (" ~ generateExpr(ifStmt.condition) ~ ")\n");
                generateBlock(ifStmt.thenBlock);
                if (ifStmt.elseBlock && ifStmt.elseBlock.stmts.length > 0)
                {
                    indent(); output.put("else\n");
                    generateBlock(ifStmt.elseBlock);
                }
                break;
                
            case HirNodeKind.While:
                auto w = cast(HirWhile)node;
                indent();
                output.put("while (" ~ generateExpr(w.condition) ~ ")\n");
                generateBlock(w.body);
                break;
                
            case HirNodeKind.For:
                auto f = cast(HirFor)node;
                indent();
                output.put("for (");
                
                // Init pode ser decl ou stmt
                if (auto decl = cast(HirVarDecl)f.init_)
                {
                    output.put(typeToC(decl.type) ~ " " ~ sanitizeName(decl.name));
                    if (decl.initValue) output.put(" = " ~ generateExpr(decl.initValue));
                }
                else if (f.init_)
                {
                    // Se for Store/Assign, precisamos remover o ";\n" e indentação que generateStmt coloca
                    // Como generateStmt é void, vamos usar um hackzinho ou gerar manualmente
                    if (cast(HirStore)f.init_ || cast(HirAssignDecl)f.init_)
                    {
                         // Gera manual para não quebrar o for
                         if (auto asg = cast(HirAssignDecl)f.init_)
                            output.put(generateLValue(asg.target) ~ " " ~ asg.op ~ " " ~ generateExpr(asg.value));
                    }
                    else
                        output.put(generateExpr(f.init_));
                }
                output.put("; ");
                if (f.condition) output.put(generateExpr(f.condition));
                output.put("; ");
                if (f.increment) output.put(generateExpr(f.increment));
                output.put(")\n");
                generateBlock(f.body);
                break;
            
            case HirNodeKind.CallStmt:
                indent();
                output.put(generateExpr((cast(HirCallStmt)node).call));
                output.put(";\n");
                break;

            case HirNodeKind.Break:    indent(); output.put("break;\n"); break;
            case HirNodeKind.Continue: indent(); output.put("continue;\n"); break;
            
            case HirNodeKind.Switch:
                auto sw = cast(HirSwitch)node;
                indent(); output.put("switch (" ~ generateExpr(sw.condition) ~ ")\n");
                indent(); output.put("{\n");
                indentLevel++;
                foreach(c; sw.cases) {
                    if (c.isDefault) {
                        indent(); output.put("default:\n");
                    } else {
                        foreach(v; c.values) {
                            indent(); output.put("case " ~ generateExpr(v) ~ ":\n");
                        }
                    }
                    generateBlock(c.body); // O bloco já tem as chaves
                    // Dica: em C precisa de break explícito se o bloco não tiver,
                    // mas vou assumir que o lowering do D já tratou o fluxo.
                    indent(); output.put("break;\n"); 
                }
                indentLevel--;
                indent(); output.put("}\n");
                break;

            case HirNodeKind.Defer:
                indent(); output.put("// Defer ignored in C backend (requires logic lowering)\n");
                if (auto d = cast(HirDefer)node) generateStmt(d.value);
                break;
                
            default:
                indent(); output.put("// Unhandled Stmt: " ~ to!string(node.kind) ~ "\n");
        }
    }
    
    // --- L-Values (A Chave da Correção) ---
    
    // Este método é usado APENAS quando o nó está no lado esquerdo de uma atribuição
    private string generateLValue(HirNode node)
    {
        // Se o Lowering mandou um AddrOf(x), em C queremos atribuir em 'x', não '&x'
        if (auto addr = cast(HirAddrOf)node)
        {
            return sanitizeName(addr.varName);
        }
        
        // Se o Lowering mandou AddrOfComplex(arr[i]), queremos 'arr[i]'
        if (auto addrC = cast(HirAddrOfComplex)node)
        {
            return generateExpr(addrC.expr);
        }
        
        // Se o Lowering mandou um ponteiro puro (ex: parametro ptr), queremos escrever em *ptr
        // Então Store(ptr, 10) vira *ptr = 10;
        return "(*" ~ generateExpr(node) ~ ")";
    }

    // --- Expressões ---
    
    private string generateExpr(HirNode node)
    {
        if (node is null) return "NULL";
        
        auto res = appender!string();
        
        switch (node.kind)
        {
            case HirNodeKind.IntLit:    res.put(to!string((cast(HirIntLit)node).value)); break;
            case HirNodeKind.LongLit:   res.put(to!string((cast(HirLongLit)node).value) ~ "LL"); break;
            case HirNodeKind.FloatLit:  
                double v = (cast(HirFloatLit)node).value;
                res.put(format("%.17g", v));
                // Adiciona 'f' se for float explicitamente e não tiver ponto decimal
                if (cast(PrimitiveType)node.type && (cast(PrimitiveType)node.type).baseType == BaseType.Float)
                    res.put("f"); 
                else if (v == cast(long)v) res.put(".0"); // Garante que pareça float
                break;
                
            case HirNodeKind.BoolLit:   res.put((cast(HirBoolLit)node).value ? "true" : "false"); break;
            case HirNodeKind.CharLit:   res.put("'" ~ escapeChar((cast(HirCharLit)node).value) ~ "'"); break;
            case HirNodeKind.StringLit: res.put("\"" ~ escapeString((cast(HirStringLit)node).value) ~ "\""); break;
            case HirNodeKind.NullLit:   res.put("NULL"); break;
            
            case HirNodeKind.Load:      res.put(sanitizeName((cast(HirLoad)node).varName)); break;
            
            // Aqui AddrOf volta a gerar '&' porque estamos numa expressão (R-Value)
            case HirNodeKind.AddrOf:    
                res.put("&" ~ sanitizeName((cast(HirAddrOf)node).varName)); 
                break;
                
            case HirNodeKind.AddrOfComplex:
                res.put("&(" ~ generateExpr((cast(HirAddrOfComplex)node).expr) ~ ")");
                break;
                
            case HirNodeKind.Deref:
                res.put("(*" ~ generateExpr((cast(HirDeref)node).ptr) ~ ")");
                break;
                
            case HirNodeKind.Binary:
                auto b = cast(HirBinary)node;
                res.put("(" ~ generateExpr(b.left) ~ " " ~ b.op ~ " " ~ generateExpr(b.right) ~ ")");
                break;
                
            case HirNodeKind.Unary:
                HirUnary un = cast(HirUnary)node;
                if (un.op == "++_postfix" || un.op == "--_postfix")
                {
                    res.put(generateExpr(un.operand));
                    res.put(un.op == "++_postfix" ? "++" : "--");
                } else if (un.op == "++_prefix" || un.op == "--_prefix")
                {
                    res.put(un.op == "++_prefix" ? "++" : "--");
                    res.put(generateExpr(un.operand));
                } else
                {
                    res.put(un.op);
                    res.put(generateExpr(un.operand));
                }
                break;
                
            case HirNodeKind.Cast:
                auto c = cast(HirCast)node;
                res.put("((" ~ typeToC(c.targetType) ~ ")" ~ generateExpr(c.value) ~ ")");
                break;
                
            case HirNodeKind.CallExpr:
                auto call = cast(HirCallExpr)node;
                if (call.isRef) res.put("(*" ~ sanitizeName(call.funcName) ~ ")");
                else res.put(sanitizeName(call.funcName));
                
                res.put("(");
                foreach (i, arg; call.args)
                {
                    if (i > 0) res.put(", ");
                    res.put(generateExpr(arg));
                }
                res.put(")");
                break;
                
            case HirNodeKind.IndexAccess: // ptr[i]
                auto idx = cast(HirIndexAccess)node;
                res.put("(" ~ generateExpr(idx.target) ~ "[" ~ generateExpr(idx.index) ~ "])");
                break;
                
            case HirNodeKind.IndexExpr: // array[i]
                auto idx2 = cast(HirIndexExpr)node;
                res.put("(" ~ generateExpr(idx2.target) ~ "[" ~ generateExpr(idx2.index) ~ "])");
                break;
                
            case HirNodeKind.MemberAccess:
                auto mem = cast(HirMemberAccess)node;
                bool isPtr = cast(PointerType)mem.target.type !is null;
                bool isEnum = cast(EnumType)mem.target.type !is null;
                if (!isEnum)
                {
                    res.put(generateExpr(mem.target));
                    res.put(isPtr ? "->" : ".");
                }
                res.put(sanitizeName(mem.memberName));
                break;

            // COMPOUND LITERALS (C99) - A mágica para structs e arrays inline
            case HirNodeKind.ArrayLit:
                auto arr = cast(HirArrayLit)node;
                res.put("(" ~ typeToC(arr.type) ~ "){");
                foreach (i, e; arr.elements) {
                    if (i > 0) res.put(", ");
                    res.put(generateExpr(e));
                }
                res.put("}");
                break;
                
            case HirNodeKind.StructLit:
                auto sl = cast(HirStructLit)node;
                res.put("(" ~ typeToC(sl.type) ~ "){");
                foreach (i, v; sl.fieldValues) {
                    if (i > 0) res.put(", ");
                    res.put(generateExpr(v));
                }
                res.put("}");
                break;
                
            case HirNodeKind.AssignExpr:
                auto ae = cast(HirAssignExpr)node;
                res.put("(" ~ generateLValue(ae.assign.target) ~ " " ~ ae.assign.op ~ " " ~ 
                    generateExpr(ae.assign.value) ~ ")");
                break;
            
            case HirNodeKind.Ternary:
                auto t = cast(HirTernary)node;
                res.put("(" ~ generateExpr(t.condition) ~ " ? " ~ generateExpr(t.trueExpr) ~ " : " ~ 
                    generateExpr(t.falseExpr) ~ ")");
                break;
                
            case HirNodeKind.FunctionRef:
                 res.put(sanitizeName((cast(HirFunctionRef)node).name));
                 break;

            default:
                res.put("/* Expr Unknown: " ~ to!string(node.kind) ~ " */");
        }
        return res.data;
    }
    
    // --- Utilitários ---

    private string typeToC(Type type)
    {
        if (type is null) return "void";
        
        if (auto p = cast(PrimitiveType)type) {
            switch(p.baseType) {
                case BaseType.Void: return "void";
                case BaseType.Bool: return "bool";
                case BaseType.Char: return "char";
                case BaseType.Byte: return "int8_t";
                case BaseType.Ubyte: return "uint8_t";
                case BaseType.Short: return "int16_t";
                case BaseType.Ushort: return "uint16_t";
                case BaseType.Int: return "int32_t";
                case BaseType.Uint: return "uint32_t";
                case BaseType.Long: return "int64_t";
                case BaseType.Ulong: return "uint64_t";
                case BaseType.Float: return "float";
                case BaseType.Double: return "double";
                case BaseType.String: return "char*"; 
                default: return "void*";
            }
        }
        if (auto ptr = cast(PointerType)type) return typeToC(ptr.pointeeType) ~ "*";
        if (ArrayType arr = cast(ArrayType)type) return typeToC(arr.elementType) ~ format("[%d]", arr.length);
        
        // Em C, struct params precisam da palavra struct ou typedef. 
        // Como fizemos typedefs no início, só o nome basta.
        if (auto s = cast(StructType)type) return sanitizeName(s.mangledName.length ? s.mangledName : s.name);
        if (auto u = cast(UnionType)type) return sanitizeName(u.mangledName.length ? u.mangledName : u.name);
        if (auto e = cast(EnumType)type) return sanitizeName(e.name);
        
        if (auto f = cast(FunctionType)type) {
            // Ponteiro de função em C é chato de gerar inline sem typedef, 
            // mas vamos tentar: Ret (*)(Args)
            string ret = typeToC(f.returnType);
            string args = "";
            foreach(i, a; f.paramTypes) {
                if (i>0) args ~= ", ";
                args ~= typeToC(a);
            }
            if (f.paramTypes.length == 0) args = "void";
            return ret ~ " (*)" ~ "(" ~ args ~ ")";
        }
        
        return "void";
    }
    
    private string getZeroInitializer(Type type)
    {
        if (type is null) return "0";
        if (cast(StructType)type || cast(UnionType)type || cast(ArrayType)type) return "{0}";
        if (cast(PointerType)type) return "NULL";
        if (auto p = cast(PrimitiveType)type) {
            if (p.baseType == BaseType.Bool) return "false";
            if (p.baseType == BaseType.Float) return "0.0f";
        }
        return "0";
    }
    
    private string sanitizeName(string name)
    {
        if (name == "main") return "main"; // Nunca mexer no main
        
        // Palavras reservadas do C
        static string[] keywords = [
            "auto", "break", "case", "char", "const", "continue", "default", "do", 
            "double", "else", "enum", "extern", "float", "for", "goto", "if", 
            "int", "long", "register", "return", "short", "signed", "sizeof", "static", 
            "struct", "switch", "typedef", "union", "unsigned", "void", "volatile", "while"
        ];
        
        if (keywords.canFind(name)) return name ~ "_";
        
        auto res = appender!string();
        foreach(c; name) {
            if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_')
                res.put(c);
            else
                res.put('_');
        }
        string s = res.data;
        if (s.length > 0 && s[0] >= '0' && s[0] <= '9') return "_" ~ s;
        return s;
    }
    
    private string escapeChar(char c) {
        if (c == '\'') return "\\'";
        if (c == '\\') return "\\\\";
        if (c == '\n') return "\\n";
        if (c == '\r') return "\\r";
        if (c == '\t') return "\\t";
        if (c == '\0') return "\\0";
        return [c];
    }
    
    private string escapeString(string s) {
        string r = "";
        foreach(c; s) {
            if (c == '"') r ~= "\\\"";
            else r ~= escapeChar(c);
        }
        return r;
    }

    private void indent() { foreach(i; 0..indentLevel) output.put("    "); }
}
