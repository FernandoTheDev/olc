module backend.vm.codegen;

import std.stdio, std.conv, std.string, std.algorithm;
import backend.vm.builder, backend.vm.virtual_machine;
import middle.hir.hir;
import frontend.types.type;

class VMBackend {
    CodeBuilder builder;
    HirProgram program;
    
    // Mapeia nomes de variáveis para índices de locals
    int[string] locals;
    int localCounter = 0;
    
    // Mapeia nomes de funções para labels e metadados
    string[string] functionLabels;
    FunctionMetadata[string] functionMeta;
    
    // Pool de constantes (cache para evitar duplicação)
    ushort[string] constantCache;
    
    // Contexto atual
    HirFunction currentFunc;
    
    // Contadores para labels únicos
    int ifCounter = 0;
    int whileCounter = 0;
    int forCounter = 0;
    
    // Loop stack para break/continue
    struct LoopContext {
        string continueLabel;
        string breakLabel;
    }
    LoopContext[] loopStack;
    
    struct FunctionMetadata {
        int numParams;
        int numLocals;
    }
    
    this(uint initialCapacity = 1024) {
        builder = CodeBuilder(initialCapacity);
    }
    
    void generate(HirProgram prog) 
    {
        program = prog;

        builder.jmpLabel("main");
        
        // Primeira passagem: coletar metadados das funções
        foreach (node; program.globals) {
            if (auto func = cast(HirFunction) node) {
                collectFunctionMetadata(func);
            }
        }
        
        // Segunda passagem: processar globals
        bool hasGlobalCode = false;
        foreach (node; program.globals) {
            if (auto varDecl = cast(HirVarDecl) node) {
                if (varDecl.isGlobal && varDecl.initValue !is null) {
                    generateGlobalInit(varDecl);
                    hasGlobalCode = true;
                }
            }
        }
        
        // Terceira passagem: gerar funções
        bool hasMain = false;
        foreach (node; program.globals) {
            if (auto func = cast(HirFunction) node) {
                generateFunction(func);
                if (func.name == "main") {
                    hasMain = true;
                }
            }
        }
        
        // Se não tem main, criar entry point para código global
        if (!hasMain && hasGlobalCode) {
            generateGlobalEntry();
        }
    }
    
    void collectFunctionMetadata(HirFunction func) {
        FunctionMetadata meta;
        meta.numParams = cast(int)func.argNames.length;
        
        // Contar locals: precisamos fazer uma análise rápida
        // Por enquanto, usar estimativa conservadora
        meta.numLocals = meta.numParams + 20; // Params + espaço para variáveis locais
        
        functionMeta[func.name] = meta;
        functionLabels[func.name] = func.name;
    }
    
    void generateGlobalInit(HirVarDecl decl)
    {
        // Globals são tratados como constantes na pool
        if (auto intLit = cast(HirIntLit) decl.initValue) {
            ushort idx = builder.constI32(intLit.value);
            constantCache[decl.name] = idx;
        } else if (auto longLit = cast(HirLongLit) decl.initValue) {
            ushort idx = builder.constI64(longLit.value);
            constantCache[decl.name] = idx;
        } else if (auto floatLit = cast(HirFloatLit) decl.initValue) {
            ushort idx = builder.constF64(floatLit.value);
            constantCache[decl.name] = idx;
        } else if (auto strLit = cast(HirStringLit) decl.initValue) {
            ushort idx = builder.constStr(strLit.value.toStringz());
            constantCache[decl.name] = idx;
        }
        // Outros tipos de inicialização global podem ser adicionados aqui
    }
    
    void generateGlobalEntry() {
        builder.label("main");
        
        // Executar código global
        foreach (node; program.globals) {
            if (cast(HirFunction) node) continue;
            if (auto varDecl = cast(HirVarDecl) node) {
                if (!varDecl.isGlobal) continue;
            }
            generateStmt(node);
        }
        
        builder.halt();
    }
    
    void generateFunction(HirFunction func) {
        currentFunc = func;
        locals.clear();
        localCounter = 0;
        loopStack = [];
        ifCounter = 0;
        whileCounter = 0;
        forCounter = 0;
        
        // Label da função
        string label = func.name;
        builder.label(label.toStringz());
        
        // Mapear argumentos para locals (slots 0, 1, 2...)
        foreach (i, argName; func.argNames) {
            locals[argName] = cast(int)i;
            localCounter++;
        }

        // hack
        if (label == "___print")
        {
            builder.loadLocal(0).print().ret();
            return;
        }
        
        // Gerar corpo
        if (func.body !is null) {
            generateBlock(func.body);
        }
        
        // Garantir return implícito se não tiver
        if ((func.body is null || func.body.stmts.length == 0 || 
            (func.body.stmts[$-1].kind != HirNodeKind.Return)) && label != "main") 
            {
            
            // Se retorna void, apenas ret
            if (auto primType = cast(PrimitiveType) func.returnType) {
                if (primType.baseType == BaseType.Void) {
                    builder.ret();
                } else {
                    // Retornar 0 como default
                    builder.pushI32(0);
                    builder.ret();
                }
            } else {
                builder.ret();
            }
        }
    }
    
    void generateBlock(HirBlock block) {
        if (block is null) return;
        
        foreach (stmt; block.stmts) {
            generateStmt(stmt);
        }
    }
    
    void generateStmt(HirNode stmt) {
        if (stmt is null) return;
        
        switch (stmt.kind) {
            case HirNodeKind.VarDecl:
                HirVarDecl varDecl = cast(HirVarDecl) stmt;
                
                // Se é global, já foi processada
                // if (varDecl.isGlobal) break;
                
                // Alocar local
                int localIdx = localCounter++;
                locals[varDecl.name] = localIdx;
                
                // Se tem inicializador, avaliar e armazenar
                if (varDecl.initValue !is null) {
                    generateExpr(varDecl.initValue);
                    builder.storeLocal(cast(ubyte)localIdx);
                } else {
                    // Inicializar com 0
                    builder.pushI32(0);
                    builder.storeLocal(cast(ubyte)localIdx);
                }
                break;
                
            case HirNodeKind.AssignDecl:
                auto assign = cast(HirAssignDecl) stmt;
                
                // Tratar operadores compostos (+=, -=, etc)
                if (assign.op != "=") {
                    // x += 5 -> x = x + 5
                    generateExpr(assign.target); // Load x
                    generateExpr(assign.value);  // Load 5
                    
                    string baseOp = assign.op[0..1];
                    emitBinaryOp(baseOp);
                    
                    // Armazenar resultado
                    storeToLValue(assign.target);
                } else {
                    // Atribuição simples
                    generateExpr(assign.value);
                    storeToLValue(assign.target);
                }
                break;
                
            case HirNodeKind.Return:
                auto ret = cast(HirReturn) stmt;
                if (ret.value !is null) {
                    generateExpr(ret.value);
                }
                builder.ret();
                break;
                
            case HirNodeKind.If:
                generateIf(cast(HirIf) stmt);
                break;
                
            case HirNodeKind.While:
                generateWhile(cast(HirWhile) stmt);
                break;
                
            case HirNodeKind.For:
                generateFor(cast(HirFor) stmt);
                break;
                
            case HirNodeKind.Break:
                if (loopStack.length > 0) {
                    builder.jmpLabel(loopStack[$-1].breakLabel.toStringz());
                } else {
                    writeln("ERROR: break outside loop");
                }
                break;
                
            case HirNodeKind.Continue:
                if (loopStack.length > 0) {
                    builder.jmpLabel(loopStack[$-1].continueLabel.toStringz());
                } else {
                    writeln("ERROR: continue outside loop");
                }
                break;
                
            case HirNodeKind.CallStmt:
                auto call = cast(HirCallStmt) stmt;
                generateExpr(call.call);
                builder.pop(); // Descartar retorno
                break;
                
            case HirNodeKind.Block:
                generateBlock(cast(HirBlock) stmt);
                break;
                
            default:
                // Se for expressão válida, avaliar e descartar
                if (isExpression(stmt)) {
                    generateExpr(stmt);
                    builder.pop();
                }
                break;
        }
    }
    
    void generateIf(HirIf ifStmt) {
        int id = ifCounter++;
        string elseLabel = "if_else_" ~ to!string(id);
        string endLabel = "if_end_" ~ to!string(id);
        
        // Avaliar condição
        generateExpr(ifStmt.condition);
        
        // Se falso, pula para else (ou end se não há else)
        if (ifStmt.elseBlock !is null) {
            builder.jzLabel(elseLabel.toStringz());
        } else {
            builder.jzLabel(endLabel.toStringz());
        }
        
        // Then block
        generateBlock(ifStmt.thenBlock);
        
        if (ifStmt.elseBlock !is null) {
            builder.jmpLabel(endLabel.toStringz());
            
            // Else block
            builder.label(elseLabel.toStringz());
            generateBlock(ifStmt.elseBlock);
        }
        
        builder.label(endLabel.toStringz());
    }
    
    void generateWhile(HirWhile whileStmt) {
        int id = whileCounter++;
        string condLabel = "while_cond_" ~ to!string(id);
        string bodyLabel = "while_body_" ~ to!string(id);
        string endLabel = "while_end_" ~ to!string(id);
        
        // Push loop context
        loopStack ~= LoopContext(condLabel, endLabel);
        
        // Condition label
        builder.label(condLabel.toStringz());
        generateExpr(whileStmt.condition);
        builder.jzLabel(endLabel.toStringz());
        
        // Body
        builder.label(bodyLabel.toStringz());
        generateBlock(whileStmt.body);
        builder.jmpLabel(condLabel.toStringz());
        
        // End
        builder.label(endLabel.toStringz());
        
        // Pop loop context
        loopStack.length--;
    }
    
    void generateFor(HirFor forStmt) {
        int id = forCounter++;
        string condLabel = "for_cond_" ~ to!string(id);
        string bodyLabel = "for_body_" ~ to!string(id);
        string incLabel = "for_inc_" ~ to!string(id);
        string endLabel = "for_end_" ~ to!string(id);
        
        // Init
        if (forStmt.init_ !is null) {
            generateStmt(forStmt.init_);
        }
        
        // Push loop context (continue vai para increment)
        loopStack ~= LoopContext(incLabel, endLabel);
        
        // Condition
        builder.label(condLabel.toStringz());
        if (forStmt.condition !is null) {
            generateExpr(forStmt.condition);
            builder.jzLabel(endLabel.toStringz());
        }
        
        // Body
        builder.label(bodyLabel.toStringz());
        generateBlock(forStmt.body);
        
        // Increment
        builder.label(incLabel.toStringz());
        if (forStmt.increment !is null) {
            if (isExpression(forStmt.increment)) {
                generateExpr(forStmt.increment);
                builder.pop(); // Descartar resultado
            } else {
                generateStmt(forStmt.increment);
            }
        }
        
        builder.jmpLabel(condLabel.toStringz());
        
        // End
        builder.label(endLabel.toStringz());
        
        // Pop loop context
        loopStack.length--;
    }
    
    void generateExpr(HirNode expr) {
        if (expr is null) {
            builder.pushI32(0);
            return;
        }
        
        switch (expr.kind) {
            case HirNodeKind.IntLit:
                auto lit = cast(HirIntLit) expr;
                builder.pushI32(lit.value);
                break;
                
            case HirNodeKind.LongLit:
                auto lit = cast(HirLongLit) expr;
                ushort idx = builder.constI64(lit.value);
                builder.push(idx);
                break;
                
            case HirNodeKind.FloatLit:
                auto lit = cast(HirFloatLit) expr;
                builder.pushF64(lit.value);
                break;
                
            case HirNodeKind.StringLit:
                auto lit = cast(HirStringLit) expr;
                builder.pushStr(lit.value.toStringz());
                break;
                
            case HirNodeKind.CharLit:
                auto lit = cast(HirCharLit) expr;
                builder.pushI32(cast(int)lit.value);
                break;
                
            case HirNodeKind.BoolLit:
                auto lit = cast(HirBoolLit) expr;
                builder.pushI32(lit.value ? 1 : 0);
                break;
                
            case HirNodeKind.NullLit:
                builder.pushI32(0);
                break;
                
            case HirNodeKind.Load:
                auto load = cast(HirLoad) expr;
                
                // Verificar se é global
                if (load.varName in constantCache) {
                    builder.push(constantCache[load.varName]);
                } else if (load.varName in locals) {
                    int localIdx = locals[load.varName];
                    builder.loadLocal(cast(ubyte)localIdx);
                } else {
                    writeln("ERROR: Undefined variable: ", load.varName);
                    builder.pushI32(0);
                }
                break;
                
            case HirNodeKind.Binary:
                auto bin = cast(HirBinary) expr;
                
                // Tratar short-circuit para && e ||
                if (bin.op == "&&") {
                    int id = ifCounter++;
                    string falseLabel = "and_false_" ~ to!string(id);
                    string endLabel = "and_end_" ~ to!string(id);
                    
                    generateExpr(bin.left);
                    builder.dup(); // Duplicar para teste
                    builder.jzLabel(falseLabel.toStringz());
                    
                    // Left é true, avaliar right
                    builder.pop(); // Remover left
                    generateExpr(bin.right);
                    builder.jmpLabel(endLabel.toStringz());
                    
                    // Left é false, resultado é false
                    builder.label(falseLabel.toStringz());
                    // Já tem false no topo
                    
                    builder.label(endLabel.toStringz());
                } else if (bin.op == "||") {
                    int id = ifCounter++;
                    string trueLabel = "or_true_" ~ to!string(id);
                    string endLabel = "or_end_" ~ to!string(id);
                    
                    generateExpr(bin.left);
                    builder.dup();
                    builder.jnzLabel(trueLabel.toStringz());
                    
                    // Left é false, avaliar right
                    builder.pop();
                    generateExpr(bin.right);
                    builder.jmpLabel(endLabel.toStringz());
                    
                    // Left é true, resultado é true
                    builder.label(trueLabel.toStringz());
                    // Já tem true no topo
                    
                    builder.label(endLabel.toStringz());
                } else {
                    // Operadores normais
                    generateExpr(bin.left);
                    generateExpr(bin.right);
                    emitBinaryOp(bin.op);
                }
                break;
                
            case HirNodeKind.Unary:
                auto un = cast(HirUnary) expr;
                
                if (un.op == "-") {
                    // Negar: 0 - x
                    builder.pushI32(0);
                    generateExpr(un.operand);
                    builder.sub();
                } else if (un.op == "!") {
                    // NOT lógico: x == 0
                    generateExpr(un.operand);
                    builder.pushI32(0);
                    builder.eq();
                } else if (un.op == "~") {
                    // NOT bitwise: -1 ^ x (ou XOR com todos bits 1)
                    generateExpr(un.operand);
                    builder.pushI32(-1);
                    // builder.xor(); // Assumindo que você adicione isso à VM
                    writeln("WARNING: Bitwise NOT not fully implemented in VM");
                } else if (un.op == "++_postfix" || un.op == "--_postfix") {
                    // x++ -> (temp = x, x = x + 1, temp)
                    auto load = cast(HirLoad) un.operand;
                    if (load !is null) {
                        // Load valor atual
                        int localIdx = locals[load.varName];
                        builder.loadLocal(cast(ubyte)localIdx);
                        builder.dup(); // Duplicar para retornar valor original
                        
                        // Incrementar/Decrementar
                        builder.pushI32(1);
                        if (un.op == "++_postfix") {
                            builder.add();
                        } else {
                            builder.sub();
                        }
                        
                        // Armazenar novo valor
                        builder.storeLocal(cast(ubyte)localIdx);
                        
                        // Valor original continua no topo
                    }
                } else {
                    generateExpr(un.operand);
                }
                break;
                
            case HirNodeKind.CallExpr:
                auto call = cast(HirCallExpr) expr;
                
                // Push argumentos na ordem
                foreach (arg; call.args) {
                    generateExpr(arg);
                }
                
                // Determinar numLocals da função alvo
                ubyte numLocals = 20; // Default
                if (call.funcName in functionMeta) {
                    numLocals = cast(ubyte)functionMeta[call.funcName].numLocals;
                }
                
                // Call
                builder.callLabel(call.funcName.toStringz(), numLocals);
                break;
                
            case HirNodeKind.Cast:
                auto cast_ = cast(HirCast) expr;
                // Para VM simples, cast é no-op (apenas avaliar valor)
                generateExpr(cast_.value);
                break;
                
            case HirNodeKind.Ternary:
                auto tern = cast(HirTernary) expr;
                int id = ifCounter++;
                string falseLabel = "tern_false_" ~ to!string(id);
                string endLabel = "tern_end_" ~ to!string(id);
                
                generateExpr(tern.condition);
                builder.jzLabel(falseLabel.toStringz());
                
                // True branch
                generateExpr(tern.trueExpr);
                builder.jmpLabel(endLabel.toStringz());
                
                // False branch
                builder.label(falseLabel.toStringz());
                generateExpr(tern.falseExpr);
                
                builder.label(endLabel.toStringz());
                break;
                
            case HirNodeKind.AssignExpr:
                auto assignExpr = cast(HirAssignExpr) expr;
                // Executar atribuição
                generateStmt(assignExpr.assign);
                // Retornar o valor atribuído
                generateExpr(assignExpr.assign.target);
                break;
                
            default:
                writeln("WARNING: Unimplemented expr kind: ", expr.kind);
                builder.pushI32(0);
                break;
        }
    }
    
    void emitBinaryOp(string op) {
        switch (op) {
            case "+": builder.add(); break;
            case "-": builder.sub(); break;
            case "*": builder.mul(); break;
            case "/": builder.div(); break;
            case "%": builder.mod(); break;
            case "==": builder.eq(); break;
            case "!=": builder.neq(); break;
            case "<": builder.lt(); break;
            case ">": builder.gt(); break;
            case "<=": builder.le(); break;
            case ">=": builder.ge(); break;
            default:
                writeln("WARNING: Unknown binary op: ", op);
                break;
        }
    }
    
    void storeToLValue(HirNode lvalue) {
        if (auto load = cast(HirLoad) lvalue) {
            if (load.varName in locals) {
                int localIdx = locals[load.varName];
                builder.storeLocal(cast(ubyte)localIdx);
            } else {
                writeln("ERROR: Cannot store to undefined variable: ", load.varName);
            }
        } else if (auto idx = cast(HirIndexExpr) lvalue) {
            // TODO: Array assignment
            writeln("WARNING: Array assignment not yet implemented in VM");
            builder.pop(); // Descartar valor por enquanto
        } else if (auto member = cast(HirMemberAccess) lvalue) {
            // TODO: Struct field assignment
            writeln("WARNING: Struct member assignment not yet implemented in VM");
            builder.pop();
        } else {
            writeln("ERROR: Invalid lvalue for assignment");
            builder.pop();
        }
    }
    
    bool isExpression(HirNode node) {
        return node.kind >= HirNodeKind.IntLit && node.kind <= HirNodeKind.AddrOfComplex;
    }
    
    VM buildVM(uint startPc = 0) {
        return builder.buildVM(startPc);
    }
    
    void compileToFile(string filename) {
        import core.stdc.stdio, std.file;
        import std.string : fromStringz;
        
        builder.build();
        
        auto file = fopen(filename.toStringz(), "wb".ptr);
        if (file is null) {
            writeln("ERROR: Could not open file for writing: ", filename);
            return;
        }
        
        // Escrever cabeçalho mágico
        fwrite("OrnVMv1\0".ptr, 1, 8, file);
        
        // Escrever tamanhos
        uint[2] sizes = [builder.programSize, builder.constantsSize];
        fwrite(sizes.ptr, uint.sizeof, 2, file);
        
        // Escrever programa
        fwrite(builder.program, uint.sizeof, builder.programSize, file);
        
        // Escrever constantes
        for (uint i = 0; i < builder.constantsSize; i++) {
            auto constant = builder.constants[i];
            fwrite(&constant.type, 1, 1, file);
            
            if (constant.type == VType.Str) {
                string str = constant.value.str.fromStringz().idup;
                uint strLen = cast(uint)str.length;
                fwrite(&strLen, uint.sizeof, 1, file);
                fwrite(str.ptr, 1, strLen, file);
            } else {
                fwrite(&constant.value, UValue.sizeof, 1, file);
            }
        }
        
        fclose(file);
        writefln("Successfully compiled to %s", filename);
        writefln("  Program size: %d instructions", builder.programSize);
        writefln("  Constants: %d", builder.constantsSize);
        writefln("  Functions: %d", functionMeta.length);
    }
}
