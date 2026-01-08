module middle.mir.lowering;

import middle.hir.hir;
import middle.mir.mir;
import frontend;
import std.conv, std.stdio : writeln;
import std.range : retro;

class HirToMir {
    MirFunction currentFunc;
    MirBasicBlock currentBlock;
    MirValue[string] varMap;
    MirProgram mirProgram;
    TypeRegistry registry;

    this(TypeRegistry registry) 
    {
        this.registry = registry;
    }

    struct DeferScope {
        HirNode[] stmts;
    }

    DeferScope[] deferStack;

    struct LoopContext {
        string continueLabel; // Para onde 'continue' vai
        string breakLabel;    // Para onde 'break' vai
        size_t deferDepth;    // Quantos escopos de defer existiam antes de entrar no loop
    }

    LoopContext[] loopStack;

    // MirProgram lower(HirProgram hir)
    // {
    //     this.mirProgram = new MirProgram(); 

    //     currentFunc = new MirFunction();
    //     currentFunc.name = "global";
        
    //     // processa outras declarações globais primeiro
    //     foreach (nd; hir.globals)
    //         if (auto varDecl = cast(HirVarDecl) nd)
    //             lowerStmt(varDecl);

    //     foreach (nd; hir.globals)
    //         if (auto ver = cast(HirVersion) nd)
    //             lowerStmt(ver);
        
    //     foreach (nd; hir.globals)
    //         if (auto func = cast(HirFunction) nd)
    //             mirProgram.functions ~= lowerFunc(func);

    //     return mirProgram;
    // }
    MirProgram lower(HirProgram hir)
    {
        this.mirProgram = new MirProgram(); 

        // Verifica se existe função main
        bool hasMainFunction = false;
        foreach (nd; hir.globals)
            if (auto func = cast(HirFunction) nd)
                hasMainFunction = func.name == "main";

        // Só cria função global executável se NÃO houver main
        if (!hasMainFunction)
        {
            // Verifica se há código global para processar
            bool hasGlobalCode = false;
            foreach (nd; hir.globals)
            {
                if (cast(HirFunction) nd) continue;
                hasGlobalCode = true;
                break;
            }

            currentFunc = new MirFunction();
            currentFunc.name = "main";
            currentFunc.returnType = new PrimitiveType(BaseType.Void);
            createAndSwitchBlock("entry");

            if (hasGlobalCode)
                // Processa TODOS os nodes globais que não sejam funções
                foreach (nd; hir.globals)
                {
                    if (cast(HirFunction) nd) continue;
                    lowerStmt(nd);
                }

            // Adiciona terminador se necessário
            if (!blockHasTerminator(currentBlock))
                emit(new MirInstr(MirOp.Ret));

            // Adiciona a função global ao programa
            mirProgram.functions ~= currentFunc;
        }
        // Se tem main, processa apenas variáveis globais (declarações)
        // IMPORTANTE: Não criamos currentFunc aqui, o lowerStmt de VarDecl
        // já detecta isGlobal e adiciona direto ao mirProgram.globals

        // Processa funções definidas pelo usuário
        foreach (nd; hir.globals)
            if (auto func = cast(HirFunction) nd)
                mirProgram.functions ~= lowerFunc(func);

        return mirProgram;
    }

private:

    void pushDeferScope() {
        deferStack ~= DeferScope([]);
    }

    void popDeferScope() {
        if (deferStack.length > 0)
            deferStack.length--;
    }

    void addDefer(HirNode stmt) {
        if (deferStack.length > 0)
            deferStack[$-1].stmts ~= stmt;
    }

    // Executa defers do escopo atual (ao chegar no '}')
    void emitDefersForTopScope() {
        if (deferStack.length == 0) return;
        // Ordem reversa (LIFO)
        foreach (stmt; deferStack[$-1].stmts.retro) {
            lowerStmt(stmt);
        }
    }

    // Executa TODOS os defers (para 'return')
    void emitAllDefers() {
        foreach (scope_; deferStack.retro)
            foreach (stmt; scope_.stmts.retro)
                lowerStmt(stmt);
    }

    // Executa defers até atingir uma certa profundidade (para break/continue)
    void emitDefersDownTo(size_t targetDepth) {
        // Itera de cima para baixo na pilha até chegar no targetDepth
        // Ex: Stack size 5, target 3 -> processa indices 4 e 3.
        long currentIdx = cast(long)deferStack.length - 1;
        while (currentIdx >= cast(long)targetDepth) {
            foreach (stmt; deferStack[currentIdx].stmts.retro)
                lowerStmt(stmt);
            currentIdx--;
        }
    }

    void clearMap()
    {
        foreach (string idx, MirValue val; varMap)
            if (!val.isGlobal)
                varMap.remove(idx);           
    }

    MirFunction lowerFunc(HirFunction hirFunc)
    {
        currentFunc = new MirFunction();
        currentFunc.name = hirFunc.name;
        currentFunc.returnType = hirFunc.returnType;
        
        // Copia e prepara a assinatura
        currentFunc.paramTypes = hirFunc.argTypes.dup;
        currentFunc.isVarArg = hirFunc.isVarArg;
        currentFunc.isOrnVarArgs = hirFunc.isOrnVarArgs;
        currentFunc.isVarArgAt = hirFunc.isVarArgAt;
        clearMap();
        deferStack = [];
        loopStack = [];

        // 1. Injeção do tipo 'int' na assinatura MIR (se for VarArg nativa)
        if (hirFunc.isVarArg && !hirFunc.isOrnVarArgs && hirFunc.body !is null)
        {
            int idx = hirFunc.isVarArgAt;   
            auto countType = new PrimitiveType(BaseType.Int);
            
            // Insere o tipo int na posição correta
            currentFunc.paramTypes = currentFunc.paramTypes[0..idx] 
                                   ~ countType
                                   ~ currentFunc.paramTypes[idx..$];
        }

        if (hirFunc.body is null) return currentFunc;

        createAndSwitchBlock("entry");

        foreach (i, argName; hirFunc.argNames)
        {
            auto type = hirFunc.argTypes[i];
            if (type is null) continue;
            int llvmArgIndex = cast(int)i;
            
            // Se já passamos do ponto de injeção do _vacount,
            // pulamos um slot (o slot que o _vacount ocupou).
            if (hirFunc.isVarArg && hirFunc.body !is null && i >= hirFunc.isVarArgAt)
                llvmArgIndex++; 

            auto ptrReg = currentFunc.newReg(new PointerType(type)); 
            emit(new MirInstr(MirOp.Alloca, ptrReg));

            auto argVal = MirValue.argument(llvmArgIndex, type);
            emitStore(argVal, ptrReg);
            varMap[argName] = ptrReg;
        }

        if (hirFunc.isVarArg && hirFunc.body !is null)
        {
            auto intType = new PrimitiveType(BaseType.Int);
            auto ptrReg = currentFunc.newReg(new PointerType(intType));
            emit(new MirInstr(MirOp.Alloca, ptrReg));
            varMap["_vacount"] = ptrReg;
        }

        lowerBlock(hirFunc.body);
        if (!blockHasTerminator(currentBlock))
            emit(new MirInstr(MirOp.Ret));

        return currentFunc;
    }

    void createAndSwitchBlock(string prefix) 
    {
        string name = currentFunc.uniqueBlockName(prefix);
        auto bb = new MirBasicBlock(name);
        currentFunc.blocks ~= bb;
        currentBlock = bb;
    }

    void lowerBlock(HirBlock block) 
    {
        if (block is null)
            return;
        
        pushDeferScope();

        foreach (stmt; block.stmts)
        {
            if (stmt is null)
                continue;
                
            // Se for defer, guarda pra depois
            if (stmt.kind == HirNodeKind.Defer) {
                // Assume que HirDefer tem um campo 'value' ou 'stmt' que é o HirNode a ser executado
                // Baseado no seu código anterior de lowering:
                addDefer((cast(HirDefer)stmt).value);
                continue;
            }

            lowerStmt(stmt);
            
            // Se encontrou um terminador (return, break, continue), paramos
            // O lowerStmt desses caras já cuidou de emitir os defers apropriados
            if (blockHasTerminator(currentBlock)) break;
        }

        // 2. Se o fluxo chegou ao fim do bloco naturalmente (sem return/break),
        // executamos os defers deste escopo agora.
        if (!blockHasTerminator(currentBlock))
            emitDefersForTopScope();

        // 3. Fecha escopo
        popDeferScope();
    }

    void emitArrayCopy(MirValue sourcePtr, MirValue destPtr, Type type)
    {
        auto arrType = cast(ArrayType) type;
        if (!arrType) return;

        // Se o tamanho vier zerado do tipo (bug do array literal), tentamos usar um fallback
        // mas idealmente o type deve vir correto.
        import std.conv;
        int length = to!int(arrType.length);

        foreach (i; 0 .. length)
        {
            // Índice i
            auto idx = MirValue.i32(i, new PrimitiveType(BaseType.Int));

            // 1. Calcula endereço do elemento na origem (src[i])
            auto srcElemPtr = currentFunc.newReg(new PointerType(arrType.elementType));
            auto gepSrc = new MirInstr(MirOp.GetElementPtr);
            gepSrc.dest = srcElemPtr;
            gepSrc.operands = [sourcePtr, idx]; // Backend adiciona o '0' inicial automático
            emit(gepSrc);

            // 2. Calcula endereço do elemento no destino (dest[i])
            auto destElemPtr = currentFunc.newReg(new PointerType(arrType.elementType));
            auto gepDest = new MirInstr(MirOp.GetElementPtr);
            gepDest.dest = destElemPtr;
            gepDest.operands = [destPtr, idx];
            emit(gepDest);

            // 3. Se for array multidimensional, recursão
            if (arrType.elementType.isArray())
            {
                emitArrayCopy(srcElemPtr, destElemPtr, arrType.elementType);
            }
            else
            {
                // 4. Copia o valor (Load -> Store)
                auto val = currentFunc.newReg(arrType.elementType);
                emit(new MirInstr(MirOp.Load, val, [srcElemPtr]));
                emit(new MirInstr(MirOp.Store, MirValue.init, [val, destElemPtr]));
            }
        }
    }

    void fillArray(MirValue basePtr, HirArrayLit lit) 
    {
        auto basePtrType = cast(PointerType) basePtr.type;
        if (!basePtrType) return;

        auto arrayType = cast(ArrayType) basePtrType.pointeeType;
        if (!arrayType) return;

        foreach (i, elemExpr; lit.elements) 
        {
            auto zero = MirValue.i32(0, new PrimitiveType(BaseType.Int));
            auto idx = MirValue.i32(cast(int)i, new PrimitiveType(BaseType.Int));

            auto elemPtrType = new PointerType(arrayType.elementType);
            auto elemPtr = currentFunc.newReg(elemPtrType);

            auto gep = new MirInstr(MirOp.GetElementPtr);
            gep.dest = elemPtr;
            gep.operands = [basePtr, zero, idx]; 

            emit(gep);

            if (auto subLit = cast(HirArrayLit) elemExpr) 
                fillArray(elemPtr, subLit);
            else 
                emitStore(lowerExpr(elemExpr), elemPtr);
        }
    }

    void lowerSwitch(HirSwitch stmt)
    {
        auto condVal = lowerExpr(stmt.condition);

        string endLabel = currentFunc.uniqueBlockName("switch_end");
        string defaultLabel = null;

        struct CaseInfo {
            string label;
            HirCaseStmt caseStmt;
        }

        CaseInfo[] cases;

        foreach (i, caseStmt; stmt.cases)
        {
            if (caseStmt.isDefault)
                defaultLabel = currentFunc.uniqueBlockName("switch_default");
            else
            {
                string label = currentFunc.uniqueBlockName("switch_case");
                cases ~= CaseInfo(label, caseStmt);
            }
        }

        if (defaultLabel is null)
            defaultLabel = endLabel;

        string currentCheckLabel = currentFunc.uniqueBlockName("switch_check");
        emitBr(currentCheckLabel);

        foreach (i, caseInfo; cases)
        {
            auto checkBB = new MirBasicBlock(currentCheckLabel);
            currentFunc.blocks ~= checkBB;
            currentBlock = checkBB;

            string nextCheckLabel;
            if (i + 1 < cases.length)
                nextCheckLabel = currentFunc.uniqueBlockName("switch_check");
            else
                nextCheckLabel = defaultLabel;

            bool firstValue = true;
            foreach (j, value; caseInfo.caseStmt.values)
            {
                auto caseVal = lowerExpr(value);

                auto cmpReg = currentFunc.newReg(new PrimitiveType(BaseType.Bool));
                auto cmpInstr = new MirInstr(MirOp.ICmp);
                cmpInstr.dest = cmpReg;
                cmpInstr.operands = [condVal, caseVal, MirValue.stringLit("==", null)];
                emit(cmpInstr);

                if (j == cast(int) caseInfo.caseStmt.values.length - 1)
                {
                    // Último valor: vai pro case ou próximo check
                    auto br = new MirInstr(MirOp.CondBr);
                    br.operands = [cmpReg, MirValue.block(caseInfo.label), MirValue.block(nextCheckLabel)];
                    emit(br);
                }
                else
                {
                    string continueLabel = currentFunc.uniqueBlockName("switch_or");
                    auto br = new MirInstr(MirOp.CondBr);
                    br.operands = [cmpReg, MirValue.block(caseInfo.label), MirValue.block(continueLabel)];
                    emit(br);

                    auto orBB = new MirBasicBlock(continueLabel);
                    currentFunc.blocks ~= orBB;
                    currentBlock = orBB;
                }
            }
            currentCheckLabel = nextCheckLabel;
        }

        foreach (i, caseInfo; cases)
        {
            auto caseBB = new MirBasicBlock(caseInfo.label);
            currentFunc.blocks ~= caseBB;
            currentBlock = caseBB;

            loopStack ~= LoopContext("", endLabel, deferStack.length);
            lowerBlock(caseInfo.caseStmt.body);
            loopStack.length--;

            if (!blockHasTerminator(currentBlock))
            {
                if (i + 1 < cases.length)
                    emitBr(cases[i + 1].label);
                else
                    emitBr(defaultLabel != endLabel ? defaultLabel : endLabel);
            }
        }

        if (defaultLabel != endLabel)
        {
            auto defaultBB = new MirBasicBlock(defaultLabel);
            currentFunc.blocks ~= defaultBB;
            currentBlock = defaultBB;

            foreach (caseStmt; stmt.cases)
            {
                if (caseStmt.isDefault)
                {
                    loopStack ~= LoopContext("", endLabel, deferStack.length);
                    lowerBlock(caseStmt.body);
                    loopStack.length--;
                    break;
                }
            }

            if (!blockHasTerminator(currentBlock))
                emitBr(endLabel);
        }

        auto endBB = new MirBasicBlock(endLabel);
        currentFunc.blocks ~= endBB;
        currentBlock = endBB;
    }

    void lowerStmt(HirNode stmt) 
    {
        if (stmt is null) return;

        switch (stmt.kind) 
        {
            case HirNodeKind.Switch:
                lowerSwitch(cast(HirSwitch) stmt);
                break;

            case HirNodeKind.Defer:
                addDefer((cast(HirDefer)stmt).value);
                break;

            case HirNodeKind.VarDecl:
                HirVarDecl var = cast(HirVarDecl) stmt;
                auto ptrType = new PointerType(var.type);

                auto ptrReg = currentFunc.newReg(ptrType);
                auto alloc = new MirInstr(MirOp.Alloca);
                alloc.dest = ptrReg;
                emit(alloc);
                
                varMap[var.name] = ptrReg;

                if (var.initValue !is null) 
                {
                    if (auto arrLit = cast(HirArrayLit) var.initValue) 
                        fillArray(ptrReg, arrLit);
                    else {
                        // Inicialização normal (int, struct, etc)
                        auto val = lowerExpr(var.initValue);
                        emitStore(val, ptrReg);
                    }
                }
                break;

            case HirNodeKind.Version:
                auto ver = cast(HirVersion) stmt;
                lowerBlock(ver.block);
                break;
                
            case HirNodeKind.Store:
                auto s = cast(HirStore) stmt;
                auto val = lowerExpr(s.value);
                auto ptr = lowerLValue(s.ptr);
                emitStore(val, ptr);
                break;
            
            case HirNodeKind.If:
                lowerIf(cast(HirIf) stmt);
                break;

            case HirNodeKind.For:
                lowerFor(cast(HirFor) stmt);
                break;

            case HirNodeKind.Block:
                lowerBlock(cast(HirBlock) stmt);
                break;

            case HirNodeKind.While:
                lowerWhile(cast(HirWhile) stmt);
                break;

            case HirNodeKind.Return:
                auto ret = cast(HirReturn) stmt;
                
                MirValue retVal;
                // 1. Calcula o valor de retorno ANTES de executar os defers
                // (pois defers podem alterar estado, mas não o valor já avaliado do return)
                if (ret.value !is null) 
                    retVal = lowerExpr(ret.value);

                // 2. Executa TODOS os defers da pilha (LIFO)
                emitAllDefers();

                // 3. Emite instrução Ret
                auto instr = new MirInstr(MirOp.Ret);
                if (ret.value !is null) 
                    instr.operands ~= retVal;
                emit(instr);
                break;

            case HirNodeKind.CallStmt:
                lowerExpr((cast(HirCallStmt)stmt).call);
                break;

            case HirNodeKind.AssignDecl:
                HirAssignDecl assign = cast(HirAssignDecl) stmt;
                auto target = lowerLValue(assign.target);

                if (assign.target.type.isArray()) 
                {
                    auto val = lowerExpr(assign.value);
                    emitArrayCopy(val, target, assign.target.type);
                }
                else 
                {
                    string op = assign.op;
                    bool isCompoundAssign = false;

                    if (op == "+=" || op == "-=" || op == "*=" || op == "/=" || op == "%=" ||
                        op == "<<=" || op == ">>=" || op == "&=" || op == "|=" || op == "^=")
                    {
                        isCompoundAssign = true;
                        op = op[0..$-1];
                    }

                    MirValue val;

                    if (isCompoundAssign) 
                    {
                        auto currentVal = currentFunc.newReg(assign.target.type);
                        emit(new MirInstr(MirOp.Load, currentVal, [target]));
                        auto rhsVal = lowerExpr(assign.value);
                        auto resultReg = currentFunc.newReg(assign.target.type);
                        bool isFloatOp = isFloat(assign.target.type) || isDouble(assign.target.type);
                        MirOp mirOp;

                        if (op == "+") mirOp = isFloatOp ? MirOp.FAdd : MirOp.Add;
                        else if (op == "-") mirOp = isFloatOp ? MirOp.FSub : MirOp.Sub;
                        else if (op == "*") mirOp = isFloatOp ? MirOp.FMul : MirOp.Mul;
                        else if (op == "/") mirOp = isFloatOp ? MirOp.FDiv : MirOp.Div;
                        else if (op == "%") mirOp = isFloatOp ? MirOp.FRem : MirOp.SRem;
                        else if (op == "<<") mirOp = MirOp.Shl;
                        else if (op == ">>") mirOp = MirOp.Shr;
                        else if (op == "&") mirOp = MirOp.BAnd;
                        else if (op == "|") mirOp = MirOp.BOr;
                        else if (op == "^") mirOp = MirOp.BXor;
                        else mirOp = MirOp.Add; // fallback

                        auto binInstr = new MirInstr(mirOp);
                        binInstr.dest = resultReg;
                        binInstr.operands = [currentVal, rhsVal];
                        emit(binInstr);

                        val = resultReg;
                    }
                    else 
                        val = lowerExpr(assign.value);
                    
                    emitStore(val, target);
                }
                break;

            case HirNodeKind.Break:
                if (loopStack.length == 0) 
                {
                    writeln("Error: Break outside of loop");
                    return;
                }
                // Executa defers até o nível do loop alvo
                emitDefersDownTo(loopStack[$-1].deferDepth);
                // Pega o topo da pilha e pula para o breakLabel
                emitBr(loopStack[$-1].breakLabel);
                // (dead block) após o break
                // para evitar que instruções subsequentes sejam emitidas no mesmo bloco 
                // (o que seria inválido no LLVM se já tem terminador).
                createAndSwitchBlock("dead_code_after_break");
                break;

            case HirNodeKind.Continue:
                if (loopStack.length == 0)
                {
                    writeln("Error: Continue outside of loop");
                    return;
                }
                // Executa defers até o nível do loop alvo
                emitDefersDownTo(loopStack[$-1].deferDepth);
                // Pega o topo da pilha e pula para o continueLabel
                emitBr(loopStack[$-1].continueLabel);
                // Mesma lógica do dead block
                createAndSwitchBlock("dead_code_after_continue");
                break;
            
            default:
                 if (isExpression(stmt)) lowerExpr(stmt);
                 break;
        }
    }

    void lowerIf(HirIf stmt) 
    {
        string thenName = currentFunc.uniqueBlockName("then");
        string elseName = currentFunc.uniqueBlockName("else");
        string mergeName = currentFunc.uniqueBlockName("merge");
    
        // if and else if
        if (stmt.condition !is null) 
        {
            auto cond = lowerExpr(stmt.condition);
            
            // Se a condição não é booleana, converte para bool comparando com 0
            if (!isBool(cond.type))
            {
                auto boolReg = currentFunc.newReg(new PrimitiveType(BaseType.Bool));
                
                // Determina o tipo de comparação
                if (isFloat(cond.type) || isDouble(cond.type))
                {
                    // Floating point: fcmp une (não é zero)
                    auto zero = isFloat(cond.type) 
                        ? MirValue.f32(0.0, cond.type)
                        : MirValue.f64(0.0, cond.type);
                    
                    auto cmpInstr = new MirInstr(MirOp.FCmp);
                    cmpInstr.dest = boolReg;
                    cmpInstr.operands = [cond, zero, MirValue.stringLit("!=", null)];
                    emit(cmpInstr);
                }
                else
                {
                    // Inteiro: icmp ne 0
                    auto zero = isLong(cond.type)
                        ? MirValue.i64(0, cond.type)
                        : MirValue.i32(0, cond.type);
                    
                    auto cmpInstr = new MirInstr(MirOp.ICmp);
                    cmpInstr.dest = boolReg;
                    cmpInstr.operands = [cond, zero, MirValue.stringLit("!=", null)];
                    emit(cmpInstr);
                }
                
                cond = boolReg;
            }
    
            auto br = new MirInstr(MirOp.CondBr);
            br.operands ~= cond;
            br.operands ~= MirValue.block(thenName);
            br.operands ~= MirValue.block(stmt.elseBlock ? elseName : mergeName);
            emit(br);
    
            auto thenBB = new MirBasicBlock(thenName);
            currentFunc.blocks ~= thenBB;
            currentBlock = thenBB;
            lowerBlock(stmt.thenBlock);
            if (!blockHasTerminator(currentBlock)) emitBr(mergeName);
    
            if (stmt.elseBlock) 
            {
                auto elseBB = new MirBasicBlock(elseName);
                currentFunc.blocks ~= elseBB;
                currentBlock = elseBB;
                lowerBlock(stmt.elseBlock);
                if (!blockHasTerminator(currentBlock)) emitBr(mergeName);
            }
    
            auto mergeBB = new MirBasicBlock(mergeName);
            currentFunc.blocks ~= mergeBB;
            currentBlock = mergeBB;
        } 
        else
            lowerBlock(stmt.thenBlock);
    }

    bool isBool(Type t) 
    {
        auto p = cast(PrimitiveType)t;
        return p && (p.baseType == BaseType.Bool);
    }

    void lowerWhile(HirWhile stmt) 
    {
        string condName = currentFunc.uniqueBlockName("while_cond");
        string bodyName = currentFunc.uniqueBlockName("while_body");
        string endName  = currentFunc.uniqueBlockName("while_end");
    
        emitBr(condName);
    
        auto condBB = new MirBasicBlock(condName);
        currentFunc.blocks ~= condBB;
        currentBlock = condBB;
        
        auto c = lowerExpr(stmt.condition);
        
        // Converte para bool se necessário
        if (!isBool(c.type))
        {
            auto boolReg = currentFunc.newReg(new PrimitiveType(BaseType.Bool));
            
            if (isFloat(c.type) || isDouble(c.type))
            {
                auto zero = isFloat(c.type) 
                    ? MirValue.f32(0.0, c.type)
                    : MirValue.f64(0.0, c.type);
                auto cmpInstr = new MirInstr(MirOp.FCmp);
                cmpInstr.dest = boolReg;
                cmpInstr.operands = [c, zero, MirValue.stringLit("!=", null)];
                emit(cmpInstr);
            }
            else
            {
                auto zero = isLong(c.type)
                    ? MirValue.i64(0, c.type)
                    : MirValue.i32(0, c.type);
                auto cmpInstr = new MirInstr(MirOp.ICmp);
                cmpInstr.dest = boolReg;
                cmpInstr.operands = [c, zero, MirValue.stringLit("!=", null)];
                emit(cmpInstr);
            }
            
            c = boolReg;
        }
        
        auto br = new MirInstr(MirOp.CondBr);
        br.operands = [c, MirValue.block(bodyName), MirValue.block(endName)];
        emit(br);
        
        loopStack ~= LoopContext(condName, endName, deferStack.length);
        auto bodyBB = new MirBasicBlock(bodyName);
        currentFunc.blocks ~= bodyBB;
        currentBlock = bodyBB;
        
        lowerBlock(stmt.body);
    
        if (!blockHasTerminator(currentBlock))
            emitBr(condName);
    
        loopStack.length--;
        auto endBB = new MirBasicBlock(endName);
        currentFunc.blocks ~= endBB;
        currentBlock = endBB;
    }

    void lowerFor(HirFor stmt)
    {
        string condName = currentFunc.uniqueBlockName("for_cond");
        string bodyName = currentFunc.uniqueBlockName("for_body");
        string incName  = currentFunc.uniqueBlockName("for_inc");
        string endName  = currentFunc.uniqueBlockName("for_end");

        if (stmt.init_) lowerStmt(stmt.init_);
        emitBr(condName);

        auto condBB = new MirBasicBlock(condName);
        currentFunc.blocks ~= condBB;
        currentBlock = condBB;
        
        if (stmt.condition)
        {
            auto c = lowerExpr(stmt.condition);
            auto br = new MirInstr(MirOp.CondBr);
            br.operands = [c, MirValue.block(bodyName), MirValue.block(endName)];
            emit(br);
        } else
            // "for (;;)" loop infinito
            emitBr(bodyName);

        loopStack ~= LoopContext(condName, endName, deferStack.length);
        auto bodyBB = new MirBasicBlock(bodyName);
        currentFunc.blocks ~= bodyBB;
        currentBlock = bodyBB;
        lowerBlock(stmt.body);
        
        if (!blockHasTerminator(currentBlock))
            emitBr(incName);

        loopStack.length--;
        auto incBB = new MirBasicBlock(incName);
        currentFunc.blocks ~= incBB;
        currentBlock = incBB;
        
        if (stmt.increment)
        {
             if (isExpression(stmt.increment)) lowerExpr(stmt.increment);
             else lowerStmt(stmt.increment);
        }

        emitBr(condName);

        auto endBB = new MirBasicBlock(endName);
        currentFunc.blocks ~= endBB;
        currentBlock = endBB;
    }

    int getTypeId(Type t) 
    {
        if (t.toStr() == BaseType.Int) return 1;
        if (t.toStr() == BaseType.Long) return 2;
        if (t.toStr() == BaseType.Float) return 3;
        if (t.toStr() == BaseType.Double) return 4;
        if (t.toStr() == BaseType.String) return 5;
        if (t.isPointer()) return 6;
        if (t.toStr() == BaseType.Char) return 7;
        return 0; // None
    }

    MirValue lowerExpr(HirNode expr)
    {
        if (currentFunc is null)
        {
            switch (expr.kind)
            {
                case HirNodeKind.StringLit:
                    auto s = cast(HirStringLit) expr;
                    return MirValue.stringLit(s.value, s.type);
                    
                case HirNodeKind.IntLit:
                {
                    HirIntLit i = cast(HirIntLit) expr;
                    writeln("Type: ", i.type.toStr());
                    
                    // Verifica o tipo REAL do literal (pode ter sido modificado por cast)
                    if (auto prim = cast(PrimitiveType) i.type)
                    {
                        long value = cast(long) i.value;
                        
                        switch (prim.baseType)
                        {
                            case BaseType.Bool:
                                return MirValue.i1(cast(bool) value, i.type);
                            
                            case BaseType.Byte:
                            case BaseType.Ubyte:
                            case BaseType.Char:
                                return MirValue.i8(cast(byte) value, i.type);
                            
                            case BaseType.Short:
                            case BaseType.Ushort:
                                return MirValue.i16(cast(short) value, i.type);
                            
                            case BaseType.Int:
                            case BaseType.Uint:
                                return MirValue.i32(cast(int) value, i.type);
                            
                            case BaseType.Long:
                            case BaseType.Ulong:
                                return MirValue.i64(value, i.type);
                            
                            default:
                                return MirValue.i32(cast(int) value, i.type);
                        }
                    }
                    
                    return MirValue.i32(cast(int) i.value, i.type);
                }
                
                case HirNodeKind.LongLit:
                {
                    HirLongLit l = cast(HirLongLit) expr;
                    writeln("Type: ", l.type.toStr());
                    
                    if (auto prim = cast(PrimitiveType) l.type)
                    {
                        long value = l.value;
                        
                        switch (prim.baseType)
                        {
                            case BaseType.Bool:
                                return MirValue.i1(cast(bool) value, l.type);
                            
                            case BaseType.Byte:
                            case BaseType.Ubyte:
                            case BaseType.Char:
                                return MirValue.i8(cast(byte) value, l.type);
                            
                            case BaseType.Short:
                            case BaseType.Ushort:
                                return MirValue.i16(cast(short) value, l.type);
                            
                            case BaseType.Int:
                            case BaseType.Uint:
                                return MirValue.i32(cast(int) value, l.type);
                            
                            case BaseType.Long:
                            case BaseType.Ulong:
                                return MirValue.i64(value, l.type);
                            
                            default:
                                return MirValue.i64(value, l.type);
                        }
                    }
                    
                    return MirValue.i64(l.value, l.type);
                }
                
                case HirNodeKind.FloatLit:
                    auto f = cast(HirFloatLit) expr;
                    return MirValue.f32(f.value, f.type);

                case HirNodeKind.BoolLit:
                    auto i = cast(HirBoolLit) expr;
                    return MirValue.i1(cast(bool)i.value, i.type);

                case HirNodeKind.NullLit:
                    return MirValue.nullPtr(null);

                case HirNodeKind.CharLit:
                    auto c = cast(HirCharLit) expr;
                    return MirValue.i8(cast(ubyte)c.value, c.type);

                case HirNodeKind.ArrayLit:
                    auto lit = cast(HirArrayLit) expr;

                    if (auto arrT = cast(ArrayType) lit.type)
                        if (arrT.length == 0) arrT.length = cast(long)lit.elements.length;

                    MirValue aggVal;
                    aggVal.type = lit.type;
                    aggVal.isConst = true;
                    aggVal.isArrayLiteral = true;

                    foreach (elem; lit.elements)
                    {
                        auto constElem = lowerExpr(elem);
                        if (!constElem.isConst)
                            writeln("Erro: Inicializadores globais devem ser constantes.");
                        aggVal.elements ~= constElem;
                    }
                    
                    return aggVal;

                default:
                    writeln("ERRO: Inicializador global deve ser uma constante em tempo de compilação!");
                    writeln("  Tipo de expressão inválida: ", expr.kind);
                    return MirValue.i32(0, new PrimitiveType(BaseType.Int));
            }
        }

        switch (expr.kind)
        {
            case HirNodeKind.FunctionRef:
                auto fnRef = cast(HirFunctionRef) expr;
                MirValue val;
                val.isConst = true;
                val.constStr = fnRef.name;
                val.type = fnRef.type;
                val.isRef = true;
                val.refType = cast(FunctionType) fnRef.type;
                return val;

            case HirNodeKind.AddrOfComplex:
                auto addr = cast(HirAddrOfComplex) expr;
                // Para &(expressão), calculamos o LValue sem fazer Load
                return lowerLValue(addr.expr);

            case HirNodeKind.ArrayLit:
                auto lit = cast(HirArrayLit) expr;

                if (auto arrT = cast(ArrayType) lit.type)
                    if (arrT.length == 0) arrT.length = cast(long)lit.elements.length;

                if (currentFunc is null)
                {
                    MirValue aggVal;
                    aggVal.type = lit.type;
                    aggVal.isConst = true;
                    aggVal.isArrayLiteral = true;

                    // Recursivamente resolve os elementos
                    // Como currentFunc é null, as chamadas recursivas também 
                    // cairão nos casos globais/constantes.
                    foreach (elem; lit.elements)
                    {
                        auto constElem = lowerExpr(elem);
                        // Validação opcional (mas recomendada)
                        if (!constElem.isConst) 
                            writeln("Erro: Inicializadores globais devem ser constantes.");
                        aggVal.elements ~= constElem;
                    }
                    
                    return aggVal;
                }
                
                auto ptrType = new PointerType(lit.type);
                auto tempPtr = currentFunc.newReg(ptrType);
                
                auto alloc = new MirInstr(MirOp.Alloca);
                alloc.dest = tempPtr;
                emit(alloc);
                fillArray(tempPtr, lit);
                
                return tempPtr;

            case HirNodeKind.StringLit:
                auto s = cast(HirStringLit) expr;
                return MirValue.stringLit(s.value, s.type);
                
            case HirNodeKind.IntLit:
                HirIntLit i = cast(HirIntLit) expr;
                return MirValue.i32(cast(int)i.value, i.type);

            case HirNodeKind.LongLit:
                auto l = cast(HirLongLit) expr;
                return MirValue.i64(l.value, l.type);
            
            case HirNodeKind.FloatLit:
                auto f = cast(HirFloatLit) expr;
                return MirValue.f32(f.value, f.type);

            case HirNodeKind.BoolLit:
                auto i = cast(HirBoolLit) expr;
                return MirValue.i1(cast(bool)i.value, i.type);

            case HirNodeKind.NullLit:
                return MirValue.nullPtr(null);

            case HirNodeKind.CharLit:
                auto c = cast(HirCharLit) expr;
                return MirValue.i8(cast(ubyte)c.value, c.type);

            case HirNodeKind.Deref:
                auto deref = cast(HirDeref) expr;
                auto ptr = lowerExpr(deref.ptr);  // Avalia o ponteiro
                // Gera Load do conteúdo apontado
                auto dest = currentFunc.newReg(deref.type);
                auto instr = new MirInstr(MirOp.Load);
                instr.dest = dest;
                instr.operands = [ptr];
                emit(instr);
                return dest;

            case HirNodeKind.Binary:
                auto bin = cast(HirBinary) expr;
                auto l = lowerExpr(bin.left);
                auto r = lowerExpr(bin.right);
                auto dest = currentFunc.newReg(bin.type);

                if (cast(PointerType) bin.left.type && (bin.op == "+" || bin.op == "-")) 
                {
                    MirValue indexVal = r;
                    if (bin.op == "-") {
                        // Gera: 0 - r
                        auto zero = MirValue.i32(0, r.type);
                        auto neg = currentFunc.newReg(r.type);
                        emit(new MirInstr(MirOp.Sub, neg, [zero, r]));
                        indexVal = neg;
                    }   
                    auto instr = new MirInstr(MirOp.GetElementPtr);
                    instr.dest = dest;
                    instr.operands = [l, indexVal]; 
                    emit(instr);
                    return dest;
                }

                if (bin.op == "&&" || bin.op == "||")
                {
                    auto instr = new MirInstr(bin.op == "&&" ? MirOp.And : MirOp.Or);
                    instr.dest = dest;
                    instr.operands = [l, r];
                    emit(instr);
                    return dest;
                }
                
                MirOp op;
                
                // Verifica se é float OU double
                bool isFloatOp = isFloat(bin.left.type) || isFloat(bin.right.type) ||
                                 isDouble(bin.left.type) || isDouble(bin.right.type);
                
                // Mapeamento Inteligente
                if (bin.op == "+") op = isFloatOp ? MirOp.FAdd : MirOp.Add;
                else if (bin.op == "-") op = isFloatOp ? MirOp.FSub : MirOp.Sub;
                else if (bin.op == "*") op = isFloatOp ? MirOp.FMul : MirOp.Mul;
                else if (bin.op == "/") op = isFloatOp ? MirOp.FDiv : MirOp.Div;
                else if (bin.op == "%") op = isFloatOp ? MirOp.FRem : MirOp.SRem;
                else if (bin.op == "<<") op = MirOp.Shl;
                else if (bin.op == ">>") op = MirOp.Shr;
                else if (bin.op == "==" || bin.op == "!=" || 
                         bin.op == "<"  || bin.op == "<=" || 
                         bin.op == ">"  || bin.op == ">=")
                    op = isFloatOp ? MirOp.FCmp : MirOp.ICmp;
                else if (bin.op == "^") op = MirOp.BXor;
                else if (bin.op == "|") op = MirOp.BOr;
                else if (bin.op == "~") op = MirOp.BNot;
                else if (bin.op == "&") op = MirOp.BAnd;
                else
                    op = MirOp.Add;

                // store
                // left is a var

                auto instr = new MirInstr(op);
                instr.dest = dest;
                instr.operands = [l, r];
                
                if (op == MirOp.ICmp || op == MirOp.FCmp)
                {
                    MirValue opStr;
                    opStr.isConst = true;
                    opStr.constStr = bin.op;
                    instr.operands ~= opStr;
                }

                emit(instr);
                return dest;

            case HirNodeKind.Load: 
                HirLoad load = cast(HirLoad) expr;
                
                if (FunctionType t = cast(FunctionType) load.type)
                {
                    // é uma função, se é load então é ponteiro
                    MirValue val;
                    val.isConst = true;
                    val.constStr = load.varName;
                    val.type = t;
                    val.isRef = true;
                    val.refType = t;
                    return val;
                }

                auto ptr = varMap[load.varName];

                if (load.type.isArray())
                    return ptr; 
                
                auto dest = currentFunc.newReg(load.type);
                auto instr = new MirInstr(MirOp.Load);
                instr.dest = dest;
                instr.operands = [ptr];
                emit(instr);
                return dest;

            case HirNodeKind.AddrOf:
                return varMap[(cast(HirAddrOf)expr).varName];

            case HirNodeKind.CallExpr:
                HirCallExpr call = cast(HirCallExpr) expr;
                auto dest = currentFunc.newReg(call.type);
                auto instr = new MirInstr(MirOp.Call);
                instr.dest = dest;
                MirValue funcNameVal; 

                if (call.funcName in varMap)
                {
                    funcNameVal = varMap[call.funcName];
                    funcNameVal.isRef = call.isRef;
                    funcNameVal.refType = call.refType;
                }
                else {
                    funcNameVal.isConst = true; 
                    funcNameVal.constStr = call.funcName;
                    funcNameVal.type = call.type;
                    funcNameVal.isRef = call.isRef;
                    funcNameVal.refType = call.refType;
                }
                instr.operands ~= funcNameVal;
            
                auto intType = new PrimitiveType(BaseType.Int);
                auto longType = new PrimitiveType(BaseType.Long);

                if (call.isOrnVarArgs) 
                {
                    auto varargsValueType = registry.lookupType("VarArgsValue");
                    auto VarArgsType = registry.lookupType("OrnVarArgs");

                    if (varargsValueType is null || VarArgsType is null) {
                        writeln("ERROR: VarArgsValue or OrnVarArgs type not found in registry!");
                        emit(instr);
                        return dest;
                    }

                    int splitIndex = call.isVarArgAt;
                    if (call.isOrnVarArgs)
                        splitIndex--;
                    int varArgCount = cast(int)(call.args.length - splitIndex);

                    for (int i = 0; i < splitIndex; i++) 
                    {
                        HirNode arg = call.args[i];
                        MirValue argVal;

                        if (i == 0 && cast(StructType)arg.type && arg.kind == HirNodeKind.CallExpr) {
                            auto tempVal = lowerExpr(arg);
                            auto tempPtrType = new PointerType(arg.type);
                            auto tempPtr = currentFunc.newReg(tempPtrType);
                            emit(new MirInstr(MirOp.Alloca, tempPtr));
                            emitStore(tempVal, tempPtr);
                            argVal = tempPtr;
                        } else
                            argVal = lowerExpr(arg);
                        instr.operands ~= argVal;
                    }

                    auto arrayType = new ArrayType(varargsValueType, 0, varArgCount);
                    auto arrayPtr = currentFunc.newReg(new PointerType(arrayType));
                    emit(new MirInstr(MirOp.Alloca, arrayPtr));

                    for (int j = 0; j < varArgCount; j++) 
                    {
                        HirNode arg = call.args[splitIndex + j];
                        MirValue val = lowerExpr(arg);

                         if (auto structType = cast(StructType)arg.type) 
                            if (structType.name == "OrnVarArgs") 
                            {
                                instr.operands ~= val;
                                continue;
                            }
                        
                        // GEP para acessar VarargsValue[j] -> [arrayPtr, j]
                        auto elemPtr = currentFunc.newReg(new PointerType(varargsValueType));
                        emit(new MirInstr(MirOp.GetElementPtr, elemPtr, 
                            [arrayPtr, MirValue.i32(j, intType)]));

                        // Acessa campo 0 (TypeId) -> [elemPtr, 0, 0]
                        auto typeIdPtr = currentFunc.newReg(new PointerType(intType));
                        emit(new MirInstr(MirOp.GetElementPtr, typeIdPtr, 
                            [elemPtr, MirValue.i32(0, intType), MirValue.i32(0, intType)]));

                        int tid = getTypeId(arg.type);
                        emitStore(MirValue.i32(tid, intType), typeIdPtr);

                        // A union no Orn é representada como i64 no backend
                        auto unionPtr = currentFunc.newReg(new PointerType(longType));
                        emit(new MirInstr(MirOp.GetElementPtr, unionPtr, 
                            [elemPtr, MirValue.i32(0, intType), MirValue.i32(1, intType)]));

                        // Converter o valor para i64 dependendo do tipo
                        MirValue convertedVal;
                        auto primType = cast(PrimitiveType)arg.type;
                        bool isString = primType && primType.baseType == BaseType.String;

                        if (isInt(arg.type) || (cast(PrimitiveType)arg.type && (cast(PrimitiveType)arg.type).baseType 
                            == BaseType.Char))
                        {
                            // int ou char -> i64 (sign extend)
                            convertedVal = currentFunc.newReg(longType);
                            emit(new MirInstr(MirOp.SExt, convertedVal, [val]));
                        }
                        else if (isDouble(arg.type)) 
                        {
                            convertedVal = currentFunc.newReg(longType);
                            emit(new MirInstr(MirOp.BitCast, convertedVal, [val]));
                        }
                        else if (isFloat(arg.type)) 
                        {
                            auto asDouble = currentFunc.newReg(new PrimitiveType(BaseType.Double));
                            emit(new MirInstr(MirOp.FPExt, asDouble, [val]));

                            convertedVal = currentFunc.newReg(longType);
                            emit(new MirInstr(MirOp.BitCast, convertedVal, [asDouble]));
                        }
                        else if (isString || cast(PointerType)arg.type) 
                        {
                            // string (char*) ou qualquer ponteiro -> ptrtoint para i64
                            convertedVal = currentFunc.newReg(longType);
                            emit(new MirInstr(MirOp.PtrToInt, convertedVal, [val]));
                        }
                        else if (isLong(arg.type))
                            convertedVal = val;
                        else 
                        {
                            // Fallback: bitcast direto (se já for i64 ou compatível)
                            convertedVal = currentFunc.newReg(longType);
                            emit(new MirInstr(MirOp.BitCast, convertedVal, [val]));
                        }

                        emitStore(convertedVal, unionPtr);
                    }

                    // Criar struct OrnVarArgs { ptr, length }
                    auto varargsStructPtr = currentFunc.newReg(new PointerType(VarArgsType));
                    emit(new MirInstr(MirOp.Alloca, varargsStructPtr));

                    auto fPtrPtr = currentFunc.newReg(new PointerType(new PointerType(varargsValueType)));
                    emit(new MirInstr(MirOp.GetElementPtr, fPtrPtr, 
                        [varargsStructPtr, MirValue.i32(0, intType), MirValue.i32(0, intType)]));

                    auto firstElemPtr = currentFunc.newReg(new PointerType(varargsValueType));
                    emit(new MirInstr(MirOp.GetElementPtr, firstElemPtr,
                        [arrayPtr, MirValue.i32(0, intType)]));
                    emitStore(firstElemPtr, fPtrPtr);

                    auto fLenPtr = currentFunc.newReg(new PointerType(longType));
                    emit(new MirInstr(MirOp.GetElementPtr, fLenPtr, 
                        [varargsStructPtr, MirValue.i32(0, intType), MirValue.i32(1, intType)]));
                    emitStore(MirValue.i64(cast(long)varArgCount, longType), fLenPtr);

                    auto ArgsValue = currentFunc.newReg(VarArgsType);
                    emit(new MirInstr(MirOp.Load, ArgsValue, [varargsStructPtr]));
                    instr.operands ~= ArgsValue;

                    emit(instr);
                    return dest;
                }

                bool injectCount = call.isVarArg && !call.isOrnVarArgs && !call.isExternalCall;
                int splitIndex = call.isVarArgAt;

                for (int i = 0; i < call.args.length; i++) 
                {
                    if (injectCount && i == splitIndex) 
                    {
                        int varArgCount = cast(int)(call.args.length - splitIndex);    
                        auto countVal = MirValue.i32(varArgCount, intType);
                        instr.operands ~= countVal;
                        injectCount = false;
                    }

                    HirNode arg = call.args[i];
                    MirValue argVal;

                    if (arg.type.isArray()) 
                    {
                        // Arrays são passados por valor, então precisamos fazer Load
                        auto ptrVal = lowerExpr(arg);

                        // Se ptrVal já é um ponteiro de array (da stack), carregamos o valor
                        if (auto ptrT = cast(PointerType) ptrVal.type) 
                        {
                            if (ptrT.pointeeType.isArray()) 
                            {
                                // Fazer Load do array
                                auto arrayVal = currentFunc.newReg(arg.type);
                                emit(new MirInstr(MirOp.Load, arrayVal, [ptrVal]));
                                argVal = arrayVal;
                            }
                            else
                                argVal = ptrVal;
                        }
                        else
                            argVal = ptrVal;
                    }
                    else if (i == 0 && cast(StructType)arg.type && arg.kind == HirNodeKind.CallExpr) {
                        auto tempVal = lowerExpr(arg);
                        auto tempPtrType = new PointerType(arg.type);
                        auto tempPtr = currentFunc.newReg(tempPtrType);
                        emit(new MirInstr(MirOp.Alloca, tempPtr));
                        emitStore(tempVal, tempPtr);
                        argVal = tempPtr;
                    } else
                        argVal = lowerExpr(arg);
                    instr.operands ~= argVal;
                }

                if (injectCount) 
                {
                    auto countVal = MirValue.i32(0, intType);
                    instr.operands ~= countVal;
                }

                emit(instr);
                return dest;
            
            case HirNodeKind.Cast:
                auto c = cast(HirCast) expr;
                auto val = lowerExpr(c.value);
                auto dest = currentFunc.newReg(c.type);

                Type srcType = val.type;
                Type dstType = c.targetType;

                int srcSize = getBitSize(srcType);
                int dstSize = getBitSize(dstType);

                bool srcIsFloat = isFloatingPoint(srcType);
                bool dstIsFloat = isFloatingPoint(dstType);
                bool srcIsPtr   = isPointerType(srcType);
                bool dstIsPtr   = isPointerType(dstType);
                bool srcIsInt   = isIntegerType(srcType);
                bool dstIsInt   = isIntegerType(dstType);

                MirOp op = MirOp.BitCast; // Default seguro (mesmo tamanho/tipo)

                // 1. Pointer <-> Integer
                if (srcIsPtr && dstIsInt) {
                    op = MirOp.PtrToInt;
                }
                else if (srcIsInt && dstIsPtr) {
                    op = MirOp.IntToPtr;
                }
                // 2. Float <-> Float (Precisão)
                else if (srcIsFloat && dstIsFloat) {
                    if (srcSize < dstSize)      op = MirOp.FPExt;   // float -> double
                    else if (srcSize > dstSize) op = MirOp.FPTrunc; // double -> float
                    // else BitCast
                }
                // 3. Float <-> Integer
                else if (srcIsFloat && dstIsInt) {
                    // Conversão de float para int requer saber se o destino é assinado ou não
                    // Normalmente usa-se FPToSI (Signed) ou FPToUI (Unsigned)
                    // Se você não tiver FPToUI no seu MIR, use FPToSI como genérico
                    op = isSignedInt(dstType) ? MirOp.FPToSI : MirOp.FPToUI;

                    // Nota: Se você não tiver opcodes Unsigned no MIR, use FPToSI.
                }
                else if (srcIsInt && dstIsFloat) {
                    // Int para Float
                    op = isSignedInt(srcType) ? MirOp.SIToFP : MirOp.UIToFP;

                    // Nota: Mesma coisa, se não tiver UIToFP, use SIToFP.
                }
                // 4. Integer <-> Integer (A mágica acontece aqui)
                else if (srcIsInt && dstIsInt) {
                    if (srcSize > dstSize) {
                        // Maior para menor: Truncar (ex: long -> int, int -> ubyte)
                        op = MirOp.Trunc;
                    }
                    else if (srcSize < dstSize) {
                        // Menor para maior: Extensão
                        // Se a origem é assinada (int, short, byte) -> Sign Extend (mantém sinal negativo)
                        // Se a origem é não-assinada (uint, ubyte, char) -> Zero Extend (preenche com zeros)
                        if (isSignedInt(srcType))
                            op = MirOp.SExt;
                        else
                            op = MirOp.ZExt; 
                    }
                    // Se tamanhos iguais (ex: int -> uint), é BitCast/No-op
                }

                auto instr = new MirInstr(op);
                instr.dest = dest;
                instr.operands = [val];
                emit(instr);
                return dest;

            case HirNodeKind.IndexExpr:
                auto idx = cast(HirIndexExpr) expr;

                // WORKAROUND: Se o tipo é POINTER, estamos processando um &tokens[j]
                // onde o parser errou e colocou o & no lugar errado.
                // Neste caso, NÃO devemos fazer Load!
                // if (cast(PointerType) idx.type)
                //     return lowerLValue(expr);

                auto ptr = lowerLValue(expr);
                auto dest = currentFunc.newReg(expr.type);
                auto instr = new MirInstr(MirOp.Load);
                instr.dest = dest;
                instr.operands = [ptr];
                emit(instr);

                return dest;

            case HirNodeKind.Unary:
                auto un = cast(HirUnary) expr;
                auto operand = lowerExpr(un.operand);
                auto dest = currentFunc.newReg(un.type);
                
                // Verifica o tipo do operando
                bool isFloatOp = isFloat(un.operand.type) || isDouble(un.operand.type);
                bool isIntOp = isInt(un.operand.type) || isLong(un.operand.type);
                
                if (un.op == "-") 
                {
                    // Negação Aritmética (Float -> FNeg, Int -> Subtrair de zero)
                    MirOp op = isFloatOp ? MirOp.FNeg : MirOp.Neg;
                    
                    if (op == MirOp.Neg)
                     {
                        // Negação Inteira: Geração manual: 0 - operando
                        auto zero = MirValue.i32(0, un.operand.type);
                        auto instr = new MirInstr(MirOp.Sub);
                        instr.dest = dest;
                        instr.operands = [zero, operand];
                        emit(instr);
                        return dest;
                    } 
                    else {
                        // Negação Float
                        auto instr = new MirInstr(MirOp.FNeg);
                        instr.dest = dest;
                        instr.operands = [operand];
                        emit(instr);
                        return dest;
                    }
                } 
                else if (un.op == "!") 
                {
                    // Negação Lógica (XOR com 1 para booleanos)
                    auto one = MirValue.i1(true, un.operand.type);
                    auto instr = new MirInstr(MirOp.Xor);
                    instr.dest = dest;
                    instr.operands = [operand, one];
                    emit(instr);
                    return dest;
                }
                else if (un.op == "~") 
                {
                    // NOT Bitwise: inverte todos os bits
                    auto instr = new MirInstr(MirOp.BNot);
                    instr.dest = dest;
                    instr.operands = [operand];
                    emit(instr);
                    return dest;
                }
                 else if (un.op == "++_postfix" || un.op == "--_postfix") 
                 {
                    auto ptr = lowerLValue(un.operand);
                    // 2. Carregar o valor.
                    auto oldVal = currentFunc.newReg(un.operand.type);
                    emit(new MirInstr(MirOp.Load, oldVal, [ptr]));
                    // 3. Adicionar/Subtrair 1.
                    auto one = isIntOp ? MirValue.i32(1, un.operand.type) : MirValue.f32(1.0, un.operand.type);
                    MirOp addOp = isFloatOp ? MirOp.FAdd : MirOp.Add;
                    MirOp subOp = isFloatOp ? MirOp.FSub : MirOp.Sub;
                    auto newVal = currentFunc.newReg(un.operand.type);
                    emit(new MirInstr(un.op == "++_postfix" ? addOp : subOp, newVal, [oldVal, one]));
                    // 4. Salvar o novo valor.
                    emit(new MirInstr(MirOp.Store, MirValue.init, [newVal, ptr]));
                    // 5. O resultado da expressão pré-fixada é o NOVO valor (newVal)
                    return oldVal;
                }
                // Se não for um dos operadores acima, retorna MirValue nulo
                return MirValue();

            case HirNodeKind.MemberAccess:
                HirMemberAccess mem = cast(HirMemberAccess) expr;

                MirValue basePtr;

                // Verifica se o target é um IndexExpr (array[i].field)
                if (mem.target.kind == HirNodeKind.IndexExpr) 
                {
                    auto idx = cast(HirIndexExpr) mem.target;
                    // precisamos carregar esse ponteiro antes de acessar o campo!

                    if (cast(PointerType) idx.type) 
                    {
                        // Ex: Person** ptr_array; ptr_array[i].name
                        // idx.type é Person* (ponteiro)

                        // 1. Calcula endereço de array[i] (Person**)
                        auto ptrToPtr = lowerLValue(mem.target);

                        // 2. Carrega o ponteiro (Person*)
                        basePtr = currentFunc.newReg(idx.type);
                        auto loadInstr = new MirInstr(MirOp.Load);
                        loadInstr.dest = basePtr;
                        loadInstr.operands = [ptrToPtr];
                        emit(loadInstr);
                    }
                    else
                        // Array normal: Person[]; array[i].name
                        // idx.type é Person (struct)
                        basePtr = lowerLValue(mem.target);
                }
                // Se o target é Load de uma variável
                else if (mem.target.kind == HirNodeKind.Load) 
                {
                    HirLoad load = cast(HirLoad) mem.target;

                    if (EnumType enm = cast(EnumType) load.type)
                    {
                        // carrega o valor do field
                        int value = enm.getMemberValue(mem.memberName);
                        Type t = new PrimitiveType(BaseType.Int);
                        basePtr = currentFunc.newReg(t);
                        return MirValue.i32(value, t);
                    }

                    if (cast(PointerType) mem.target.type) 
                    {
                        // Ex: Person* current; current.name
                        auto ptrToPtr = varMap[load.varName];

                        basePtr = currentFunc.newReg(load.type);
                        auto loadInstr = new MirInstr(MirOp.Load);
                        loadInstr.dest = basePtr;
                        loadInstr.operands = [ptrToPtr];
                        emit(loadInstr);
                    }
                    else
                        // Variável normal (Person x; x.name)
                        basePtr = varMap[load.varName];
                }
                // Se o target já é um ponteiro (expressão que retorna ponteiro)
                else if (cast(PointerType) mem.target.type)
                    basePtr = lowerExpr(mem.target);
                // Caso geral: pega o LValue
                else
                    basePtr = lowerLValue(mem.target);

                // Agora calcula o offset do campo
                auto i8Type = new PointerType(new PrimitiveType(BaseType.Char));
                auto bytePtr = currentFunc.newReg(i8Type);
                auto cast1 = new MirInstr(MirOp.BitCast);
                cast1.dest = bytePtr;
                cast1.operands = [basePtr];
                emit(cast1);

                auto offsetVal = MirValue.i32(mem.memberOffset, new PrimitiveType(BaseType.Int));
                auto newBytePtr = currentFunc.newReg(i8Type);

                auto gep = new MirInstr(MirOp.GetElementPtr);
                gep.dest = newBytePtr;
                gep.operands = [bytePtr, offsetVal];
                emit(gep);

                auto fieldPtrType = new PointerType(mem.type);
                auto fieldPtr = currentFunc.newReg(fieldPtrType);

                auto cast2 = new MirInstr(MirOp.BitCast);
                cast2.dest = fieldPtr;
                cast2.operands = [newBytePtr];
                emit(cast2);

                // Carregar o valor do campo
                auto dest = currentFunc.newReg(expr.type);
                auto instr = new MirInstr(MirOp.Load);
                instr.dest = dest;
                instr.operands = [fieldPtr];
                emit(instr);

                return dest;

            case HirNodeKind.StructLit:
                auto lit = cast(HirStructLit) expr;
                
                // 1. Aloca espaço temporário na stack para montar a struct
                // Isso cria: %temp = alloca %Test
                auto structPtrType = new PointerType(lit.type);
                auto structPtr = currentFunc.newReg(structPtrType);
                emit(new MirInstr(MirOp.Alloca, structPtr));
                
                // 2. Itera sobre os valores definidos no HIR
                foreach (i, valExpr; lit.fieldValues) 
                {
                    // Avalia o valor (ex: 69, "Fernando")
                    auto val = lowerExpr(valExpr);
                    
                    auto zero = MirValue.i32(0, new PrimitiveType(BaseType.Int));
                    auto idx = MirValue.i32(cast(int)i, new PrimitiveType(BaseType.Int));
                    
                    // Cria registrador para o ponteiro do campo
                    auto fieldPtrType = new PointerType(val.type); 
                    auto fieldPtr = currentFunc.newReg(fieldPtrType);
                    
                    auto gep = new MirInstr(MirOp.GetElementPtr);
                    gep.dest = fieldPtr;
                    // IMPORTANTE: Mandamos 3 operandos: [Base, Index0, Index1]
                    gep.operands = [structPtr, zero, idx]; 
                    emit(gep);
                    
                    // 3. O PASSO QUE FALTAVA: Store
                    emitStore(val, fieldPtr);
                }
                
                // 4. Carrega a struct montada para retornar como "Valor"
                // Isso permite que o VarDecl pegue esse valor e guarde na variável oficial 't'
                auto destStruct = currentFunc.newReg(lit.type);
                emit(new MirInstr(MirOp.Load, destStruct, [structPtr]));
                return destStruct;

            case HirNodeKind.AssignExpr:
                auto ae = cast(HirAssignExpr) expr;
                // Executa a atribuição
                lowerStmt(ae.assign);
                // Retorna o valor (faz Load do target)
                auto ptr = lowerLValue(ae.assign.target);
                auto dest = currentFunc.newReg(ae.type);
                emit(new MirInstr(MirOp.Load, dest, [ptr]));
                return dest;

            default: return MirValue();
        }
    }

    // Retorna o tamanho em bits (approx) para decisão de cast
    int getBitSize(Type t)
    {
        if (cast(PointerType) t) return 64; // Assumindo 64-bit arch
        
        if (auto prim = cast(PrimitiveType) t) {
            switch (prim.baseType) {
                case BaseType.Bool:   return 1;
                case BaseType.Byte:   
                case BaseType.Ubyte:  
                case BaseType.Char:   return 8;
                case BaseType.Short:  
                case BaseType.Ushort: return 16;
                case BaseType.Int:    
                case BaseType.Uint:   
                case BaseType.Float:  return 32;
                case BaseType.Long:   
                case BaseType.Ulong:  
                case BaseType.Double: return 64;
                // String costuma ser struct {ptr, len} ou ptr. 
                // Se for ptr char*, é 64.
                case BaseType.String: return 64; 
                default: return 0;
            }
        }
        return 0;
    }
    
    // Verifica se é ponto flutuante
    bool isFloatingPoint(Type t)
    {
        if (auto prim = cast(PrimitiveType) t) {
            return prim.baseType == BaseType.Float || 
                   prim.baseType == BaseType.Double;
        }
        return false;
    }
    
    // Verifica se é inteiro (assinado ou não)
    bool isIntegerType(Type t)
    {
        if (auto prim = cast(PrimitiveType) t) {
            switch (prim.baseType) {
                case BaseType.Byte: case BaseType.Ubyte:
                case BaseType.Short: case BaseType.Ushort:
                case BaseType.Int: case BaseType.Uint:
                case BaseType.Long: case BaseType.Ulong:
                case BaseType.Char: case BaseType.Bool:
                    return true;
                default: return false;
            }
        }
        return false;
    }
    
    // Verifica se o tipo de origem requer extensão de sinal (Signed vs Unsigned)
    bool isSignedInt(Type t)
    {
        if (auto prim = cast(PrimitiveType) t) {
            switch (prim.baseType) {
                case BaseType.Byte:
                case BaseType.Short:
                case BaseType.Int:
                case BaseType.Long:
                    return true;
                default: return false; // Char, Bool, U... são unsigned
            }
        }
        return false;
    }
    
    bool isPointerType(Type t)
    {
        if (cast(PointerType) t) return true;
        if (auto prim = cast(PrimitiveType) t) 
            return prim.baseType == BaseType.String; 
        return false;
    }

    MirValue lowerLValue(HirNode node)
    {
        // Caso 1: &var → retorna ponteiro direto
        if (node.kind == HirNodeKind.AddrOf)
            return varMap[(cast(HirAddrOf)node).varName];
    
        // Caso 2: *ptr → retorna o valor do ponteiro (SEM carregar!)
        else if (node.kind == HirNodeKind.Deref)
            // Avalia a expressão do ponteiro (isso pode gerar Load)
               return lowerExpr((cast(HirDeref)node).ptr);

        // Caso 3: var → retorna o ponteiro da variável
        else if (node.kind == HirNodeKind.Load)
            return varMap[(cast(HirLoad) node).varName];

        else if (node.kind == HirNodeKind.IndexExpr)
        {
            auto idx = cast(HirIndexExpr) node;
            MirValue basePtr;

            if (idx.target.kind == HirNodeKind.Load)
            {
                auto load = cast(HirLoad) idx.target;
                auto stackPtr = varMap[load.varName]; // O endereço na stack (Alloca)

                // Se é Array Estático, o Alloca JÁ É o endereço base. Não faz Load.
                // Se é Ponteiro, o Alloca guarda o endereço. Precisa fazer Load.
                if (load.type.isArray()) 
                {
                    basePtr = stackPtr;
                }
                else 
                {
                    // É um int* ptr. Carrega o valor do ponteiro.
                    basePtr = currentFunc.newReg(load.type);
                    auto loadInstr = new MirInstr(MirOp.Load);
                    loadInstr.dest = basePtr;
                    loadInstr.operands = [stackPtr];
                    emit(loadInstr);
                }
            } 
            else if (idx.target.kind == HirNodeKind.IndexExpr)
            {
                // Obtém o endereço do elemento anterior (endereço de x[i])
                auto ptrToElement = lowerLValue(idx.target);

                // Se x[i] resultou em um Ponteiro (ex: int**), precisamos carregar
                // para pegar o endereço base do próximo nível.
                // Se x[i] resultou em um Array interno (ex: int[10][10]), o endereço já serve.
                
                if (cast(PointerType) idx.target.type) 
                {
                    basePtr = currentFunc.newReg(idx.target.type);
                    auto loadInstr = new MirInstr(MirOp.Load);
                    loadInstr.dest = basePtr;
                    loadInstr.operands = [ptrToElement];
                    emit(loadInstr);
                }
                else 
                    basePtr = ptrToElement;
            }
            else
                basePtr = lowerExpr(idx.target);

            // auto indexVal = lowerExpr(idx.index);
            
            // // O resultado de lowerLValue deve ser sempre um PONTEIRO para o elemento
            // auto dest = currentFunc.newReg(new PointerType(idx.type));
            // auto instr = new MirInstr(MirOp.GetElementPtr);
            // instr.dest = dest;
            // instr.operands = [basePtr, indexVal];
            // emit(instr);

            // return dest; 
            auto indexVal = lowerExpr(idx.index);
    
    auto dest = currentFunc.newReg(new PointerType(idx.type));
    auto instr = new MirInstr(MirOp.GetElementPtr);
    instr.dest = dest;
    
    if (auto ptrT = cast(PointerType) basePtr.type) {
        if (cast(ArrayType) ptrT.pointeeType) {
            auto zero = MirValue.i32(0, new PrimitiveType(BaseType.Int));
            instr.operands = [basePtr, zero, indexVal];
        } else {
            instr.operands = [basePtr, indexVal];
        }
    } else {
        instr.operands = [basePtr, indexVal];
    }
    
    emit(instr);
    return dest; 
        } else if (node.kind == HirNodeKind.MemberAccess) 
        {
            auto mem = cast(HirMemberAccess) node;
            if (mem.target.kind == HirNodeKind.IndexExpr) 
            {
                auto idx = cast(HirIndexExpr) mem.target;    
                MirValue basePtr;

                // Se array[i] retorna ponteiro, carrega primeiro
                if (cast(PointerType) idx.type) 
                {
                    // Ex: ptr_array[i].age = 29
                    // 1. Calcula endereço de array[i] (ponteiro para ponteiro)
                    auto ptrToPtr = lowerLValue(mem.target);

                    // 2. Carrega o ponteiro
                    basePtr = currentFunc.newReg(idx.type);
                    auto loadInstr = new MirInstr(MirOp.Load);
                    loadInstr.dest = basePtr;
                    loadInstr.operands = [ptrToPtr];
                    emit(loadInstr);
                }
                else
                    // Array normal de structs
                    basePtr = lowerLValue(mem.target);

                // Calcula offset do campo
                auto i8Type = new PointerType(new PrimitiveType(BaseType.Char));
                auto bytePtr = currentFunc.newReg(i8Type);
                auto cast1 = new MirInstr(MirOp.BitCast);
                cast1.dest = bytePtr;
                cast1.operands = [basePtr];
                emit(cast1);

                auto offsetVal = MirValue.i32(mem.memberOffset, new PrimitiveType(BaseType.Int));
                auto newBytePtr = currentFunc.newReg(i8Type);

                auto gep = new MirInstr(MirOp.GetElementPtr);
                gep.dest = newBytePtr;
                gep.operands = [bytePtr, offsetVal];
                emit(gep);

                auto fieldPtrType = new PointerType(mem.type);
                auto fieldPtr = currentFunc.newReg(fieldPtrType);

                auto cast2 = new MirInstr(MirOp.BitCast);
                cast2.dest = fieldPtr;
                cast2.operands = [newBytePtr];
                emit(cast2);

                return fieldPtr;
            }
    
            // 1. Pega o ponteiro base da struct
            MirValue basePtr;

            // Se o target é Load e o tipo é ponteiro
            if (mem.target.kind == HirNodeKind.Load && cast(PointerType) mem.target.type) 
            {
                auto load = cast(HirLoad) mem.target;
                auto ptrToPtr = varMap[load.varName];

                basePtr = currentFunc.newReg(load.type);
                auto loadInstr = new MirInstr(MirOp.Load);
                loadInstr.dest = basePtr;
                loadInstr.operands = [ptrToPtr];
                emit(loadInstr);
            }
            else if (cast(PointerType) mem.target.type)
                basePtr = lowerExpr(mem.target); 
            else
                basePtr = lowerLValue(mem.target);

            // 2. Cast para i8*
            auto i8Type = new PointerType(new PrimitiveType(BaseType.Char)); 

            auto bytePtr = currentFunc.newReg(i8Type);
            auto cast1 = new MirInstr(MirOp.BitCast);
            cast1.dest = bytePtr;
            cast1.operands = [basePtr];
            emit(cast1);

            // 3. GEP para somar o offset
            auto offsetVal = MirValue.i32(mem.memberOffset, new PrimitiveType(BaseType.Int));
            auto newBytePtr = currentFunc.newReg(i8Type);

            auto gep = new MirInstr(MirOp.GetElementPtr);
            gep.dest = newBytePtr;
            gep.operands = [bytePtr, offsetVal]; 
            emit(gep);

            // 4. Cast de volta para o tipo do campo
            auto fieldPtrType = new PointerType(mem.type);
            auto fieldPtr = currentFunc.newReg(fieldPtrType);

            auto cast2 = new MirInstr(MirOp.BitCast);
            cast2.dest = fieldPtr;
            cast2.operands = [newBytePtr];
            emit(cast2);

            return fieldPtr;
        }
    
        return lowerExpr(node);
    }

    void emit(MirInstr instr)
    {
        currentBlock.instructions ~= instr;
    }

    void emitBr(string targetName)
    {
        auto instr = new MirInstr(MirOp.Br);
        instr.operands = [MirValue.block(targetName)];
        emit(instr);
    }

    void emitStore(MirValue val, MirValue ptr)
    {
        auto instr = new MirInstr(MirOp.Store);
        instr.operands = [val, ptr];
        emit(instr);
    }

    bool blockHasTerminator(MirBasicBlock block) 
    {
        if (block.instructions.length == 0) return false;
        auto op = block.instructions[$-1].op;
        return op == MirOp.Br || op == MirOp.CondBr || op == MirOp.Ret;
    }

    bool isExpression(HirNode node) 
    {
        return node.kind >= HirNodeKind.IntLit && node.kind <= HirNodeKind.CallExpr;
    }

    bool isFloat(Type t) 
    {
        auto p = cast(PrimitiveType)t;
        return p && (p.baseType == BaseType.Float);
    }

    bool isDouble(Type t) 
    {
        auto p = cast(PrimitiveType)t;
        return p && (p.baseType == BaseType.Double);
    }
    
    bool isInt(Type t) 
    {
        auto p = cast(PrimitiveType)t;
        return p && (p.baseType == BaseType.Int);
    }

    bool isLong(Type t) 
    {
        auto p = cast(PrimitiveType)t;
        return p && (p.baseType == BaseType.Long);
    }

    MirOp mapBinOp(string op) {
        if (op == "+") return MirOp.Add;
        if (op == "-") return MirOp.Sub;
        if (op == "*") return MirOp.Mul;
        if (op == "==") return MirOp.ICmp;
        if (op == "<<") return MirOp.Shl;
        if (op == ">>") return MirOp.Shr;
        return MirOp.Add;
    }
}
