module middle.opt.func_inline;

import frontend.parser.ast;
import middle.hir.hir;
import frontend.types.type;
import std.stdio : writeln;
import std.conv : to;
import std.algorithm : canFind;

/// Function Inlining - Substitui chamadas de função pequenas pelo corpo da função
/// Reduz overhead de chamadas e permite mais otimizações
class FuncInline
{
    // Mapa de funções disponíveis para inlining
    HirFunction[string] inlinableFuncs;
    
    // Rastreia profundidade de inlining para evitar explosão de código
    int[string] inlineDepth;
    
    // Configurações
    int maxInlineSize = 50;          // Tamanho máximo de função para inline (em statements)
    int maxInlineDepth = 3;          // Profundidade máxima de inlining recursivo
    int maxTotalInlines = 100;       // Limite total de inlines por função
    
    // Estatísticas
    int inlineCount = 0;
    int skippedRecursive = 0;
    int skippedTooLarge = 0;
    
    HirProgram optimize(HirProgram program)
    {
        inlineCount = 0;
        skippedRecursive = 0;
        skippedTooLarge = 0;
        inlinableFuncs.clear();
        inlineDepth.clear();
        
        // Primeira passagem: identifica funções candidatas a inlining
        foreach (global; program.globals)
            if (auto func = cast(HirFunction) global)
                if (isInlinable(func))
                    inlinableFuncs[func.name] = func;
        
        foreach (ref global; program.globals)
            if (auto func = cast(HirFunction) global)
            {
                inlineDepth.clear();
                if (func.body !is null)
                    func.body = inlineInBlock(func.body, func.name);
            }
        
        return program;
    }
    
private:
    
    bool isInlinable(HirFunction func)
    {
        // Não faz inline de:
        // - main
        // - funções sem corpo
        // - funções variádicas
        // - funções muito grandes
        
        if (func.name == "main")
            return false;
        
        if (func.body is null)
            return false;
        
        if (func.isVarArg || func.isOrnVarArgs)
            return false;
        
        int size = calculateFunctionSize(func);
        
        if (size > maxInlineSize)
        {
            skippedTooLarge++;
            return false;
        }
        
        return true;
    }
    
    int calculateFunctionSize(HirFunction func)
    {
        if (func.body is null)
            return 0;
        return countStatements(func.body);
    }
    
    int countStatements(HirBlock block)
    {
        int count = 0;
        foreach (stmt; block.stmts)
            count += countStmt(stmt);
        return count;
    }
    
    int countStmt(HirNode stmt)
    {
        if (stmt is null) return 0;
        
        switch (stmt.kind)
        {
            case HirNodeKind.If:
                auto ifStmt = cast(HirIf) stmt;
                int count = 1;
                count += countStatements(ifStmt.thenBlock);
                if (ifStmt.elseBlock !is null)
                    count += countStatements(ifStmt.elseBlock);
                return count;
            
            case HirNodeKind.While:
                auto whileStmt = cast(HirWhile) stmt;
                return 1 + countStatements(whileStmt.body);
            
            case HirNodeKind.For:
                auto forStmt = cast(HirFor) stmt;
                return 1 + countStatements(forStmt.body);
            
            case HirNodeKind.Block:
                return countStatements(cast(HirBlock) stmt);
            
            case HirNodeKind.Switch:
                auto switchStmt = cast(HirSwitch) stmt;
                int count = 1;
                foreach (caseStmt; switchStmt.cases)
                    count += countStatements(caseStmt.body);
                return count;
            
            case HirNodeKind.Version:
                auto ver = cast(HirVersion) stmt;
                return countStatements(ver.block);
            
            default:
                return 1;
        }
    }
    
    HirBlock inlineInBlock(HirBlock block, string currentFunc)
    {
        if (block is null) return null;
        
        HirNode[] newStmts;
        
        foreach (stmt; block.stmts)
        {
            auto inlined = inlineInStmt(stmt, currentFunc);
            
            if (auto b = cast(HirBlock) inlined)
                newStmts ~= b.stmts;
            else if (inlined !is null)
                newStmts ~= inlined;
        }
        
        block.stmts = newStmts;
        return block;
    }
    
    struct InlineExprResult
    {
        HirNode[] prefixStmts;  // Statements a serem inseridos antes
        HirNode expr;            // Expressão final (ou Load de variável temporária)
    }

            
    HirNode inlineInStmt(HirNode stmt, string currentFunc)
    {
        if (stmt is null) return null;     
        switch (stmt.kind)
        {
            case HirNodeKind.CallStmt:
                auto callStmt = cast(HirCallStmt) stmt;
                if (auto inlined = tryInlineCall(callStmt.call, currentFunc))
                    return inlined;
                callStmt.call = cast(HirCallExpr) inlineInExpr(callStmt.call, currentFunc);
                return callStmt;
            
            case HirNodeKind.VarDecl:
                auto decl = cast(HirVarDecl) stmt;
                if (decl.initValue !is null)
                {
                    auto result = tryInlineExpr(decl.initValue, currentFunc);    
                    if (result.prefixStmts.length > 0)
                    {
                        auto block = new HirBlock();
                        block.stmts = result.prefixStmts;
                        decl.initValue = result.expr;
                        block.stmts ~= decl;
                        return block;
                    }
                    else
                        decl.initValue = result.expr;
                }
                return decl;
            
            case HirNodeKind.AssignDecl:
                auto assign = cast(HirAssignDecl) stmt;
                assign.target = inlineInExpr(assign.target, currentFunc);
                assign.value = inlineInExpr(assign.value, currentFunc);
                return assign;
            
            case HirNodeKind.Store:
                auto store = cast(HirStore) stmt;
                store.ptr = inlineInExpr(store.ptr, currentFunc);
                store.value = inlineInExpr(store.value, currentFunc);
                return store;
            
            case HirNodeKind.Return:
                auto ret = cast(HirReturn) stmt;
                if (ret.value !is null)
                    ret.value = inlineInExpr(ret.value, currentFunc);
                return ret;
            
            case HirNodeKind.If:
                auto ifStmt = cast(HirIf) stmt;
                ifStmt.condition = inlineInExpr(ifStmt.condition, currentFunc);
                ifStmt.thenBlock = inlineInBlock(ifStmt.thenBlock, currentFunc);
                if (ifStmt.elseBlock !is null)
                    ifStmt.elseBlock = inlineInBlock(ifStmt.elseBlock, currentFunc);
                return ifStmt;
            
            case HirNodeKind.While:
                auto whileStmt = cast(HirWhile) stmt;
                whileStmt.condition = inlineInExpr(whileStmt.condition, currentFunc);
                whileStmt.body = inlineInBlock(whileStmt.body, currentFunc);
                return whileStmt;
            
            case HirNodeKind.For:
                auto forStmt = cast(HirFor) stmt;
                if (forStmt.init_ !is null)
                    forStmt.init_ = inlineInStmt(forStmt.init_, currentFunc);
                if (forStmt.condition !is null)
                    forStmt.condition = inlineInExpr(forStmt.condition, currentFunc);
                if (forStmt.increment !is null)
                    forStmt.increment = inlineInExpr(forStmt.increment, currentFunc);
                forStmt.body = inlineInBlock(forStmt.body, currentFunc);
                return forStmt;
            
            case HirNodeKind.Block:
                return inlineInBlock(cast(HirBlock) stmt, currentFunc);
            
            case HirNodeKind.Switch:
                auto switchStmt = cast(HirSwitch) stmt;
                switchStmt.condition = inlineInExpr(switchStmt.condition, currentFunc);
                foreach (ref caseStmt; switchStmt.cases)
                    caseStmt.body = inlineInBlock(caseStmt.body, currentFunc);
                return switchStmt;
            
            case HirNodeKind.Version:
                auto ver = cast(HirVersion) stmt;
                ver.block = inlineInBlock(ver.block, currentFunc);
                return ver;
            
            case HirNodeKind.Defer:
                auto defer = cast(HirDefer) stmt;
                defer.value = inlineInStmt(defer.value, currentFunc);
                return defer;
            
            case HirNodeKind.Break:
            case HirNodeKind.Continue:
                return stmt;
            
            default:
                return stmt;
        }
    }
    
    HirNode inlineInExpr(HirNode expr, string currentFunc)
    {
        if (expr is null) return null;
        switch (expr.kind)
        {
            case HirNodeKind.CallExpr:
                auto call = cast(HirCallExpr) expr;
                // Tenta fazer inline da chamada se for possível
                // NOTA: Inline de expressões é complexo porque precisamos converter o bloco
                // em uma sequência de statements + expressão final
                // Por enquanto, só processamos argumentos
                
                // TODO: Implementar inline completo de expressões
                // Isso requer extrair o valor de retorno e criar statements temporários         
                foreach (ref arg; call.args)
                    arg = inlineInExpr(arg, currentFunc);
                return call;
            
            case HirNodeKind.Binary:
                auto bin = cast(HirBinary) expr;
                bin.left = inlineInExpr(bin.left, currentFunc);
                bin.right = inlineInExpr(bin.right, currentFunc);
                return bin;
            
            case HirNodeKind.Unary:
                auto un = cast(HirUnary) expr;
                un.operand = inlineInExpr(un.operand, currentFunc);
                return un;
            
            case HirNodeKind.Ternary:
                auto tern = cast(HirTernary) expr;
                tern.condition = inlineInExpr(tern.condition, currentFunc);
                tern.trueExpr = inlineInExpr(tern.trueExpr, currentFunc);
                tern.falseExpr = inlineInExpr(tern.falseExpr, currentFunc);
                return tern;
            
            case HirNodeKind.Cast:
                auto cast_ = cast(HirCast) expr;
                cast_.value = inlineInExpr(cast_.value, currentFunc);
                return cast_;
            
            case HirNodeKind.IndexAccess:
                auto idx = cast(HirIndexAccess) expr;
                idx.target = inlineInExpr(idx.target, currentFunc);
                idx.index = inlineInExpr(idx.index, currentFunc);
                return expr;
            
            case HirNodeKind.IndexExpr:
                auto idx = cast(HirIndexExpr) expr;
                idx.target = inlineInExpr(idx.target, currentFunc);
                idx.index = inlineInExpr(idx.index, currentFunc);
                return expr;
            
            case HirNodeKind.MemberAccess:
                auto mem = cast(HirMemberAccess) expr;
                mem.target = inlineInExpr(mem.target, currentFunc);
                return mem;
            
            case HirNodeKind.ArrayLit:
                auto arr = cast(HirArrayLit) expr;
                foreach (ref elem; arr.elements)
                    elem = inlineInExpr(elem, currentFunc);
                return arr;
            
            case HirNodeKind.StructLit:
                auto str = cast(HirStructLit) expr;
                foreach (ref field; str.fieldValues)
                    field = inlineInExpr(field, currentFunc);
                return str;
            
            case HirNodeKind.Load:
                auto load = cast(HirLoad) expr;
                if (load.ptr !is null)
                    load.ptr = inlineInExpr(load.ptr, currentFunc);
                return load;
            
            case HirNodeKind.AddrOf:
                auto addr = cast(HirAddrOf) expr;
                if (addr.target !is null)
                    addr.target = inlineInExpr(addr.target, currentFunc);
                return addr;
            
            case HirNodeKind.AddrOfComplex:
                auto addr = cast(HirAddrOfComplex) expr;
                addr.expr = inlineInExpr(addr.expr, currentFunc);
                return addr;
            
            case HirNodeKind.Deref:
                auto deref = cast(HirDeref) expr;
                deref.ptr = inlineInExpr(deref.ptr, currentFunc);
                return deref;
            
            case HirNodeKind.AssignExpr:
                auto assign = cast(HirAssignExpr) expr;
                if (assign.assign !is null)
                {
                    assign.assign.target = inlineInExpr(assign.assign.target, currentFunc);
                    assign.assign.value = inlineInExpr(assign.assign.value, currentFunc);
                }
                return assign;
            
            // Literais - não precisam de processamento
            case HirNodeKind.IntLit:
            case HirNodeKind.LongLit:
            case HirNodeKind.FloatLit:
            case HirNodeKind.BoolLit:
            case HirNodeKind.CharLit:
            case HirNodeKind.StringLit:
            case HirNodeKind.NullLit:
            case HirNodeKind.FunctionRef:
                return expr;
            
            default:
                return expr;
        }
    }
    
    HirNode tryInlineCall(HirCallExpr call, string currentFunc)
    {
        if (call.funcName !in inlinableFuncs)
            return null;
        
        // Evita recursão infinita
        if (call.funcName == currentFunc)
        {
            skippedRecursive++;
            return null;
        }
        
        int depth = inlineDepth.get(call.funcName, 0);
        if (depth >= maxInlineDepth)
        {
            skippedRecursive++;
            return null;
        }
        
        // Limita total de inlines
        if (inlineCount >= maxTotalInlines)
            return null;
        
        auto targetFunc = inlinableFuncs[call.funcName];   
        string suffix = "_inline_" ~ to!string(inlineCount);
        
        auto resultBlock = new HirBlock();
        
        // PASSO 1: Criar variáveis temporárias para cada argumento
        foreach (i, paramName; targetFunc.argNames)
            if (i < call.args.length)
            {
                auto paramDecl = new HirVarDecl();
                paramDecl.name = paramName ~ suffix;
                paramDecl.type = targetFunc.argTypes[i];
                paramDecl.initValue = cloneNode(call.args[i]);
                paramDecl.isGlobal = false;
                paramDecl.isConst = false;        
                resultBlock.stmts ~= paramDecl;
            }
        
        // PASSO 2: Cria uma cópia do corpo da função
        auto inlinedBody = cloneBlock(targetFunc.body);        
        // PASSO 3: Renomeia variáveis locais (os parâmetros já foram criados com o suffix)
        renameVariables(inlinedBody, suffix);
        // PASSO 4: Remove returns (para statements, não precisamos do valor)
        removeReturns(inlinedBody, targetFunc.returnType);
        // PASSO 5: Incrementa contadores
        inlineCount++;
        inlineDepth[call.funcName] = depth + 1;
        // PASSO 6: Recursivamente processa o código inline
        inlinedBody = inlineInBlock(inlinedBody, currentFunc);
        inlineDepth[call.funcName] = depth;
        // PASSO 7: Adiciona os statements do corpo ao bloco resultado
        resultBlock.stmts ~= inlinedBody.stmts;
        return resultBlock;
    }
    
    // Tenta fazer inline de uma expressão, retornando statements prefix + expressão final
    InlineExprResult tryInlineExpr(HirNode expr, string currentFunc)
    {
        InlineExprResult result;
        result.expr = expr;
        
        if (expr is null)
            return result;
        
        // Se for uma CallExpr, tenta inline
        if (auto call = cast(HirCallExpr) expr)
        {
            // Verifica se pode fazer inline
            if (call.funcName !in inlinableFuncs)
            {
                result.expr = inlineInExpr(expr, currentFunc);
                return result;
            }
            
            // Evita recursão
            if (call.funcName == currentFunc)
            {
                skippedRecursive++;
                result.expr = inlineInExpr(expr, currentFunc);
                return result;
            }
            
            int depth = inlineDepth.get(call.funcName, 0);
            if (depth >= maxInlineDepth || inlineCount >= maxTotalInlines)
            {
                result.expr = inlineInExpr(expr, currentFunc);
                return result;
            }
            
            auto targetFunc = inlinableFuncs[call.funcName];        
            string suffix = "_inline_" ~ to!string(inlineCount);
            
            // PASSO 1: Criar variáveis temporárias para cada argumento
            foreach (i, paramName; targetFunc.argNames)
                if (i < call.args.length)
                {
                    auto paramDecl = new HirVarDecl();
                    paramDecl.name = paramName ~ suffix;
                    paramDecl.type = targetFunc.argTypes[i];
                    paramDecl.initValue = cloneNode(call.args[i]);
                    paramDecl.isGlobal = false;
                    paramDecl.isConst = false;        
                    result.prefixStmts ~= paramDecl;
                }
            
            // PASSO 2: Clona o corpo da função
            auto inlinedBody = cloneBlock(targetFunc.body);
            // PASSO 3: Renomeia todas as variáveis locais (mas não os parâmetros, já renomeados)
            renameVariables(inlinedBody, suffix);
            // PASSO 4: Incrementa contador
            inlineCount++;
            inlineDepth[call.funcName] = depth + 1;
            // PASSO 5: Processa recursivamente o corpo inline
            inlinedBody = inlineInBlock(inlinedBody, currentFunc);
            inlineDepth[call.funcName] = depth;
            // PASSO 6: Extrai o valor de retorno
            auto returnInfo = extractReturnValue(inlinedBody, targetFunc.returnType, suffix);
            // PASSO 7: Adiciona os statements do corpo inline aos prefix statements
            result.prefixStmts ~= returnInfo.stmts;
            result.expr = returnInfo.returnExpr;
            return result;
        }
        // Para outros tipos de expressão, processa recursivamente
        result.expr = inlineInExpr(expr, currentFunc);
        return result;
    }
    
    // Estrutura para informação de retorno extraída
    struct ReturnInfo
    {
        HirNode[] stmts;        // Todos os statements exceto o return
        HirNode returnExpr;     // A expressão de retorno (ou null para void)
    }
    
    // Extrai o valor de retorno de um bloco inline
    ReturnInfo extractReturnValue(HirBlock block, Type returnType, string suffix)
    {
        ReturnInfo info;
        
        if (block is null || block.stmts.length == 0)
        {
            info.returnExpr = null;
            return info;
        }
        
        // Procura pelo return no final
        HirReturn lastReturn = null;
        
        foreach (i, stmt; block.stmts)
            if (auto ret = cast(HirReturn) stmt)
                // Encontrou return, não adiciona aos statements
                lastReturn = ret;
            else
                info.stmts ~= stmt;
        
        if (lastReturn !is null && lastReturn.value !is null)
            // Retorna a expressão diretamente
            info.returnExpr = lastReturn.value;
        else
            // Função void ou sem retorno explícito
            info.returnExpr = null;
        
        return info;
    }
    
    HirBlock cloneBlock(HirBlock original)
    {
        if (original is null) return null;
        
        auto clone = new HirBlock();
        foreach (stmt; original.stmts)
            clone.stmts ~= cloneNode(stmt);
        return clone;
    }
    
    HirNode cloneNode(HirNode node)
    {
        if (node is null) return null;
        
        switch (node.kind)
        {
            case HirNodeKind.VarDecl:
                auto orig = cast(HirVarDecl) node;
                auto clone = new HirVarDecl();
                clone.name = orig.name;
                clone.type = orig.type;
                clone.isGlobal = orig.isGlobal;
                clone.isConst = orig.isConst;
                clone.initValue = cloneNode(orig.initValue);
                return clone;
            
            case HirNodeKind.AssignDecl:
                auto orig = cast(HirAssignDecl) node;
                auto clone = new HirAssignDecl();
                clone.target = cloneNode(orig.target);
                clone.value = cloneNode(orig.value);
                clone.op = orig.op;
                clone.type = orig.type;
                return clone;
            
            case HirNodeKind.Store:
                auto orig = cast(HirStore) node;
                auto clone = new HirStore();
                clone.ptr = cloneNode(orig.ptr);
                clone.value = cloneNode(orig.value);
                clone.type = orig.type;
                return clone;
            
            case HirNodeKind.Return:
                auto orig = cast(HirReturn) node;
                auto clone = new HirReturn();
                clone.value = cloneNode(orig.value);
                clone.type = orig.type;
                return clone;
            
            case HirNodeKind.If:
                auto orig = cast(HirIf) node;
                auto clone = new HirIf();
                clone.condition = cloneNode(orig.condition);
                clone.thenBlock = cloneBlock(orig.thenBlock);
                clone.elseBlock = cloneBlock(orig.elseBlock);
                clone.type = orig.type;
                return clone;
            
            case HirNodeKind.While:
                auto orig = cast(HirWhile) node;
                auto clone = new HirWhile();
                clone.condition = cloneNode(orig.condition);
                clone.body = cloneBlock(orig.body);
                clone.type = orig.type;
                return clone;
            
            case HirNodeKind.For:
                auto orig = cast(HirFor) node;
                auto clone = new HirFor();
                clone.init_ = cloneNode(orig.init_);
                clone.condition = cloneNode(orig.condition);
                clone.increment = cloneNode(orig.increment);
                clone.body = cloneBlock(orig.body);
                clone.type = orig.type;
                return clone;
            
            case HirNodeKind.Block:
                return cloneBlock(cast(HirBlock) node);
            
            case HirNodeKind.Switch:
                auto orig = cast(HirSwitch) node;
                auto clone = new HirSwitch();
                clone.condition = cloneNode(orig.condition);
                foreach (caseStmt; orig.cases)
                {
                    auto caseClone = new HirCaseStmt();
                    foreach (val; caseStmt.values)
                        caseClone.values ~= cloneNode(val);
                    caseClone.body = cloneBlock(caseStmt.body);
                    caseClone.isDefault = caseStmt.isDefault;
                    clone.cases ~= caseClone;
                }
                clone.type = orig.type;
                return clone;
            
            case HirNodeKind.CallStmt:
                auto orig = cast(HirCallStmt) node;
                auto clone = new HirCallStmt();
                clone.call = cast(HirCallExpr) cloneNode(orig.call);
                clone.type = orig.type;
                return clone;
            
            case HirNodeKind.Version:
                auto orig = cast(HirVersion) node;
                return new HirVersion(cloneBlock(orig.block));
            
            case HirNodeKind.Defer:
                auto orig = cast(HirDefer) node;
                return new HirDefer(cloneNode(orig.value));
            
            case HirNodeKind.Break:
                return new HirBreak();
            
            case HirNodeKind.Continue:
                return new HirContinue();
            
            case HirNodeKind.Load:
                auto orig = cast(HirLoad) node;
                auto clone = new HirLoad();
                clone.ptr = cloneNode(orig.ptr);
                clone.varName = orig.varName;
                clone.type = orig.type;
                return clone;
            
            case HirNodeKind.AddrOf:
                auto orig = cast(HirAddrOf) node;
                auto clone = new HirAddrOf();
                clone.target = cloneNode(orig.target);
                clone.varName = orig.varName;
                clone.type = orig.type;
                return clone;
            
            case HirNodeKind.AddrOfComplex:
                auto orig = cast(HirAddrOfComplex) node;
                return new HirAddrOfComplex(cloneNode(orig.expr), orig.type);
            
            case HirNodeKind.Deref:
                auto orig = cast(HirDeref) node;
                auto clone = new HirDeref();
                clone.ptr = cloneNode(orig.ptr);
                clone.type = orig.type;
                return clone;
            
            case HirNodeKind.IntLit:
                auto orig = cast(HirIntLit) node;
                return new HirIntLit(orig.value, orig.type);
            
            case HirNodeKind.LongLit:
                auto orig = cast(HirLongLit) node;
                return new HirLongLit(orig.value, orig.type);
            
            case HirNodeKind.FloatLit:
                auto orig = cast(HirFloatLit) node;
                return new HirFloatLit(orig.value, orig.type);
            
            case HirNodeKind.BoolLit:
                auto orig = cast(HirBoolLit) node;
                return new HirBoolLit(orig.value, orig.type);
            
            case HirNodeKind.CharLit:
                auto orig = cast(HirCharLit) node;
                return new HirCharLit(orig.value, orig.type);
            
            case HirNodeKind.StringLit:
                auto orig = cast(HirStringLit) node;
                return new HirStringLit(orig.value, orig.type);
            
            case HirNodeKind.NullLit:
                auto orig = cast(HirNullLit) node;
                return new HirNullLit(orig.type);
            
            case HirNodeKind.ArrayLit:
                auto orig = cast(HirArrayLit) node;
                auto clone = new HirArrayLit(orig.type);
                foreach (elem; orig.elements)
                    clone.elements ~= cloneNode(elem);
                return clone;
            
            case HirNodeKind.StructLit:
                auto orig = cast(HirStructLit) node;
                auto clone = new HirStructLit();
                clone.structName = orig.structName;
                clone.isConstructorCall = orig.isConstructorCall;
                clone.type = orig.type;
                foreach (field; orig.fieldValues)
                    clone.fieldValues ~= cloneNode(field);
                return clone;
            
            case HirNodeKind.Binary:
                auto orig = cast(HirBinary) node;
                auto clone = new HirBinary();
                clone.op = orig.op;
                clone.left = cloneNode(orig.left);
                clone.right = cloneNode(orig.right);
                clone.type = orig.type;
                return clone;
            
            case HirNodeKind.Unary:
                auto orig = cast(HirUnary) node;
                auto clone = new HirUnary();
                clone.op = orig.op;
                clone.operand = cloneNode(orig.operand);
                clone.type = orig.type;
                return clone;
            
            case HirNodeKind.Ternary:
                auto orig = cast(HirTernary) node;
                auto clone = new HirTernary();
                clone.condition = cloneNode(orig.condition);
                clone.trueExpr = cloneNode(orig.trueExpr);
                clone.falseExpr = cloneNode(orig.falseExpr);
                clone.type = orig.type;
                return clone;
            
            case HirNodeKind.Cast:
                auto orig = cast(HirCast) node;
                auto clone = new HirCast();
                clone.value = cloneNode(orig.value);
                clone.targetType = orig.targetType;
                clone.type = orig.type;
                return clone;
            
            case HirNodeKind.IndexAccess:
                auto orig = cast(HirIndexAccess) node;
                auto clone = new HirIndexAccess();
                clone.target = cloneNode(orig.target);
                clone.index = cloneNode(orig.index);
                clone.type = orig.type;
                return clone;
            
            case HirNodeKind.IndexExpr:
                auto orig = cast(HirIndexExpr) node;
                auto clone = new HirIndexExpr();
                clone.target = cloneNode(orig.target);
                clone.index = cloneNode(orig.index);
                clone.type = orig.type;
                return clone;
            
            case HirNodeKind.MemberAccess:
                auto orig = cast(HirMemberAccess) node;
                auto clone = new HirMemberAccess();
                clone.target = cloneNode(orig.target);
                clone.memberName = orig.memberName;
                clone.memberOffset = orig.memberOffset;
                clone.type = orig.type;
                return clone;
            
            case HirNodeKind.CallExpr:
                auto orig = cast(HirCallExpr) node;
                auto clone = new HirCallExpr();
                clone.funcName = orig.funcName;
                clone.isVarArg = orig.isVarArg;
                clone.isOrnVarArgs = orig.isOrnVarArgs;
                clone.isExternalCall = orig.isExternalCall;
                clone.isRef = orig.isRef;
                clone.isVarArgAt = orig.isVarArgAt;
                clone.refType = orig.refType;
                clone.type = orig.type;
                foreach (arg; orig.args)
                    clone.args ~= cloneNode(arg);
                return clone;
            
            case HirNodeKind.AssignExpr:
                auto orig = cast(HirAssignExpr) node;
                auto clone = new HirAssignExpr();
                clone.assign = cast(HirAssignDecl) cloneNode(orig.assign);
                clone.type = orig.type;
                return clone;
            
            case HirNodeKind.FunctionRef:
                auto orig = cast(HirFunctionRef) node;
                auto clone = new HirFunctionRef();
                clone.name = orig.name;
                clone.type = orig.type;
                return clone;
            
            default:
                // Fallback: retorna o nó original (não é ideal)
                return node;
        }
    }
    
    void renameVariables(HirBlock block, string suffix)
    {
        if (block is null) return;
        
        foreach (ref stmt; block.stmts)
            renameInStmt(stmt, suffix);
    }
    
    void renameInStmt(HirNode stmt, string suffix)
    {
        if (stmt is null) return;
        
        switch (stmt.kind)
        {
            case HirNodeKind.VarDecl:
                auto decl = cast(HirVarDecl) stmt;
                decl.name ~= suffix;
                renameInExpr(decl.initValue, suffix);
                break;
            
            case HirNodeKind.AssignDecl:
                auto assign = cast(HirAssignDecl) stmt;
                renameInExpr(assign.target, suffix);
                renameInExpr(assign.value, suffix);
                break;
            
            case HirNodeKind.Store:
                auto store = cast(HirStore) stmt;
                renameInExpr(store.ptr, suffix);
                renameInExpr(store.value, suffix);
                break;
            
            case HirNodeKind.Return:
                auto ret = cast(HirReturn) stmt;
                renameInExpr(ret.value, suffix);
                break;
            
            case HirNodeKind.If:
                auto ifStmt = cast(HirIf) stmt;
                renameInExpr(ifStmt.condition, suffix);
                renameVariables(ifStmt.thenBlock, suffix);
                renameVariables(ifStmt.elseBlock, suffix);
                break;
            
            case HirNodeKind.While:
                auto whileStmt = cast(HirWhile) stmt;
                renameInExpr(whileStmt.condition, suffix);
                renameVariables(whileStmt.body, suffix);
                break;
            
            case HirNodeKind.For:
                auto forStmt = cast(HirFor) stmt;
                renameInStmt(forStmt.init_, suffix);
                renameInExpr(forStmt.condition, suffix);
                renameInExpr(forStmt.increment, suffix);
                renameVariables(forStmt.body, suffix);
                break;
            
            case HirNodeKind.Block:
                renameVariables(cast(HirBlock) stmt, suffix);
                break;
            
            case HirNodeKind.Switch:
                auto switchStmt = cast(HirSwitch) stmt;
                renameInExpr(switchStmt.condition, suffix);
                foreach (caseStmt; switchStmt.cases)
                {
                    foreach (val; caseStmt.values)
                        renameInExpr(val, suffix);
                    renameVariables(caseStmt.body, suffix);
                }
                break;
            
            case HirNodeKind.CallStmt:
                auto call = cast(HirCallStmt) stmt;
                renameInExpr(call.call, suffix);
                break;
            
            case HirNodeKind.Version:
                auto ver = cast(HirVersion) stmt;
                renameVariables(ver.block, suffix);
                break;
            
            case HirNodeKind.Defer:
                auto defer = cast(HirDefer) stmt;
                renameInStmt(defer.value, suffix);
                break;
            
            default:
                break;
        }
    }
    
    void renameInExpr(HirNode expr, string suffix)
    {
        if (expr is null) return;
        
        switch (expr.kind)
        {
            case HirNodeKind.Load:
                auto load = cast(HirLoad) expr;
                load.varName ~= suffix;
                renameInExpr(load.ptr, suffix);
                break;
            
            case HirNodeKind.AddrOf:
                auto addr = cast(HirAddrOf) expr;
                addr.varName ~= suffix;
                renameInExpr(addr.target, suffix);
                break;
            
            case HirNodeKind.AddrOfComplex:
                auto addr = cast(HirAddrOfComplex) expr;
                renameInExpr(addr.expr, suffix);
                break;
            
            case HirNodeKind.Deref:
                auto deref = cast(HirDeref) expr;
                renameInExpr(deref.ptr, suffix);
                break;
            
            case HirNodeKind.Binary:
                auto bin = cast(HirBinary) expr;
                renameInExpr(bin.left, suffix);
                renameInExpr(bin.right, suffix);
                break;
            
            case HirNodeKind.Unary:
                auto un = cast(HirUnary) expr;
                renameInExpr(un.operand, suffix);
                break;
            
            case HirNodeKind.Ternary:
                auto tern = cast(HirTernary) expr;
                renameInExpr(tern.condition, suffix);
                renameInExpr(tern.trueExpr, suffix);
                renameInExpr(tern.falseExpr, suffix);
                break;
            
            case HirNodeKind.Cast:
                auto cast_ = cast(HirCast) expr;
                renameInExpr(cast_.value, suffix);
                break;
            
            case HirNodeKind.IndexAccess:
                auto idx = cast(HirIndexAccess) expr;
                renameInExpr(idx.target, suffix);
                renameInExpr(idx.index, suffix);
                break;
            
            case HirNodeKind.IndexExpr:
                auto idx = cast(HirIndexExpr) expr;
                renameInExpr(idx.target, suffix);
                renameInExpr(idx.index, suffix);
                break;
            
            case HirNodeKind.MemberAccess:
                auto mem = cast(HirMemberAccess) expr;
                renameInExpr(mem.target, suffix);
                break;
            
            case HirNodeKind.CallExpr:
                auto call = cast(HirCallExpr) expr;
                foreach (arg; call.args)
                    renameInExpr(arg, suffix);
                break;
            
            case HirNodeKind.ArrayLit:
                auto arr = cast(HirArrayLit) expr;
                foreach (elem; arr.elements)
                    renameInExpr(elem, suffix);
                break;
            
            case HirNodeKind.StructLit:
                auto str = cast(HirStructLit) expr;
                foreach (field; str.fieldValues)
                    renameInExpr(field, suffix);
                break;
            
            case HirNodeKind.AssignExpr:
                auto assign = cast(HirAssignExpr) expr;
                if (assign.assign !is null)
                {
                    renameInExpr(assign.assign.target, suffix);
                    renameInExpr(assign.assign.value, suffix);
                }
                break;
            
            default:
                break;
        }
    }
    
    void substituteParameters(HirBlock block, string[] paramNames, HirNode[] args)
    {
        // Cria mapa de substituições
        HirNode[string] substitutions;
        foreach (i, paramName; paramNames)
            if (i < args.length)
                substitutions[paramName] = args[i];
        
        // Aplica substituições
        foreach (ref stmt; block.stmts)
            substituteInStmt(stmt, substitutions);
    }
    
    void substituteInStmt(HirNode stmt, HirNode[string] substitutions)
    {
        if (stmt is null) return;
        
        switch (stmt.kind)
        {
            case HirNodeKind.VarDecl:
                auto decl = cast(HirVarDecl) stmt;
                if (decl.initValue !is null)
                    decl.initValue = substituteInExpr(decl.initValue, substitutions);
                break;
            
            case HirNodeKind.AssignDecl:
                auto assign = cast(HirAssignDecl) stmt;
                assign.target = substituteInExpr(assign.target, substitutions);
                assign.value = substituteInExpr(assign.value, substitutions);
                break;
            
            case HirNodeKind.Store:
                auto store = cast(HirStore) stmt;
                store.ptr = substituteInExpr(store.ptr, substitutions);
                store.value = substituteInExpr(store.value, substitutions);
                break;
            
            case HirNodeKind.Return:
                auto ret = cast(HirReturn) stmt;
                if (ret.value !is null)
                    ret.value = substituteInExpr(ret.value, substitutions);
                break;
            
            case HirNodeKind.If:
                auto ifStmt = cast(HirIf) stmt;
                ifStmt.condition = substituteInExpr(ifStmt.condition, substitutions);
                foreach (ref s; ifStmt.thenBlock.stmts)
                    substituteInStmt(s, substitutions);
                if (ifStmt.elseBlock !is null)
                    foreach (ref s; ifStmt.elseBlock.stmts)
                        substituteInStmt(s, substitutions);
                break;
            
            case HirNodeKind.While:
                auto whileStmt = cast(HirWhile) stmt;
                whileStmt.condition = substituteInExpr(whileStmt.condition, substitutions);
                foreach (ref s; whileStmt.body.stmts)
                    substituteInStmt(s, substitutions);
                break;
            
            case HirNodeKind.For:
                auto forStmt = cast(HirFor) stmt;
                if (forStmt.init_ !is null)
                    substituteInStmt(forStmt.init_, substitutions);
                if (forStmt.condition !is null)
                    forStmt.condition = substituteInExpr(forStmt.condition, substitutions);
                if (forStmt.increment !is null)
                    forStmt.increment = substituteInExpr(forStmt.increment, substitutions);
                foreach (ref s; forStmt.body.stmts)
                    substituteInStmt(s, substitutions);
                break;
            
            case HirNodeKind.Block:
                auto block = cast(HirBlock) stmt;
                foreach (ref s; block.stmts)
                    substituteInStmt(s, substitutions);
                break;
            
            case HirNodeKind.Switch:
                auto switchStmt = cast(HirSwitch) stmt;
                switchStmt.condition = substituteInExpr(switchStmt.condition, substitutions);
                foreach (caseStmt; switchStmt.cases)
                {
                    foreach (ref val; caseStmt.values)
                        val = substituteInExpr(val, substitutions);
                    foreach (ref s; caseStmt.body.stmts)
                        substituteInStmt(s, substitutions);
                }
                break;
            
            case HirNodeKind.CallStmt:
                auto call = cast(HirCallStmt) stmt;
                call.call = cast(HirCallExpr) substituteInExpr(call.call, substitutions);
                break;
            
            case HirNodeKind.Version:
                auto ver = cast(HirVersion) stmt;
                foreach (ref s; ver.block.stmts)
                    substituteInStmt(s, substitutions);
                break;
            
            case HirNodeKind.Defer:
                auto defer = cast(HirDefer) stmt;
                substituteInStmt(defer.value, substitutions);
                break;
            
            default:
                break;
        }
    }
    
    HirNode substituteInExpr(HirNode expr, HirNode[string] substitutions)
    {
        if (expr is null) return null;
        
        switch (expr.kind)
        {
            case HirNodeKind.Load:
                auto load = cast(HirLoad) expr;
                if (load.varName in substitutions)
                    return cloneNode(substitutions[load.varName]);
                if (load.ptr !is null)
                    load.ptr = substituteInExpr(load.ptr, substitutions);
                return load;
            
            case HirNodeKind.AddrOf:
                auto addr = cast(HirAddrOf) expr;
                if (addr.varName in substitutions)
                {
                    // Criar AddrOf do argumento substituído
                    auto newAddr = new HirAddrOf();
                    newAddr.target = cloneNode(substitutions[addr.varName]);
                    newAddr.type = addr.type;
                    return newAddr;
                }
                if (addr.target !is null)
                    addr.target = substituteInExpr(addr.target, substitutions);
                return addr;
            
            case HirNodeKind.AddrOfComplex:
                auto addr = cast(HirAddrOfComplex) expr;
                addr.expr = substituteInExpr(addr.expr, substitutions);
                return addr;
            
            case HirNodeKind.Deref:
                auto deref = cast(HirDeref) expr;
                deref.ptr = substituteInExpr(deref.ptr, substitutions);
                return deref;
            
            case HirNodeKind.Binary:
                auto bin = cast(HirBinary) expr;
                bin.left = substituteInExpr(bin.left, substitutions);
                bin.right = substituteInExpr(bin.right, substitutions);
                return bin;
            
            case HirNodeKind.Unary:
                auto un = cast(HirUnary) expr;
                un.operand = substituteInExpr(un.operand, substitutions);
                return un;
            
            case HirNodeKind.Ternary:
                auto tern = cast(HirTernary) expr;
                tern.condition = substituteInExpr(tern.condition, substitutions);
                tern.trueExpr = substituteInExpr(tern.trueExpr, substitutions);
                tern.falseExpr = substituteInExpr(tern.falseExpr, substitutions);
                return tern;
            
            case HirNodeKind.Cast:
                auto cast_ = cast(HirCast) expr;
                cast_.value = substituteInExpr(cast_.value, substitutions);
                return cast_;
            
            case HirNodeKind.IndexAccess:
                auto idx = cast(HirIndexAccess) expr;
                idx.target = substituteInExpr(idx.target, substitutions);
                idx.index = substituteInExpr(idx.index, substitutions);
                return idx;
            
            case HirNodeKind.IndexExpr:
                auto idx = cast(HirIndexExpr) expr;
                idx.target = substituteInExpr(idx.target, substitutions);
                idx.index = substituteInExpr(idx.index, substitutions);
                return idx;
            
            case HirNodeKind.MemberAccess:
                auto mem = cast(HirMemberAccess) expr;
                mem.target = substituteInExpr(mem.target, substitutions);
                return mem;
            
            case HirNodeKind.CallExpr:
                auto call = cast(HirCallExpr) expr;
                foreach (ref arg; call.args)
                    arg = substituteInExpr(arg, substitutions);
                return call;
            
            case HirNodeKind.ArrayLit:
                auto arr = cast(HirArrayLit) expr;
                foreach (ref elem; arr.elements)
                    elem = substituteInExpr(elem, substitutions);
                return arr;
            
            case HirNodeKind.StructLit:
                auto str = cast(HirStructLit) expr;
                foreach (ref field; str.fieldValues)
                    field = substituteInExpr(field, substitutions);
                return str;
            
            case HirNodeKind.AssignExpr:
                auto assign = cast(HirAssignExpr) expr;
                if (assign.assign !is null)
                {
                    assign.assign.target = substituteInExpr(assign.assign.target, substitutions);
                    assign.assign.value = substituteInExpr(assign.assign.value, substitutions);
                }
                return assign;
            
            default:
                return expr;
        }
    }
    
    void removeReturns(HirBlock block, Type returnType)
    {
        // Remove returns em funções void
        // Para funções com retorno, seria necessário converter em assignment  
        if (cast(PrimitiveType) returnType)
        {
            auto prim = cast(PrimitiveType) returnType;
            if (prim.baseType != BaseType.Void)
                return; // Não remove returns de funções não-void por simplicidade
        }
        HirNode[] newStmts;
        foreach (stmt; block.stmts)
        {
            if (stmt.kind != HirNodeKind.Return)
                newStmts ~= stmt;
            else
            {
                // Para funções void, apenas remove o return
                // Se tiver valor de retorno, mantém a expressão como statement
                auto ret = cast(HirReturn) stmt;
                if (ret.value !is null)
                {
                    // Converte em CallStmt se for uma chamada
                    if (auto call = cast(HirCallExpr) ret.value)
                    {
                        auto callStmt = new HirCallStmt();
                        callStmt.call = call;
                        newStmts ~= callStmt;
                    }
                }
            }
        }
        block.stmts = newStmts;
    }
}
