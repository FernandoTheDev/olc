module middle.opt.dead_code;

import frontend.parser.ast;
import middle.hir.hir;
import frontend.types.type;
import std.stdio : writeln;
import std.algorithm : canFind;

/// Eliminação de Código Morto (Dead Code Elimination)
/// Remove código que nunca será executado ou variáveis que nunca são usadas
class DeadCode
{
    // Rastreia variáveis usadas
    bool[string] usedVars;
    
    // Rastreia funções chamadas
    bool[string] calledFuncs;
    
    // Rastreia se uma variável é escrita (para detectar write-only vars)
    bool[string] writtenVars;
    
    // Contador de otimizações
    int eliminatedStmts = 0;
    int eliminatedVars = 0;
    int eliminatedFuncs = 0;
    
    HirProgram optimize(HirProgram program)
    {
        eliminatedStmts = 0;
        eliminatedVars = 0;
        eliminatedFuncs = 0;
        usedVars.clear();
        calledFuncs.clear();
        writtenVars.clear();
        
        // Primeira passagem: marca código alcançável
        markReachableCode(program);
        
        // Segunda passagem: elimina código morto
        program = eliminateDeadCode(program);
        
        return program;
    }
    
private:
    
    void markReachableCode(HirProgram program)
    {
        // main é sempre alcançável
        calledFuncs["main"] = true;
        
        // Marca todas as funções globais que possam ser chamadas externamente
        foreach (global; program.globals)
        {
            if (auto func = cast(HirFunction) global)
            {
                // Funções exportadas ou main são sempre alcançáveis
                if (func.name == "main")
                {
                    markFunctionReachable(func);
                }
            }
            // Marca variáveis globais constantes como usadas (podem ser referenciadas externamente)
            else if (auto varDecl = cast(HirVarDecl) global)
            {
                if (varDecl.isConst && varDecl.initValue !is null)
                {
                    usedVars[varDecl.name] = true;
                    markExprReachable(varDecl.initValue);
                }
            }
        }
        
        // Itera até não haver mais mudanças (point fixo)
        // porque funções podem chamar outras funções
        bool changed = true;
        while (changed)
        {
            changed = false;
            foreach (global; program.globals)
            {
                if (auto func = cast(HirFunction) global)
                {
                    if (func.name in calledFuncs && func.body !is null)
                    {
                        size_t oldSize = calledFuncs.length;
                        markFunctionReachable(func);
                        if (calledFuncs.length > oldSize)
                            changed = true;
                    }
                }
            }
        }
    }
    
    void markFunctionReachable(HirFunction func)
    {
        if (func.body !is null)
            markBlockReachable(func.body);
    }
    
    void markBlockReachable(HirBlock block)
    {
        if (block is null) return;
        
        foreach (stmt; block.stmts)
            markStmtReachable(stmt);
    }
    
    void markStmtReachable(HirNode stmt)
    {
        if (stmt is null) return;
        
        switch (stmt.kind)
        {
            case HirNodeKind.CallStmt:
                auto call = cast(HirCallStmt) stmt;
                markExprReachable(call.call);
                break;
            
            case HirNodeKind.VarDecl:
                auto decl = cast(HirVarDecl) stmt;
                // Marca a variável como escrita
                writtenVars[decl.name] = true;
                if (decl.initValue !is null)
                    markExprReachable(decl.initValue);
                break;
            
            case HirNodeKind.AssignDecl:
                auto assign = cast(HirAssignDecl) stmt;
                markLValueAsWritten(assign.target);
                markExprReachable(assign.target);
                markExprReachable(assign.value);
                break;
            
            case HirNodeKind.Return:
                auto ret = cast(HirReturn) stmt;
                if (ret.value !is null)
                    markExprReachable(ret.value);
                break;
            
            case HirNodeKind.If:
                auto ifStmt = cast(HirIf) stmt;
                markExprReachable(ifStmt.condition);
                markBlockReachable(ifStmt.thenBlock);
                if (ifStmt.elseBlock !is null)
                    markBlockReachable(ifStmt.elseBlock);
                break;
            
            case HirNodeKind.While:
                auto whileStmt = cast(HirWhile) stmt;
                markExprReachable(whileStmt.condition);
                markBlockReachable(whileStmt.body);
                break;
            
            case HirNodeKind.For:
                auto forStmt = cast(HirFor) stmt;
                if (forStmt.init_ !is null)
                    markStmtReachable(forStmt.init_);
                if (forStmt.condition !is null)
                    markExprReachable(forStmt.condition);
                if (forStmt.increment !is null)
                    markExprReachable(forStmt.increment);
                markBlockReachable(forStmt.body);
                break;
            
            case HirNodeKind.Block:
                markBlockReachable(cast(HirBlock) stmt);
                break;
            
            case HirNodeKind.Switch:
                auto switchStmt = cast(HirSwitch) stmt;
                markExprReachable(switchStmt.condition);
                foreach (caseStmt; switchStmt.cases)
                {
                    foreach (value; caseStmt.values)
                        markExprReachable(value);
                    markBlockReachable(caseStmt.body);
                }
                break;
            
            case HirNodeKind.Defer:
                auto defer = cast(HirDefer) stmt;
                markStmtReachable(defer.value);
                break;
            
            case HirNodeKind.Store:
                auto store = cast(HirStore) stmt;
                markLValueAsWritten(store.ptr);
                markExprReachable(store.ptr);
                markExprReachable(store.value);
                break;
            
            default:
                break;
        }
    }
    
    void markLValueAsWritten(HirNode lvalue)
    {
        if (lvalue is null) return;
        
        if (auto addr = cast(HirAddrOf) lvalue)
            writtenVars[addr.varName] = true;
        else if (auto load = cast(HirLoad) lvalue)
            writtenVars[load.varName] = true;
    }
    
    void markExprReachable(HirNode expr)
    {
        if (expr is null) return;
        
        switch (expr.kind)
        {
            case HirNodeKind.Load:
                auto load = cast(HirLoad) expr;
                usedVars[load.varName] = true;
                break;
            
            case HirNodeKind.AddrOf:
                auto addr = cast(HirAddrOf) expr;
                usedVars[addr.varName] = true;
                break;
            
            case HirNodeKind.CallExpr:
                auto call = cast(HirCallExpr) expr;
                calledFuncs[call.funcName] = true;
                foreach (arg; call.args)
                    markExprReachable(arg);
                break;
            
            case HirNodeKind.Binary:
                auto bin = cast(HirBinary) expr;
                markExprReachable(bin.left);
                markExprReachable(bin.right);
                break;
            
            case HirNodeKind.Unary:
                auto un = cast(HirUnary) expr;
                markExprReachable(un.operand);
                break;
            
            case HirNodeKind.Ternary:
                auto tern = cast(HirTernary) expr;
                markExprReachable(tern.condition);
                markExprReachable(tern.trueExpr);
                markExprReachable(tern.falseExpr);
                break;
            
            case HirNodeKind.Cast:
                auto cast_ = cast(HirCast) expr;
                markExprReachable(cast_.value);
                break;
            
            case HirNodeKind.IndexAccess:
            case HirNodeKind.IndexExpr:
                if (auto idx = cast(HirIndexAccess) expr)
                {
                    markExprReachable(idx.target);
                    markExprReachable(idx.index);
                }
                else if (auto idx = cast(HirIndexExpr) expr)
                {
                    markExprReachable(idx.target);
                    markExprReachable(idx.index);
                }
                break;
            
            case HirNodeKind.MemberAccess:
                auto mem = cast(HirMemberAccess) expr;
                markExprReachable(mem.target);
                break;
            
            case HirNodeKind.Deref:
                auto deref = cast(HirDeref) expr;
                markExprReachable(deref.ptr);
                break;
            
            case HirNodeKind.AddrOfComplex:
                auto addr = cast(HirAddrOfComplex) expr;
                markExprReachable(addr.expr);
                break;
            
            case HirNodeKind.ArrayLit:
                auto arr = cast(HirArrayLit) expr;
                foreach (elem; arr.elements)
                    markExprReachable(elem);
                break;
            
            case HirNodeKind.StructLit:
                auto str = cast(HirStructLit) expr;
                foreach (field; str.fieldValues)
                    markExprReachable(field);
                break;
            
            case HirNodeKind.AssignExpr:
                auto assign = cast(HirAssignExpr) expr;
                markLValueAsWritten(assign.assign.target);
                markExprReachable(assign.assign.target);
                markExprReachable(assign.assign.value);
                break;
            
            case HirNodeKind.FunctionRef:
                auto fnRef = cast(HirFunctionRef) expr;
                calledFuncs[fnRef.name] = true;
                break;
            
            // Literais não precisam de processamento
            case HirNodeKind.IntLit:
            case HirNodeKind.LongLit:
            case HirNodeKind.FloatLit:
            case HirNodeKind.BoolLit:
            case HirNodeKind.CharLit:
            case HirNodeKind.StringLit:
            case HirNodeKind.NullLit:
                break;
            
            default:
                break;
        }
    }
    
    HirProgram eliminateDeadCode(HirProgram program)
    {
        HirNode[] liveGlobals;
        
        foreach (global; program.globals)
        {
            // Mantém declarações de tipo (sempre necessárias para o sistema de tipos)
            if (global.kind == HirNodeKind.StructDecl ||
                global.kind == HirNodeKind.UnionDecl ||
                global.kind == HirNodeKind.EnumDecl)
            {
                liveGlobals ~= global;
                continue;
            }
            
            // Remove funções não chamadas
            if (auto func = cast(HirFunction) global)
            {
                if (func.name in calledFuncs)
                {
                    func.body = eliminateDeadBlock(func.body);
                    liveGlobals ~= func;
                }
                else
                {
                    eliminatedFuncs++;
                    // writeln("Eliminada função morta: ", func.name);
                }
                continue;
            }
            
            // Remove variáveis globais não usadas
            if (auto varDecl = cast(HirVarDecl) global)
            {
                // Mantém se: é usada OU é const (pode ser referenciada externamente)
                if (varDecl.name in usedVars || varDecl.isConst)
                {
                    liveGlobals ~= varDecl;
                }
                else
                {
                    eliminatedVars++;
                    // writeln("Eliminada variável global morta: ", varDecl.name);
                }
                continue;
            }
            
            // Outros nós globais (imports, etc)
            liveGlobals ~= global;
        }
        
        program.globals = liveGlobals;
        return program;
    }
    
    HirBlock eliminateDeadBlock(HirBlock block)
    {
        if (block is null) return null;
        
        HirNode[] liveStmts;
        bool unreachable = false;
        
        foreach (stmt; block.stmts)
        {
            // Após um return/break/continue, código é inalcançável
            if (unreachable)
            {
                eliminatedStmts++;
                // writeln("Eliminado statement inalcançável");
                continue;
            }
            
            auto eliminated = eliminateDeadStmt(stmt);
            if (eliminated !is null)
                liveStmts ~= eliminated;
            // else: stmt foi eliminado (já contado em eliminatedStmts/eliminatedVars)
            
            // Marca código após return/break/continue como morto
            if (stmt.kind == HirNodeKind.Return ||
                stmt.kind == HirNodeKind.Break ||
                stmt.kind == HirNodeKind.Continue)
            {
                unreachable = true;
            }
        }
        
        block.stmts = liveStmts;
        return block;
    }
    
    HirNode eliminateDeadStmt(HirNode stmt)
    {
        if (stmt is null) return null;
        
        switch (stmt.kind)
        {
            case HirNodeKind.VarDecl:
                auto decl = cast(HirVarDecl) stmt;
                
                // Mantém se:
                // 1. A variável é usada (lida)
                // 2. Ou tem efeitos colaterais na inicialização
                // 3. Ou é uma const (pode ser usada em tempo de compilação)
                bool* isUsed = decl.name in usedVars;
                bool hasSideEffects = hasEffects(decl.initValue);
                
                if (!isUsed && !hasSideEffects && !decl.isConst)
                {
                    eliminatedVars++;
                    // writeln("Eliminada variável local morta: ", decl.name);
                    return null;
                }
                
                return decl;
            
            case HirNodeKind.AssignDecl:
                auto assign = cast(HirAssignDecl) stmt;
                
                // Se a atribuição tem efeitos colaterais, mantém
                if (hasEffects(assign.value) || hasEffects(assign.target))
                    return stmt;
                
                // Verifica se o target é uma variável morta
                if (auto addr = cast(HirAddrOf) assign.target)
                {
                    if (addr.varName !in usedVars)
                    {
                        eliminatedStmts++;
                        // writeln("Eliminada atribuição morta para: ", addr.varName);
                        return null;
                    }
                }
                
                return stmt;
            
            case HirNodeKind.If:
                auto ifStmt = cast(HirIf) stmt;
                ifStmt.thenBlock = eliminateDeadBlock(ifStmt.thenBlock);
                if (ifStmt.elseBlock !is null)
                    ifStmt.elseBlock = eliminateDeadBlock(ifStmt.elseBlock);
                
                // Se ambos os blocos estão vazios e a condição não tem efeitos, remove o if
                if (ifStmt.thenBlock.stmts.length == 0 && 
                    (ifStmt.elseBlock is null || ifStmt.elseBlock.stmts.length == 0) &&
                    !hasEffects(ifStmt.condition))
                {
                    eliminatedStmts++;
                    return null;
                }
                
                return ifStmt;
            
            case HirNodeKind.While:
                auto whileStmt = cast(HirWhile) stmt;
                whileStmt.body = eliminateDeadBlock(whileStmt.body);
                return whileStmt;
            
            case HirNodeKind.For:
                auto forStmt = cast(HirFor) stmt;
                forStmt.init_ = eliminateDeadStmt(forStmt.init_);
                forStmt.body = eliminateDeadBlock(forStmt.body);
                return forStmt;
            
            case HirNodeKind.Block:
                return eliminateDeadBlock(cast(HirBlock) stmt);
            
            case HirNodeKind.Switch:
                auto switchStmt = cast(HirSwitch) stmt;
                foreach (ref caseStmt; switchStmt.cases)
                    caseStmt.body = eliminateDeadBlock(caseStmt.body);
                return switchStmt;
            
            case HirNodeKind.CallStmt:
                // Sempre mantém chamadas de função (podem ter efeitos colaterais)
                return stmt;
            
            case HirNodeKind.Return:
            case HirNodeKind.Break:
            case HirNodeKind.Continue:
                return stmt;
            
            case HirNodeKind.Defer:
                auto defer = cast(HirDefer) stmt;
                defer.value = eliminateDeadStmt(defer.value);
                return defer;
            
            case HirNodeKind.Store:
                // Store sempre tem efeitos colaterais
                return stmt;
            
            default:
                return stmt;
        }
    }
    
    bool hasEffects(HirNode expr)
    {
        if (expr is null) return false;
        
        switch (expr.kind)
        {
            // Chamadas de função sempre têm efeitos colaterais potenciais
            case HirNodeKind.CallExpr:
                return true;
            
            // Atribuições têm efeitos
            case HirNodeKind.AssignDecl:
            case HirNodeKind.AssignExpr:
            case HirNodeKind.Store:
                return true;
            
            // Operadores unários de incremento/decremento têm efeitos
            case HirNodeKind.Unary:
                auto un = cast(HirUnary) expr;
                if (un.op == "++" || un.op == "--")
                    return true;
                return hasEffects(un.operand);
            
            // Recursivamente verifica sub-expressões
            case HirNodeKind.Binary:
                auto bin = cast(HirBinary) expr;
                return hasEffects(bin.left) || hasEffects(bin.right);
            
            case HirNodeKind.Ternary:
                auto tern = cast(HirTernary) expr;
                return hasEffects(tern.condition) || 
                       hasEffects(tern.trueExpr) || 
                       hasEffects(tern.falseExpr);
            
            case HirNodeKind.IndexAccess:
            case HirNodeKind.IndexExpr:
                // Acesso a array pode ter efeitos se usar operador sobrecarregado
                if (auto idx = cast(HirIndexAccess) expr)
                    return hasEffects(idx.target) || hasEffects(idx.index);
                else if (auto idx = cast(HirIndexExpr) expr)
                    return hasEffects(idx.target) || hasEffects(idx.index);
                return false;
            
            case HirNodeKind.MemberAccess:
                auto mem = cast(HirMemberAccess) expr;
                return hasEffects(mem.target);
            
            case HirNodeKind.Cast:
                auto cast_ = cast(HirCast) expr;
                return hasEffects(cast_.value);
            
            case HirNodeKind.Deref:
                auto deref = cast(HirDeref) expr;
                return hasEffects(deref.ptr);
            
            case HirNodeKind.AddrOfComplex:
                auto addr = cast(HirAddrOfComplex) expr;
                return hasEffects(addr.expr);
            
            case HirNodeKind.ArrayLit:
                auto arr = cast(HirArrayLit) expr;
                foreach (elem; arr.elements)
                    if (hasEffects(elem))
                        return true;
                return false;
            
            case HirNodeKind.StructLit:
                auto str = cast(HirStructLit) expr;
                foreach (field; str.fieldValues)
                    if (hasEffects(field))
                        return true;
                return false;
            
            // Literais e loads não têm efeitos
            case HirNodeKind.IntLit:
            case HirNodeKind.LongLit:
            case HirNodeKind.FloatLit:
            case HirNodeKind.BoolLit:
            case HirNodeKind.CharLit:
            case HirNodeKind.StringLit:
            case HirNodeKind.NullLit:
            case HirNodeKind.Load:
            case HirNodeKind.AddrOf:
            case HirNodeKind.FunctionRef:
                return false;
            
            default:
                // Por segurança, assume que tem efeitos se não conhecemos o nó
                return true;
        }
    }
}
