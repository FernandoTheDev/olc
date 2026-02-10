module middle.opt.constant_folding;

import frontend.parser.ast;
import middle.hir.hir;
import frontend.types.type;
import std.stdio : writeln;
import std.conv : to;
import std.math : pow, sqrt, sin, cos, tan, log, exp, abs, floor, ceil, round;

class ConstantFolding
{
    // Tabela de símbolos para rastrear valores constantes conhecidos
    HirNode[string] constantSymbols;
    
    // Rastreia se uma variável é mutável (foi modificada após declaração)
    bool[string] mutableVars;
    
    // Contador de otimizações realizadas
    int optimizationCount = 0;
    
    // Aplica constant folding em um programa HIR completo
    HirProgram optimize(HirProgram program)
    {
        optimizationCount = 0;
        constantSymbols.clear();
        mutableVars.clear();
        
        // Primeira passagem: identifica constantes globais
        foreach (ref global; program.globals)
        {
            if (auto varDecl = cast(HirVarDecl) global)
            {
                if (varDecl.isConst && varDecl.initValue !is null)
                {
                    auto folded = foldNode(varDecl.initValue);
                    if (isConstantValue(folded))
                    {
                        constantSymbols[varDecl.name] = folded;
                        varDecl.initValue = folded;
                    }
                }
            }
        }
        
        // Segunda passagem: otimiza funções
        foreach (ref global; program.globals)
        {
            if (auto func = cast(HirFunction) global)
                global = optimizeFunction(func);
        }
        
        return program;
    }
    
private:
    
    HirFunction optimizeFunction(HirFunction func)
    {
        // Salva estado anterior de símbolos
        auto savedSymbols = constantSymbols.dup;
        auto savedMutable = mutableVars.dup;
        
        // Marca parâmetros como mutáveis
        foreach (argName; func.argNames)
            mutableVars[argName] = true;
        
        if (func.body !is null)
            func.body = optimizeBlock(func.body);
        
        // Restaura estado
        constantSymbols = savedSymbols;
        mutableVars = savedMutable;
        
        return func;
    }
    
    HirBlock optimizeBlock(HirBlock block)
    {
        auto savedSymbols = constantSymbols.dup;
        auto savedMutable = mutableVars.dup;
        
        HirNode[] optimizedStmts;
        
        foreach (stmt; block.stmts)
        {
            auto optimized = optimizeStmt(stmt);
            if (optimized !is null)
                optimizedStmts ~= optimized;
        }
        
        block.stmts = optimizedStmts;
        
        // Restaura símbolos no fim do bloco (escopo)
        constantSymbols = savedSymbols;
        mutableVars = savedMutable;
        
        return block;
    }
    
    HirNode optimizeStmt(HirNode stmt)
    {
        if (stmt is null) return null;
        
        switch (stmt.kind)
        {
            case HirNodeKind.VarDecl:
                return optimizeVarDecl(cast(HirVarDecl) stmt);
            
            case HirNodeKind.AssignDecl:
                return optimizeAssign(cast(HirAssignDecl) stmt);
            
            case HirNodeKind.Store:
                auto store = cast(HirStore) stmt;
                store.value = foldNode(store.value);
                return store;
            
            case HirNodeKind.Return:
                auto ret = cast(HirReturn) stmt;
                if (ret.value !is null)
                    ret.value = foldNode(ret.value);
                return ret;
            
            case HirNodeKind.If:
                return optimizeIf(cast(HirIf) stmt);
            
            case HirNodeKind.While:
                return optimizeWhile(cast(HirWhile) stmt);
            
            case HirNodeKind.For:
                return optimizeFor(cast(HirFor) stmt);
            
            case HirNodeKind.CallStmt:
                auto call = cast(HirCallStmt) stmt;
                call.call = cast(HirCallExpr) foldNode(call.call);
                return call;
            
            case HirNodeKind.Block:
                return optimizeBlock(cast(HirBlock) stmt);
            
            case HirNodeKind.Switch:
                return optimizeSwitch(cast(HirSwitch) stmt);
            
            case HirNodeKind.Defer:
                auto defer = cast(HirDefer) stmt;
                defer.value = optimizeStmt(defer.value);
                return defer;
            
            default:
                return stmt;
        }
    }
    
    HirNode optimizeVarDecl(HirVarDecl decl)
    {
        if (decl.initValue !is null)
        {
            decl.initValue = foldNode(decl.initValue);
            
            // Se é const e tem valor constante, guarda
            if (isConstantValue(decl.initValue))
                constantSymbols[decl.name] = decl.initValue;
            // Se não é const, marca como mutável
            else if (!decl.isConst)
                mutableVars[decl.name] = true;
        }
        
        return decl;
    }
    
    HirNode optimizeAssign(HirAssignDecl assign)
    {
        assign.value = foldNode(assign.value);
        
        // Se atribuindo a uma variável, marca como mutável
        if (auto addr = cast(HirAddrOf) assign.target)
        {
            mutableVars[addr.varName] = true;
            constantSymbols.remove(addr.varName);
        }
        
        return assign;
    }
    
    HirNode optimizeIf(HirIf ifStmt)
    {
        ifStmt.condition = foldNode(ifStmt.condition);
        
        // Se condição é constante, elimina branch morto
        if (auto boolLit = cast(HirBoolLit) ifStmt.condition)
        {
            optimizationCount++;
            
            if (boolLit.value)
            {
                // Condição sempre verdadeira: retorna apenas thenBlock
                auto block = new HirBlock();
                block.stmts = ifStmt.thenBlock.stmts;
                return optimizeBlock(block);
            }
            else
            {
                // Condição sempre falsa: retorna elseBlock ou nada
                if (ifStmt.elseBlock !is null)
                {
                    auto block = new HirBlock();
                    block.stmts = ifStmt.elseBlock.stmts;
                    return optimizeBlock(block);
                }
                return null; // Remove o if inteiro
            }
        }
        
        // Otimiza ambos os branches
        ifStmt.thenBlock = optimizeBlock(ifStmt.thenBlock);
        if (ifStmt.elseBlock !is null)
            ifStmt.elseBlock = optimizeBlock(ifStmt.elseBlock);
        
        return ifStmt;
    }
    
    HirNode optimizeWhile(HirWhile whileStmt)
    {
        whileStmt.condition = foldNode(whileStmt.condition);
        
        // Se condição é false constante, remove o loop
        if (auto boolLit = cast(HirBoolLit) whileStmt.condition)
        {
            if (!boolLit.value)
            {
                optimizationCount++;
                return null; // Loop nunca executa
            }
        }
        
        whileStmt.body = optimizeBlock(whileStmt.body);
        return whileStmt;
    }
    
    HirNode optimizeFor(HirFor forStmt)
    {
        // if (forStmt.init_ !is null)
        //     forStmt.init_ = optimizeStmt(forStmt.init_);
        
        // if (forStmt.condition !is null)
        //     forStmt.condition = foldNode(forStmt.condition);
        
        // if (forStmt.increment !is null)
        //     forStmt.increment = foldNode(forStmt.increment);
        
        forStmt.body = optimizeBlock(forStmt.body);
        return forStmt;
    }
    
    HirNode optimizeSwitch(HirSwitch switchStmt)
    {
        switchStmt.condition = foldNode(switchStmt.condition);
        
        // Se condição é constante, seleciona apenas o case correto
        if (isConstantValue(switchStmt.condition))
        {
            foreach (caseStmt; switchStmt.cases)
            {
                if (caseStmt.isDefault)
                    continue;
                
                foreach (value; caseStmt.values)
                {
                    auto foldedValue = foldNode(value);
                    if (constantsEqual(switchStmt.condition, foldedValue))
                    {
                        optimizationCount++;
                        auto block = new HirBlock();
                        block.stmts = caseStmt.body.stmts;
                        return optimizeBlock(block);
                    }
                }
            }
            
            // Se nenhum case bateu, usa default
            foreach (caseStmt; switchStmt.cases)
            {
                if (caseStmt.isDefault)
                {
                    optimizationCount++;
                    auto block = new HirBlock();
                    block.stmts = caseStmt.body.stmts;
                    return optimizeBlock(block);
                }
            }
            
            // Nenhum case bateu e não tem default
            return null;
        }
        
        // Otimiza cada case
        foreach (ref caseStmt; switchStmt.cases)
        {
            foreach (ref value; caseStmt.values)
                value = foldNode(value);
            caseStmt.body = optimizeBlock(caseStmt.body);
        }
        
        return switchStmt;
    }
    
    // Função principal de folding
    HirNode foldNode(HirNode node)
    {
        if (node is null) return null;
        
        switch (node.kind)
        {
            case HirNodeKind.Binary:
                return foldBinary(cast(HirBinary) node);
            
            case HirNodeKind.Unary:
                return foldUnary(cast(HirUnary) node);
            
            case HirNodeKind.Ternary:
                return foldTernary(cast(HirTernary) node);
            
            case HirNodeKind.Cast:
                return foldCast(cast(HirCast) node);
            
            case HirNodeKind.Load:
                return foldLoad(cast(HirLoad) node);
            
            case HirNodeKind.CallExpr:
                return foldCall(cast(HirCallExpr) node);
            
            case HirNodeKind.IndexAccess:
            case HirNodeKind.IndexExpr:
                return foldIndex(node);
            
            case HirNodeKind.ArrayLit:
                return foldArrayLit(cast(HirArrayLit) node);
            
            case HirNodeKind.AssignExpr:
                auto assign = cast(HirAssignExpr) node;
                assign.assign = cast(HirAssignDecl) optimizeAssign(assign.assign);
                return assign;
            
            // Valores literais já são constantes
            case HirNodeKind.IntLit:
            case HirNodeKind.LongLit:
            case HirNodeKind.FloatLit:
            case HirNodeKind.BoolLit:
            case HirNodeKind.CharLit:
            case HirNodeKind.StringLit:
            case HirNodeKind.NullLit:
                return node;
            
            default:
                return node;
        }
    }
    
    HirNode foldBinary(HirBinary binary)
    {
        binary.left = foldNode(binary.left);
        binary.right = foldNode(binary.right);
        
        // Se ambos os lados são constantes, calcula
        if (isConstantValue(binary.left) && isConstantValue(binary.right))
        {
            optimizationCount++;
            return evaluateBinary(binary);
        }
        
        // Otimizações algébricas mesmo sem constantes
        return algebraicOptimization(binary);
    }
    
    HirNode evaluateBinary(HirBinary binary)
    {
        auto left = binary.left;
        auto right = binary.right;
        string op = binary.op;
        
        // Operações inteiras (int, uint, byte, ubyte, short, ushort)
        if (auto leftInt = cast(HirIntLit) left)
        {
            if (auto rightInt = cast(HirIntLit) right)
            {
                // Determina se é operação unsigned
                bool isUnsigned = false;
                if (auto prim = cast(PrimitiveType) binary.type)
                {
                    isUnsigned = prim.baseType == BaseType.Uint || 
                                 prim.baseType == BaseType.Ubyte ||
                                 prim.baseType == BaseType.Ushort;
                }
                
                switch (op)
                {
                    case "+": return new HirIntLit(leftInt.value + rightInt.value, binary.type);
                    case "-": return new HirIntLit(leftInt.value - rightInt.value, binary.type);
                    case "*": return new HirIntLit(leftInt.value * rightInt.value, binary.type);
                    case "/": 
                        if (rightInt.value == 0) return binary;
                        if (isUnsigned)
                        {
                            uint ul = cast(uint)leftInt.value;
                            uint ur = cast(uint)rightInt.value;
                            return new HirIntLit(cast(int)(ul / ur), binary.type);
                        }
                        return new HirIntLit(leftInt.value / rightInt.value, binary.type);
                    case "%": 
                        if (rightInt.value == 0) return binary;
                        if (isUnsigned)
                        {
                            uint ul = cast(uint)leftInt.value;
                            uint ur = cast(uint)rightInt.value;
                            return new HirIntLit(cast(int)(ul % ur), binary.type);
                        }
                        return new HirIntLit(leftInt.value % rightInt.value, binary.type);
                    case "&": return new HirIntLit(leftInt.value & rightInt.value, binary.type);
                    case "|": return new HirIntLit(leftInt.value | rightInt.value, binary.type);
                    case "^": return new HirIntLit(leftInt.value ^ rightInt.value, binary.type);
                    case "<<": return new HirIntLit(leftInt.value << rightInt.value, binary.type);
                    case ">>": 
                        if (isUnsigned)
                            return new HirIntLit(cast(int)(cast(uint)leftInt.value >>> rightInt.value), binary.type);
                        return new HirIntLit(leftInt.value >> rightInt.value, binary.type);
                    case "==": return new HirBoolLit(leftInt.value == rightInt.value, binary.type);
                    case "!=": return new HirBoolLit(leftInt.value != rightInt.value, binary.type);
                    case "<": 
                        if (isUnsigned)
                            return new HirBoolLit(cast(uint)leftInt.value < cast(uint)rightInt.value, binary.type);
                        return new HirBoolLit(leftInt.value < rightInt.value, binary.type);
                    case "<=": 
                        if (isUnsigned)
                            return new HirBoolLit(cast(uint)leftInt.value <= cast(uint)rightInt.value, binary.type);
                        return new HirBoolLit(leftInt.value <= rightInt.value, binary.type);
                    case ">": 
                        if (isUnsigned)
                            return new HirBoolLit(cast(uint)leftInt.value > cast(uint)rightInt.value, binary.type);
                        return new HirBoolLit(leftInt.value > rightInt.value, binary.type);
                    case ">=": 
                        if (isUnsigned)
                            return new HirBoolLit(cast(uint)leftInt.value >= cast(uint)rightInt.value, binary.type);
                        return new HirBoolLit(leftInt.value >= rightInt.value, binary.type);
                    default: return binary;
                }
            }
        }
        
        // Operações long (long, ulong)
        if (auto leftLong = cast(HirLongLit) left)
        {
            if (auto rightLong = cast(HirLongLit) right)
            {
                // Determina se é operação unsigned
                bool isUnsigned = false;
                if (auto prim = cast(PrimitiveType) binary.type)
                    isUnsigned = prim.baseType == BaseType.Ulong;
                
                switch (op)
                {
                    case "+": return new HirLongLit(leftLong.value + rightLong.value, binary.type);
                    case "-": return new HirLongLit(leftLong.value - rightLong.value, binary.type);
                    case "*": return new HirLongLit(leftLong.value * rightLong.value, binary.type);
                    case "/": 
                        if (rightLong.value == 0) return binary;
                        if (isUnsigned)
                        {
                            ulong ul = cast(ulong)leftLong.value;
                            ulong ur = cast(ulong)rightLong.value;
                            return new HirLongLit(cast(long)(ul / ur), binary.type);
                        }
                        return new HirLongLit(leftLong.value / rightLong.value, binary.type);
                    case "%": 
                        if (rightLong.value == 0) return binary;
                        if (isUnsigned)
                        {
                            ulong ul = cast(ulong)leftLong.value;
                            ulong ur = cast(ulong)rightLong.value;
                            return new HirLongLit(cast(long)(ul % ur), binary.type);
                        }
                        return new HirLongLit(leftLong.value % rightLong.value, binary.type);
                    case "&": return new HirLongLit(leftLong.value & rightLong.value, binary.type);
                    case "|": return new HirLongLit(leftLong.value | rightLong.value, binary.type);
                    case "^": return new HirLongLit(leftLong.value ^ rightLong.value, binary.type);
                    case "<<": return new HirLongLit(leftLong.value << rightLong.value, binary.type);
                    case ">>": 
                        if (isUnsigned)
                            return new HirLongLit(cast(long)(cast(ulong)leftLong.value >>> rightLong.value),
                                binary.type);
                        return new HirLongLit(leftLong.value >> rightLong.value, binary.type);
                    case "==": return new HirBoolLit(leftLong.value == rightLong.value, binary.type);
                    case "!=": return new HirBoolLit(leftLong.value != rightLong.value, binary.type);
                    case "<": 
                        if (isUnsigned)
                            return new HirBoolLit(cast(ulong)leftLong.value < cast(ulong)rightLong.value, binary.type);
                        return new HirBoolLit(leftLong.value < rightLong.value, binary.type);
                    case "<=": 
                        if (isUnsigned)
                            return new HirBoolLit(cast(ulong)leftLong.value <= cast(ulong)rightLong.value, binary.type);
                        return new HirBoolLit(leftLong.value <= rightLong.value, binary.type);
                    case ">": 
                        if (isUnsigned)
                            return new HirBoolLit(cast(ulong)leftLong.value > cast(ulong)rightLong.value, binary.type);
                        return new HirBoolLit(leftLong.value > rightLong.value, binary.type);
                    case ">=": 
                        if (isUnsigned)
                            return new HirBoolLit(cast(ulong)leftLong.value >= cast(ulong)rightLong.value, binary.type);
                        return new HirBoolLit(leftLong.value >= rightLong.value, binary.type);
                    default: return binary;
                }
            }
        }
        
        // Operações float
        if (auto leftFloat = cast(HirFloatLit) left)
        {
            if (auto rightFloat = cast(HirFloatLit) right)
            {
                switch (op)
                {
                    case "+": return new HirFloatLit(leftFloat.value + rightFloat.value, binary.type);
                    case "-": return new HirFloatLit(leftFloat.value - rightFloat.value, binary.type);
                    case "*": return new HirFloatLit(leftFloat.value * rightFloat.value, binary.type);
                    case "/": 
                        if (rightFloat.value == 0.0) return binary;
                        return new HirFloatLit(leftFloat.value / rightFloat.value, binary.type);
                    case "==": return new HirBoolLit(leftFloat.value == rightFloat.value, binary.type);
                    case "!=": return new HirBoolLit(leftFloat.value != rightFloat.value, binary.type);
                    case "<": return new HirBoolLit(leftFloat.value < rightFloat.value, binary.type);
                    case "<=": return new HirBoolLit(leftFloat.value <= rightFloat.value, binary.type);
                    case ">": return new HirBoolLit(leftFloat.value > rightFloat.value, binary.type);
                    case ">=": return new HirBoolLit(leftFloat.value >= rightFloat.value, binary.type);
                    default: return binary;
                }
            }
        }
        
        // Operações booleanas
        if (auto leftBool = cast(HirBoolLit) left)
        {
            if (auto rightBool = cast(HirBoolLit) right)
            {
                switch (op)
                {
                    case "&&": return new HirBoolLit(leftBool.value && rightBool.value, binary.type);
                    case "||": return new HirBoolLit(leftBool.value || rightBool.value, binary.type);
                    case "==": return new HirBoolLit(leftBool.value == rightBool.value, binary.type);
                    case "!=": return new HirBoolLit(leftBool.value != rightBool.value, binary.type);
                    default: return binary;
                }
            }
        }
        
        // Concatenação de strings
        if (auto leftStr = cast(HirStringLit) left)
        {
            if (auto rightStr = cast(HirStringLit) right)
            {
                if (op == "+")
                    return new HirStringLit(leftStr.value ~ rightStr.value, binary.type);
            }
        }
        
        return binary;
    }
    
    HirNode algebraicOptimization(HirBinary binary)
    {
        string op = binary.op;
        auto left = binary.left;
        auto right = binary.right;
        
        // x + 0 = x
        if (op == "+")
        {
            if (isZero(right)) return left;
            if (isZero(left)) return right;
        }
        
        // x - 0 = x
        if (op == "-" && isZero(right))
            return left;
        
        // x * 0 = 0
        if (op == "*")
        {
            if (isZero(left)) return left;
            if (isZero(right)) return right;
        }
        
        // x * 1 = x
        if (op == "*")
        {
            if (isOne(right)) return left;
            if (isOne(left)) return right;
        }
        
        // x / 1 = x
        if (op == "/" && isOne(right))
            return left;
        
        // x & 0 = 0
        if (op == "&")
        {
            if (isZero(left)) return left;
            if (isZero(right)) return right;
        }
        
        // x | 0 = x
        if (op == "|")
        {
            if (isZero(right)) return left;
            if (isZero(left)) return right;
        }
        
        // x ^ 0 = x
        if (op == "^")
        {
            if (isZero(right)) return left;
            if (isZero(left)) return right;
        }
        
        // false && x = false
        if (op == "&&")
        {
            if (auto boolLit = cast(HirBoolLit) left)
                if (!boolLit.value) return left;
            if (auto boolLit = cast(HirBoolLit) right)
                if (!boolLit.value) return right;
        }
        
        // true || x = true
        if (op == "||")
        {
            if (auto boolLit = cast(HirBoolLit) left)
                if (boolLit.value) return left;
            if (auto boolLit = cast(HirBoolLit) right)
                if (boolLit.value) return right;
        }
        
        return binary;
    }
    
    HirNode foldUnary(HirUnary unary)
    {
        if (unary.operand.kind != HirNodeKind.Load)
            unary.operand = foldNode(unary.operand);

        if (HirLoad load = cast(HirLoad) unary.operand)
            mutableVars[load.varName] = true;
        
        if (!isConstantValue(unary.operand))
            return unary;
        
        optimizationCount++;
        
        switch (unary.op)
        {
            case "-":
                if (auto intLit = cast(HirIntLit) unary.operand)
                    return new HirIntLit(-intLit.value, unary.type);
                if (auto longLit = cast(HirLongLit) unary.operand)
                    return new HirLongLit(-longLit.value, unary.type);
                if (auto floatLit = cast(HirFloatLit) unary.operand)
                    return new HirFloatLit(-floatLit.value, unary.type);
                break;
            
            case "!":
                if (auto boolLit = cast(HirBoolLit) unary.operand)
                    return new HirBoolLit(!boolLit.value, unary.type);
                break;
            
            case "~":
                if (auto intLit = cast(HirIntLit) unary.operand)
                    return new HirIntLit(~intLit.value, unary.type);
                if (auto longLit = cast(HirLongLit) unary.operand)
                    return new HirLongLit(~longLit.value, unary.type);
                break;
            
            default:
                break;
        }
        
        return unary;
    }
    
    HirNode foldTernary(HirTernary ternary)
    {
        ternary.condition = foldNode(ternary.condition);
        ternary.trueExpr = foldNode(ternary.trueExpr);
        ternary.falseExpr = foldNode(ternary.falseExpr);
        
        // Se condição é constante, retorna apenas um lado
        if (auto boolLit = cast(HirBoolLit) ternary.condition)
        {
            optimizationCount++;
            return boolLit.value ? ternary.trueExpr : ternary.falseExpr;
        }
        
        return ternary;
    }
    
    HirNode foldCast(HirCast _cast)
    {
        _cast.value = foldNode(_cast.value);
        
        if (!isConstantValue(_cast.value))
            return _cast;
        
        optimizationCount++;
        
        auto targetPrim = cast(PrimitiveType) _cast.targetType;
        if (targetPrim is null)
            return _cast;
        
        // Cast de int para outros tipos
        if (auto intLit = cast(HirIntLit) _cast.value)
        {
            switch (targetPrim.baseType)
            {
                case BaseType.Byte:
                    return new HirIntLit(cast(byte)intLit.value, _cast.targetType);
                case BaseType.Ubyte:
                    return new HirIntLit(cast(ubyte)intLit.value, _cast.targetType);
                case BaseType.Short:
                    return new HirIntLit(cast(short)intLit.value, _cast.targetType);
                case BaseType.Ushort:
                    return new HirIntLit(cast(ushort)intLit.value, _cast.targetType);
                case BaseType.Int:
                    return new HirIntLit(intLit.value, _cast.targetType);
                case BaseType.Uint:
                    return new HirIntLit(cast(int)cast(uint)intLit.value, _cast.targetType);
                case BaseType.Long:
                    return new HirLongLit(cast(long)intLit.value, _cast.targetType);
                case BaseType.Ulong:
                    return new HirLongLit(cast(long)cast(ulong)intLit.value, _cast.targetType);
                case BaseType.Float:
                case BaseType.Double:
                    return new HirFloatLit(cast(double)intLit.value, _cast.targetType);
                case BaseType.Bool:
                    return new HirBoolLit(intLit.value != 0, _cast.targetType);
                case BaseType.Char:
                    return new HirCharLit(cast(char)intLit.value, _cast.targetType);
                default:
                    return _cast;
            }
        }
        
        // Cast de long para outros tipos
        if (auto longLit = cast(HirLongLit) _cast.value)
        {
            switch (targetPrim.baseType)
            {
                case BaseType.Byte:
                    return new HirIntLit(cast(byte)longLit.value, _cast.targetType);
                case BaseType.Ubyte:
                    return new HirIntLit(cast(ubyte)longLit.value, _cast.targetType);
                case BaseType.Short:
                    return new HirIntLit(cast(short)longLit.value, _cast.targetType);
                case BaseType.Ushort:
                    return new HirIntLit(cast(ushort)longLit.value, _cast.targetType);
                case BaseType.Int:
                    return new HirIntLit(cast(int)longLit.value, _cast.targetType);
                case BaseType.Uint:
                    return new HirIntLit(cast(int)cast(uint)longLit.value, _cast.targetType);
                case BaseType.Long:
                    return new HirLongLit(longLit.value, _cast.targetType);
                case BaseType.Ulong:
                    return new HirLongLit(cast(long)cast(ulong)longLit.value, _cast.targetType);
                case BaseType.Float:
                case BaseType.Double:
                    return new HirFloatLit(cast(double)longLit.value, _cast.targetType);
                case BaseType.Bool:
                    return new HirBoolLit(longLit.value != 0, _cast.targetType);
                case BaseType.Char:
                    return new HirCharLit(cast(char)longLit.value, _cast.targetType);
                default:
                    return _cast;
            }
        }
        
        // Cast de float para outros tipos
        if (auto floatLit = cast(HirFloatLit) _cast.value)
        {
            switch (targetPrim.baseType)
            {
                case BaseType.Byte:
                    return new HirIntLit(cast(byte)floatLit.value, _cast.targetType);
                case BaseType.Ubyte:
                    return new HirIntLit(cast(ubyte)floatLit.value, _cast.targetType);
                case BaseType.Short:
                    return new HirIntLit(cast(short)floatLit.value, _cast.targetType);
                case BaseType.Ushort:
                    return new HirIntLit(cast(ushort)floatLit.value, _cast.targetType);
                case BaseType.Int:
                    return new HirIntLit(cast(int)floatLit.value, _cast.targetType);
                case BaseType.Uint:
                    return new HirIntLit(cast(int)cast(uint)floatLit.value, _cast.targetType);
                case BaseType.Long:
                    return new HirLongLit(cast(long)floatLit.value, _cast.targetType);
                case BaseType.Ulong:
                    return new HirLongLit(cast(long)cast(ulong)floatLit.value, _cast.targetType);
                case BaseType.Float:
                    return new HirFloatLit(cast(float)floatLit.value, _cast.targetType);
                case BaseType.Double:
                    return new HirFloatLit(floatLit.value, _cast.targetType);
                case BaseType.Bool:
                    return new HirBoolLit(floatLit.value != 0.0, _cast.targetType);
                default:
                    return _cast;
            }
        }
        
        // Cast de bool para outros tipos
        if (auto boolLit = cast(HirBoolLit) _cast.value)
        {
            int val = boolLit.value ? 1 : 0;
            switch (targetPrim.baseType)
            {
                case BaseType.Byte:
                case BaseType.Ubyte:
                case BaseType.Short:
                case BaseType.Ushort:
                case BaseType.Int:
                case BaseType.Uint:
                    return new HirIntLit(val, _cast.targetType);
                case BaseType.Long:
                case BaseType.Ulong:
                    return new HirLongLit(val, _cast.targetType);
                case BaseType.Float:
                case BaseType.Double:
                    return new HirFloatLit(cast(double)val, _cast.targetType);
                case BaseType.Bool:
                    return boolLit;
                default:
                    return _cast;
            }
        }
        
        // Cast de char para outros tipos
        if (auto charLit = cast(HirCharLit) _cast.value)
        {
            switch (targetPrim.baseType)
            {
                case BaseType.Byte:
                case BaseType.Ubyte:
                case BaseType.Short:
                case BaseType.Ushort:
                case BaseType.Int:
                case BaseType.Uint:
                    return new HirIntLit(cast(int)charLit.value, _cast.targetType);
                case BaseType.Long:
                case BaseType.Ulong:
                    return new HirLongLit(cast(long)charLit.value, _cast.targetType);
                case BaseType.Char:
                    return charLit;
                default:
                    return _cast;
            }
        }
        
        return _cast;
    }
    
    HirNode foldLoad(HirLoad load)
    {
        // Se a variável está na tabela de constantes, substitui
        if (auto constValue = load.varName in constantSymbols)
        {
            // Só substitui se não foi modificada
            if (load.varName !in mutableVars || !mutableVars[load.varName])
            {
                optimizationCount++;
                return *constValue;
            }
        }
        
        return load;
    }
    
    HirNode foldCall(HirCallExpr call)
    {
        // Otimiza argumentos
        foreach (ref arg; call.args)
            arg = foldNode(arg);
        
        // Funções matemáticas intrínsecas podem ser dobradas
        // TODO: implementar funções como sqrt, sin, cos, etc
        
        return call;
    }
    
    HirNode foldIndex(HirNode node)
    {
        HirNode target, index;
        
        if (auto idx = cast(HirIndexAccess) node)
        {
            idx.target = foldNode(idx.target);
            idx.index = foldNode(idx.index);
            target = idx.target;
            index = idx.index;
        }
        else if (auto idx = cast(HirIndexExpr) node)
        {
            idx.target = foldNode(idx.target);
            idx.index = foldNode(idx.index);
            target = idx.target;
            index = idx.index;
        }
        else
        {
            return node;
        }
        
        // Se índice e array são constantes, pode calcular
        if (auto arrLit = cast(HirArrayLit) target)
        {
            if (auto intLit = cast(HirIntLit) index)
            {
                int idx = intLit.value;
                if (idx >= 0 && idx < arrLit.elements.length)
                {
                    optimizationCount++;
                    return arrLit.elements[idx];
                }
            }
        }
        
        return node;
    }
    
    HirNode foldArrayLit(HirArrayLit arr)
    {
        foreach (ref elem; arr.elements)
            elem = foldNode(elem);
        return arr;
    }
    
    // Utilitários
    bool isConstantValue(HirNode node)
    {
        if (node is null) return false;
        
        switch (node.kind)
        {
            case HirNodeKind.IntLit:
            case HirNodeKind.LongLit:
            case HirNodeKind.FloatLit:
            case HirNodeKind.BoolLit:
            case HirNodeKind.CharLit:
            case HirNodeKind.StringLit:
            case HirNodeKind.NullLit:
                return true;
            
            case HirNodeKind.ArrayLit:
                auto arr = cast(HirArrayLit) node;
                foreach (elem; arr.elements)
                    if (!isConstantValue(elem))
                        return false;
                return true;
            
            default:
                return false;
        }
    }
    
    bool isZero(HirNode node)
    {
        if (auto intLit = cast(HirIntLit) node)
            return intLit.value == 0;
        if (auto longLit = cast(HirLongLit) node)
            return longLit.value == 0;
        if (auto floatLit = cast(HirFloatLit) node)
            return floatLit.value == 0.0;
        return false;
    }
    
    bool isOne(HirNode node)
    {
        if (auto intLit = cast(HirIntLit) node)
            return intLit.value == 1;
        if (auto longLit = cast(HirLongLit) node)
            return longLit.value == 1;
        if (auto floatLit = cast(HirFloatLit) node)
            return floatLit.value == 1.0;
        return false;
    }
    
    bool constantsEqual(HirNode a, HirNode b)
    {
        if (a.kind != b.kind)
            return false;
        
        if (auto aInt = cast(HirIntLit) a)
            if (auto bInt = cast(HirIntLit) b)
                return aInt.value == bInt.value;
        
        if (auto aLong = cast(HirLongLit) a)
            if (auto bLong = cast(HirLongLit) b)
                return aLong.value == bLong.value;
        
        if (auto aFloat = cast(HirFloatLit) a)
            if (auto bFloat = cast(HirFloatLit) b)
                return aFloat.value == bFloat.value;
        
        if (auto aBool = cast(HirBoolLit) a)
            if (auto bBool = cast(HirBoolLit) b)
                return aBool.value == bBool.value;
        
        if (auto aChar = cast(HirCharLit) a)
            if (auto bChar = cast(HirCharLit) b)
                return aChar.value == bChar.value;
        
        if (auto aStr = cast(HirStringLit) a)
            if (auto bStr = cast(HirStringLit) b)
                return aStr.value == bStr.value;
        
        return false;
    }
}

// Exemplo de uso:
/*
auto program = astLowerer.lower(ast);
auto optimizer = new ConstantFolding();
program = optimizer.optimize(program);

// Antes:
// let x = 1 + 2
// let y = x * 3
// if (true) { ... }

// Depois:
// let x = 3
// let y = 9
// { ... }  // if removido
*/
