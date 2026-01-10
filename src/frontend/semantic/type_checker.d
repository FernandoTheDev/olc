module frontend.semantic.type_checker;

import frontend;
import common.reporter;

struct CastValidation
{
    bool isValid;
    bool needsCast;
    bool hasWarning;
    string warningMessage;
    bool isNarrowing;  // Conversão que pode perder dados
}

class TypeChecker
{
    Context ctx;
    DiagnosticError error;
    FunctionAnalyzer funcAnalyzer;
    TypeRegistry registry;
    FuncDecl[] funcs;
    StructDecl[] structs;
    StructDecl[] qeue;

    this(Context ctx, DiagnosticError error, TypeRegistry registry)
    {
        this.ctx = ctx;
        this.error = error;
        this.registry = registry;
    }

    Type checkExpression(Node expr)
    {
        if (expr is null)
            return VoidType.instance();

        if (expr.resolvedType !is null)
            return expr.resolvedType;

        Type type = checkExpressionInternal(expr);
        expr.resolvedType = type;
        return type;
    }


    pragma(inline, true)
    void reportError(string message, Loc loc, Suggestion[] suggestions = null)
    {
        error.addError(Diagnostic(message, loc, suggestions));
    }

    pragma(inline, true)
    void reportWarning(string message, Loc loc, Suggestion[] suggestions = null)
    {
        error.addWarning(Diagnostic(message, loc, suggestions));
    }

    bool checkType(Type left, Type right, Loc loc, bool strict = true)
    {
        if (left is null || right is null)
            return false;
        if (!left.isCompatibleWith(right, strict))
        {
            reportError(format("Incompatible types, expected '%s' received '%s'.", left.toStr(), right.toStr()), loc);
            return false;
        }
        return true;
    }

    bool checkTypeComp(Type left, Type right, Loc loc, bool strict = true)
    {
        if (!left.isCompatibleWith(right, strict))
        {
            reportError(format("Incompatible types, a type compatible with '%s' was expected but '%s' was received.", 
                left.toStr(), right.toStr()), loc);
            return false;
        }
        return true;
    }

    bool checkTypeBoth(Type left, Type right, Loc loc, bool strict = true)
    {
        if (!left.isCompatibleWith(right, strict) || !right.isCompatibleWith(left, strict))
        {
            reportError("Both values ​must be compatible with each other.", loc);
            return false;
        }
        return true;
    }

private:
    Type checkExpressionInternal(Node expr)
    {
        // Literais
        if (auto lit = cast(IntLit) expr)
            return new PrimitiveType(BaseType.Int);

        if (auto lit = cast(LongLit) expr)
            return new PrimitiveType(BaseType.Long);

        if (auto lit = cast(FloatLit) expr)
            return new PrimitiveType(BaseType.Float);

        if (auto lit = cast(DoubleLit) expr)
            return new PrimitiveType(BaseType.Double);

        if (auto lit = cast(StringLit) expr)
            return new PointerType(new PrimitiveType(BaseType.Char));

        if (auto lit = cast(BoolLit) expr)
            return new PrimitiveType(BaseType.Bool);

        if (auto lit = cast(NullLit) expr)
            return new PointerType(new PrimitiveType(BaseType.Void));

        if (auto lit = cast(CharLit) expr)
            return new PrimitiveType(BaseType.Char);

        if (auto structLit = cast(StructLit) expr)
            return checkStructLit(structLit);

        if (auto ident = cast(Identifier) expr)
            return checkIdentifier(ident);

        if (auto binary = cast(BinaryExpr) expr)
            return checkBinaryExpr(binary, expr);
        
        if (auto unary = cast(UnaryExpr) expr)
            return checkUnaryExpr(unary);
        
        if (auto assign = cast(AssignDecl) expr)
            return checkAssignDecl(assign);

        if (auto call = cast(CallExpr) expr)
            return checkCallExpr(call);

        if (auto index = cast(IndexExpr) expr)
            return checkIndexExpr(index);

        if (auto member = cast(MemberExpr) expr)
            return checkMemberExpr(member);

        if (auto arr = cast(ArrayLit) expr)
            return checkArrayLiteral(arr);

        if (auto ternary = cast(TernaryExpr) expr)
            return checkTernary(ternary);

        if (auto cst = cast(CastExpr) expr)
            return checkCastExpr(cst);

        if (auto sizeof = cast(SizeOfExpr) expr)
            return checkSizeof(sizeof);

        reportError("Unknown expression in type checking.", expr.loc);
        return new PrimitiveType(BaseType.Any);
    }

    CastValidation validateNumericCast(Node node, Type sourceType, Type targetType)
    {
        CastValidation result;
        result.isValid = true;
        result.needsCast = true;
        
        auto sourcePrim = cast(PrimitiveType) sourceType;
        auto targetPrim = cast(PrimitiveType) targetType;
        
        if (sourcePrim is null || targetPrim is null)
        {
            result.isValid = false;
            return result;
        }
        
        // Se os tipos são idênticos, não precisa cast
        if (sourcePrim.baseType == targetPrim.baseType)
        {
            result.needsCast = false;
            return result;
        }
        
        // Tenta extrair o valor literal para validação em tempo de compilação
        bool hasLiteralValue = false;
        long literalValue = 0;
        double floatValue = 0.0;
        bool isFloatLiteral = false;
        
        if (auto intLit = cast(IntLit) node)
        {
            hasLiteralValue = true;
            literalValue = intLit.value.get!int;
        }
        else if (auto longLit = cast(LongLit) node)
        {
            hasLiteralValue = true;
            literalValue = longLit.value.get!long;
        }
        else if (auto floatLit = cast(FloatLit) node)
        {
            hasLiteralValue = true;
            isFloatLiteral = true;
            floatValue = floatLit.value.get!float;
        }
        else if (auto doubleLit = cast(DoubleLit) node)
        {
            hasLiteralValue = true;
            isFloatLiteral = true;
            floatValue = doubleLit.value.get!double;
        }
        
        // Validação baseada no valor literal
        if (hasLiteralValue && !isFloatLiteral)
        {
            // Ranges dos tipos inteiros
            bool fitsInTarget = true;
            
            switch (targetPrim.baseType)
            {
                case BaseType.Byte:   // -128 a 127
                    fitsInTarget = literalValue >= -128 && literalValue <= 127;
                    break;
                case BaseType.Ubyte:  // 0 a 255
                    fitsInTarget = literalValue >= 0 && literalValue <= 255;
                    break;
                case BaseType.Short:  // -32768 a 32767
                    fitsInTarget = literalValue >= -32_768 && literalValue <= 32_767;
                    break;
                case BaseType.Ushort: // 0 a 65535
                    fitsInTarget = literalValue >= 0 && literalValue <= 65_535;
                    break;
                case BaseType.Int:    // -2^31 a 2^31-1
                    fitsInTarget = literalValue >= int.min && literalValue <= int.max;
                    break;
                case BaseType.Uint:   // 0 a 2^32-1
                    fitsInTarget = literalValue >= 0 && literalValue <= uint.max;
                    break;
                case BaseType.Long:   // -2^63 a 2^63-1
                    fitsInTarget = true; // long já é o maior tipo inteiro signed
                    break;
                case BaseType.Ulong:  // 0 a 2^64-1
                    fitsInTarget = literalValue >= 0;
                    break;
                case BaseType.Char:   // 0 a 255
                    fitsInTarget = literalValue >= 0 && literalValue <= 255;
                    break;
                case BaseType.Bool:   // 0 ou 1
                    fitsInTarget = literalValue == 0 || literalValue == 1;
                    if (!fitsInTarget)
                    {
                        result.hasWarning = true;
                        result.warningMessage = format(
                            "Value %d will be converted to bool (%s)",
                            literalValue, literalValue != 0 ? "true" : "false"
                        );
                        fitsInTarget = true; // Sempre é válido, mas com warning
                    }
                    break;
                case BaseType.Float:
                case BaseType.Double:
                    // Conversão de inteiro para float sempre válida (pode haver perda de precisão)
                    if (literalValue > 16_777_216) // Limite de precisão float
                    {
                        result.hasWarning = true;
                        result.warningMessage = "Large integer may lose precision when converted to float";
                    }
                    break;
                default:
                    break;
            }
            
            if (!fitsInTarget)
            {
                result.isValid = false;
                result.warningMessage = format(
                    "Value %d is out of range for type '%s'",
                    literalValue, targetPrim.toStr()
                );
                return result;
            }
        }
        
        // Validação baseada em tipos (quando não há literal)
        if (!hasLiteralValue)
        {
            // Detecta conversões que podem perder dados (narrowing)
            int sourceRank = TYPE_HIERARCHY.get(sourcePrim.baseType, 0);
            int targetRank = TYPE_HIERARCHY.get(targetPrim.baseType, 0);
            
            // Narrowing conversion (tipo maior -> tipo menor)
            if (sourceRank > targetRank)
            {
                result.isNarrowing = true;
                result.hasWarning = true;
                result.warningMessage = format(
                    "Implicit narrowing conversion from '%s' to '%s' may lose data",
                    sourcePrim.toStr(), targetPrim.toStr()
                );
            }
            
            // Conversões signed <-> unsigned podem ser problemáticas
            bool sourceUnsigned = sourcePrim.isUnsigned();
            bool targetUnsigned = targetPrim.isUnsigned();
            
            if (sourceUnsigned != targetUnsigned && sourceRank == targetRank)
            {
                result.hasWarning = true;
                result.warningMessage = format(
                    "Implicit conversion between signed and unsigned types ('%s' to '%s')",
                    sourcePrim.toStr(), targetPrim.toStr()
                );
            }
        }
        
        // Validação de conversões float <-> int
        if (isFloatLiteral)
        {
            if (targetPrim.isInteger())
            {
                result.hasWarning = true;
                result.warningMessage = format(
                    "Converting floating-point value %.2f to integer type '%s' (fractional part will be truncated)",
                    floatValue, targetPrim.toStr()
                );
                
                // Verifica se o valor cabe no tipo inteiro de destino
                long intPart = cast(long) floatValue;
                auto tempValidation = validateNumericCast(
                    new LongLit(intPart, node.loc), 
                    new PrimitiveType(BaseType.Long), 
                    targetType
                );
                
                if (!tempValidation.isValid)
                {
                    result.isValid = false;
                    result.warningMessage = format(
                        "Floating-point value %.2f is out of range for type '%s'",
                        floatValue, targetPrim.toStr()
                    );
                }
            }
        }
        
        return result;
    }

    public void makeImplicitCast(ref Node node, Type targetType)
    {
        node = implicitCast(node, targetType);
    }

    public Node implicitCast(Node node, Type targetType)
    {
        if (node is null || targetType is null)
            return node;

        Type sourceType = node.resolvedType;
        if (sourceType is null)
            return node;

        string sourceStr = sourceType.toStr();
        string targetStr = targetType.toStr();

        // Caso 1: Tipos idênticos - sem cast necessário
        if (sourceStr == targetStr)
            return node;

        // Caso 2: null para ponteiros - sempre válido, sem cast
        if (sourceStr == "void*" && targetType.isPointer())
            return node;

        // Caso 3: Ponteiro para void* - sempre válido com cast
        if (targetStr == "void*" && sourceType.isPointer())
        {
            auto castNode = new CastExpr(null, node, node.loc);
            castNode.resolvedType = targetType;
            return castNode;
        }

        // Caso 4: Ponteiros do mesmo tipo base (mas diferentes níveis de const, etc)
        if (targetType.isPointer() && sourceType.isPointer())
        {
            auto targetPtr = cast(PointerType) targetType;
            auto sourcePtr = cast(PointerType) sourceType;

            // Se apontam para o mesmo tipo, não precisa cast
            if (targetPtr.pointeeType.toStr() == sourcePtr.pointeeType.toStr())
                return node;

            // Se o target aceita o source (compatibilidade), faz cast
            if (targetType.isCompatibleWith(sourceType, false))
            {
                auto castNode = new CastExpr(null, node, node.loc);
                castNode.resolvedType = targetType;
                return castNode;
            }
        }

        // Caso 5: Conversões numéricas
        auto sourcePrim = cast(PrimitiveType) sourceType;
        auto targetPrim = cast(PrimitiveType) targetType;

        if (sourcePrim !is null && targetPrim !is null)
        {
            // Ambos são primitivos
            if (sourcePrim.isNumeric() && targetPrim.isNumeric())
            {
                CastValidation validation = validateNumericCast(node, sourceType, targetType);

                // Cast inválido - reporta erro e retorna sem cast
                if (!validation.isValid)
                {
                    reportError(validation.warningMessage, node.loc);
                    return node;
                }

                // Reporta warnings para narrowing conversions
                if (validation.hasWarning && validation.isNarrowing)
                    // Aqui você pode usar um sistema de warnings se tiver
                    // Por enquanto vamos silenciar narrowing implícitos
                    reportWarning(format("Warning: %s", validation.warningMessage), node.loc);

                // Se não precisa cast, retorna o node original
                if (!validation.needsCast)
                    return node;

                // Cria o cast implícito numérico
                auto castNode = new CastExpr(null, node, node.loc);
                castNode.resolvedType = targetType;
                return castNode;
            }

            // bool para outros tipos primitivos
            if (sourcePrim.baseType == BaseType.Bool && targetPrim.isNumeric())
            {
                auto castNode = new CastExpr(null, node, node.loc);
                castNode.resolvedType = targetType;
                return castNode;
            }

            // Outros tipos primitivos para bool
            if (targetPrim.baseType == BaseType.Bool && sourcePrim.isNumeric())
            {
                auto castNode = new CastExpr(null, node, node.loc);
                castNode.resolvedType = targetType;
                return castNode;
            }
        }

        // Caso 6: Inteiro para ponteiro (conversão não estrita)
        if (targetType.isPointer() && sourcePrim !is null && sourcePrim.isInteger())
        {
            if (targetType.isCompatibleWith(sourceType, false))
            {
                auto castNode = new CastExpr(null, node, node.loc);
                castNode.resolvedType = targetType;
                return castNode;
            }
        }

        // Caso 7: Ponteiro para inteiro (conversão não estrita)
        if (sourceType.isPointer() && targetPrim !is null && targetPrim.isInteger())
        {
            if (targetType.isCompatibleWith(sourceType, false))
            {
                auto castNode = new CastExpr(null, node, node.loc);
                castNode.resolvedType = targetType;
                return castNode;
            }
        }

        // Caso 8: Array decay para ponteiro
        if (auto sourceArray = cast(ArrayType) sourceType)
        {
            if (auto targetPtr = cast(PointerType) targetType)
            {
                if (targetPtr.pointeeType.toStr() == sourceArray.elementType.toStr())
                {
                    auto castNode = new CastExpr(null, node, node.loc);
                    castNode.resolvedType = targetType;
                    return castNode;
                }
            }
        }

        // Caso 9: Enum para int
        if (sourceType.isEnum() && targetPrim !is null && targetPrim.baseType == BaseType.Int)
        {
            auto castNode = new CastExpr(null, node, node.loc);
            castNode.resolvedType = targetType;
            return castNode;
        }

        // Caso 10: Compatibilidade genérica (função, struct, union, etc)
        if (targetType.isCompatibleWith(sourceType, false))
        {
            // Se já são compatíveis mas não idênticos, pode precisar de cast
            // Exemplo: diferentes tipos de função compatíveis
            if (cast(FunctionType) targetType || cast(StructType) targetType || cast(UnionType) targetType)
            {
                // Esses geralmente não precisam de cast explícito se já são compatíveis
                return node;
            }

            // Outros casos que precisam de cast
            auto castNode = new CastExpr(null, node, node.loc);
            castNode.resolvedType = targetType;
            return castNode;
        }

        return node;
    }

    bool isNumericType(Type t)
    {
        if (auto prim = cast(PrimitiveType) t)
            return prim.isNumeric();
        return false;
    }

    bool tryGetConstantValue(Node node, Context ctx, out long intValue, out double floatValue, out bool isFloat)
    {
        if (auto intLit = cast(IntLit) node)
        {
            intValue = intLit.value.get!int;
            isFloat = false;
            return true;
        }

        if (auto longLit = cast(LongLit) node)
        {
            intValue = longLit.value.get!long;
            isFloat = false;
            return true;
        }

        if (auto floatLit = cast(FloatLit) node)
        {
            floatValue = floatLit.value.get!float;
            isFloat = true;
            return true;
        }

        if (auto doubleLit = cast(DoubleLit) node)
        {
            floatValue = doubleLit.value.get!double;
            isFloat = true;
            return true;
        }

        if (auto ident = cast(Identifier) node)
        {
            Symbol sym = ctx.lookup(ident.value.get!string);
            if (auto varSym = cast(VarSymbol) sym)
                if (varSym.isConst && varSym.value !is null)
                    return tryGetConstantValue(varSym.value, ctx, intValue, floatValue, isFloat);
        }

        return false;
    }

    // public void makeImplicitCast(ref Node node, Type targetType)
    // {
    //     node = implicitCast(node, targetType);
    // }

    // public Node implicitCast(Node node, Type targetType)
    // {
    //     Type sourceType = node.resolvedType;

    //     if (targetType == sourceType)
    //         return node;

    //     if (targetType is null)
    //         return node;

    //     if (!targetType.isCompatibleWith(sourceType, false))
    //         return node;
        
    //     if (isNumeric(sourceType) && isNumeric(targetType))
    //     {
    //         auto castNode = new CastExpr(null, node, node.loc);
    //         castNode.resolvedType = targetType; 
    //         return castNode;
    //     }

    //     if (targetType.toStr() == "void*" && sourceType.isPointer()) 
    //     {
    //          auto castNode = new CastExpr(null, node, node.loc);
    //          castNode.resolvedType = targetType;
    //          return castNode;
    //     }

    //     if (sourceType.toStr() == "null" && targetType.isPointer())
    //          return node;

    //     return node;
    // }

    Type checkSizeof(SizeOfExpr sizeof)
    {
        Type type = new TypeResolver(ctx, error, registry).resolve(sizeof.type);
        if (sizeof.value !is null)
            sizeof.value.resolvedType = checkExpression(sizeof.value);
        sizeof.resolvedType_ = new TypeResolver(ctx, error, registry).resolve(sizeof.type_);
        sizeof.resolvedType = type;
        return type;
    }

    int getRank(Type t) 
    {
        if (PrimitiveType p = cast(PrimitiveType) t)
            return TYPE_HIERARCHY.get(p.baseType, 0);
        return 0;
    }

    bool isNumeric(Type t)
    {
        return getRank(t) > 0;
    }

    Type checkStructLit(StructLit lit)
    {
        StructSymbol structSym = ctx.lookupStruct(lit.structName);
        if (structSym is null)
        {
            reportError(format("Struct '%s' not found", lit.structName), lit.loc);
            lit.resolvedType = new PrimitiveType(BaseType.Any);
            return new PrimitiveType(BaseType.Any);
        }
        
        StructType structType = structSym.structType;
        lit.mangledName = structSym.declaration.mangledName;
        structType.mangledName = lit.mangledName;
        lit.resolvedType = structType;
        
        // 1. Se for chamada de construtor: User("John")
        if (lit.isConstructorCall)
        {
            auto ctor = structType.getConstructor();
            if (ctor is null)
            {
                reportError(
                    format("Struct '%s' does not have a constructor", lit.structName),
                    lit.loc
                );
                return lit.resolvedType;
            }
            
            // Valida número de argumentos
            if (lit.fieldInits.length != ctor.funcDecl.args.length)
            {
                reportError(
                    format("Constructor expects %d arguments, got %d",
                           ctor.funcDecl.args.length, lit.fieldInits.length),
                    lit.loc
                );
                return lit.resolvedType;
            }
            
            // Valida tipos dos argumentos
            foreach (i, init; lit.fieldInits)
            {
                Type argType = checkExpression(init.value);
                Type expectedType = ctor.funcDecl.args[i].resolvedType;
                
                if (!expectedType.isCompatibleWith(argType))
                {
                    reportError(
                        format("Argument %d: expected '%s', got '%s'",
                               i + 1, expectedType.toStr(), argType.toStr()),
                        init.value.loc
                    );
                    return lit.resolvedType;
                }
            }
        }
        // 2. Se for inicialização posicional: Test{"John", 17}
        else if (lit.isPositional)
        {
            if (lit.fieldInits.length > structType.fieldCount())
            {
                reportError(
                    format("Too many initializers: struct '%s' has %d fields, got %d",
                           lit.structName, structType.fieldCount(), lit.fieldInits.length),
                    lit.loc
                );
                return lit.resolvedType;
            }
            
            // Valida cada campo na ordem
            foreach (i, ref init; lit.fieldInits)
            {
                if (i >= structType.fields.length)
                    break;
                
                StructField field = structType.fields[i];
                Type valueType = checkExpression(init.value);
                makeImplicitCast(init.value, field.resolvedType);
                valueType = init.value.resolvedType;

                if (!field.resolvedType.isCompatibleWith(valueType))
                {
                    reportError(
                        format("Field '%s': expected '%s', got '%s'",
                               field.name, field.resolvedType.toStr(), valueType.toStr()),
                        init.value.loc
                    );
                    return lit.resolvedType;
                }
            }
        }
        // 3. Se for inicialização nomeada: Test{.name="John", .age=17}
        else
        {
            bool[string] initializedFields;
            
            foreach (init; lit.fieldInits)
            {
                // Verifica se o campo existe
                if (!structType.hasField(init.name))
                {
                    reportError(
                        format("Struct '%s' does not have field '%s'",
                               lit.structName, init.name),
                        init.loc
                    );
                    continue;
                }
                
                // Verifica duplicatas
                if (init.name in initializedFields)
                {
                    reportError(
                        format("Field '%s' initialized multiple times", init.name),
                        init.loc
                    );
                    continue;
                }
                initializedFields[init.name] = true;
                
                // Valida tipo
                Type fieldType = structType.getFieldType(init.name);
                Type valueType = checkExpression(init.value);
                
                if (!fieldType.isCompatibleWith(valueType))
                    reportError(
                        format("Field '%s': expected '%s', got '%s'",
                               init.name, fieldType.toStr(), valueType.toStr()),
                        init.value.loc
                    );
            }
            
            // Verifica se campos sem valor padrão foram inicializados
            foreach (field; structType.fields)
            {
                if (field.name !in initializedFields && field.defaultValue is null)
                    reportError(
                        format("Field '%s' must be initialized (no default value)", field.name),
                        lit.loc,
                        [Suggestion(format("Add: .%s = <value>", field.name))]
                    );
            }
        }

        return lit.resolvedType;
    }

    Type checkMemberExpr(MemberExpr expr)
    {
        void structError(StructType structType, string member, Loc loc)
        {
            reportError(
                    format("Struct '%s' does not have field '%s'",
                           structType.name, member),
                    loc,
                    [Suggestion(format("Available fields: %s", 
                        getAvailableFields(structType)))]
                );
        }

        void unionError(UnionType un, string member, Loc loc)
        {
            reportError(
                    format("Union '%s' does not have field '%s'",
                           un.name, member),
                    loc,
                    [Suggestion(format("Available fields: %s", 
                        getAvailableFields(un)))]
                );
        }

        void enumError(EnumType enm, string member, Loc loc)
        {
            reportError(
                    format("Enum '%s' does not have field '%s'",
                           enm.name, member),
                    loc,
                    [Suggestion(format("Available fields: %s", 
                        getAvailableFields(enm)))]
                );
        }

        Type targetType = checkExpression(expr.target);

        if (StructType structType = cast(StructType) targetType)
        {
            if (!structType.hasField(expr.member))
            {
                structError(structType, expr.member, expr.loc);
                return new PrimitiveType(BaseType.Any);
            }

            // Retorna o tipo do campo
            Type fieldType = structType.getFieldType(expr.member);
            expr.resolvedType = fieldType;
            return fieldType;
        }

        // 2. Acesso a campo através de ponteiro para struct
        if (PointerType ptrType = cast(PointerType) targetType)
        {
            if (StructType structType = cast(StructType) ptrType.pointeeType)
            {
                // ptr->field é equivalente a (*ptr).field
                if (!structType.hasField(expr.member))
                {
                    structError(structType, expr.member, expr.loc);
                    return new PrimitiveType(BaseType.Any);
                }
                Type fieldType = structType.getFieldType(expr.member);
                expr.resolvedType = fieldType;
                return fieldType;
            }
        }

        if (UnionType un = cast(UnionType) targetType)
        {
            // Verifica se o campo existe
            if (!un.hasField(expr.member))
            {
                unionError(un, expr.member, expr.loc);
                return new PrimitiveType(BaseType.Any);
            }

            // Retorna o tipo do campo
            Type fieldType = un.getFieldType(expr.member);
            expr.resolvedType = fieldType;
            return fieldType;
        }

        if (EnumType enm = cast(EnumType) targetType)
        {
            // Verifica se o campo existe
            if (!enm.hasMember(expr.member))
            {
                enumError(enm, expr.member, expr.loc);
                return new PrimitiveType(BaseType.Any);
            }
            return enm.baseType;
        }

        reportError(
            format("Type '%s' does not have members", targetType.toStr()),
            expr.target.loc,
            [Suggestion("Only structs and pointers to structs support member access")]
        );
        return new PrimitiveType(BaseType.Any);
    }

    string getAvailableFields(StructType structType)
    {
        if (structType.fields.length == 0)
            return "(none)";

        return structType.fields.map!(f => f.name).join(", ");
    }

    string getAvailableFields(UnionType un)
    {
        if (un.fields.length == 0)
            return "(none)";

        return un.fields.map!(f => f.name).join(", ");
    }    

    string getAvailableFields(EnumType enm)
    {
        if (enm.members.length == 0)
            return "(none)";

        return enm.members.byKey.map!(f => f).join(", ");
    }

    Type checkCastExpr(CastExpr expr)
    {
        expr.resolvedType = new TypeResolver(ctx, error, registry).resolve(expr.target);
        expr.from.resolvedType = checkExpression(expr.from);
        checkTypeComp(expr.resolvedType, expr.from.resolvedType, expr.loc, false);
        return expr.resolvedType;
    }

    Type checkTernary(TernaryExpr ternary)
    {
        Type condition = checkExpression(ternary.condition);
        Type left = ternary.trueExpr is null ? null : checkExpression(ternary.trueExpr);
        Type right = checkExpression(ternary.falseExpr);

        if (!checkType(condition, new PrimitiveType(BaseType.Bool), ternary.loc))
            return new PrimitiveType(BaseType.Any);

        if (left is null)
            return right;

        if (!checkTypeBoth(left, right, ternary.loc))
            return new PrimitiveType(BaseType.Any);

        if (right.toStr() == left.toStr())
            return left;

        return left;
    }

    Type checkIdentifier(Identifier ident)
    {
        string id = ident.value.get!string;
        Symbol sym = ctx.lookup(id);

        if (sym is null)
        {
            reportError(format("'%s' was not declared.", id), ident.loc);
            return new PrimitiveType(BaseType.Any);
        }

        if (FunctionSymbol fn = cast(FunctionSymbol) sym)
        {
            FunctionType type = new FunctionType(fn.paramTypes, fn.returnType);
            type.mangled = fn.declaration.mangledName;
            ident.resolvedType = type;
            ident.mangledName = type.mangled;
            ident.isFunctionReference = true;
            return type;
        }

        if (sym.type is null)
        {
            reportError(format("'%s' has no defined type.", id), ident.loc);
            return new PrimitiveType(BaseType.Any);
        }

        // writeln("Ref: ", sym.type.refConst);
        // writeln("Const: ", (cast(VarSymbol)sym).isConst, "\n");

        ident.resolvedType = sym.type;
        return sym.type;
    }

    bool isInteger(Type t)
    {
        if (PrimitiveType primi = cast(PrimitiveType) t)
            return primi.baseType == BaseType.Int || primi.baseType == BaseType.Long 
                || primi.baseType == BaseType.Uint || primi.baseType == BaseType.Ulong
                || primi.baseType == BaseType.Ushort || primi.baseType == BaseType.Short
                || primi.baseType == BaseType.Ubyte || primi.baseType == BaseType.Byte;
        return false;
    }

    Type checkBinaryExpr(BinaryExpr expr, Node n)
    {
        Type leftType = checkExpression(expr.left);
        expr.left.resolvedType = leftType;
        Type rightType = checkExpression(expr.right);
        expr.right.resolvedType = rightType;
        PointerType ptr;
        
        string op = expr.op;

        // Caso 1: Ponteiro + Inteiro (ex: walker + 1)
        if (op == "+" && leftType.isPointer() && isInteger(rightType))
        {
            ptr = cast(PointerType) leftType;
            if (ptr.isCompatibleWith(rightType, false))
            {
                expr.resolvedType = leftType; // O resultado continua sendo User*
                return expr.resolvedType;
            }
        }

        // Caso 2: Ponteiro - Inteiro (ex: walker - 2)
        if (op == "-" && leftType.isPointer() && isInteger(rightType))
        {
            ptr = cast(PointerType) leftType;
            if (ptr.isCompatibleWith(rightType, false))
            {
                expr.resolvedType = leftType; // O resultado continua sendo User*
                return expr.resolvedType;
            }
        }

        string getOpName(string op, bool isRight)
        {
            if (isRight)
                switch (op)
                    {
                    case "+":  return "opAddRight";
                    case "-":  return "opSubRight";
                    case "*":  return "opMulRight";
                    case "/":  return "opDivRight";
                    case "%":  return "opModRight";
                    case "==": return "opEqualsRight";
                    case "!=": return "opNotEqualsRight";
                    case "<":  return "opLessRight";
                    case ">":  return "opGreaterRight";
                    case "<=": return "opLessEqualRight";
                    case ">=": return "opGreaterEqualRight";
                    default: return null; 
                }
            else
                switch (op)
                {
                    case "+":  return "opAdd";
                    case "-":  return "opSub";
                    case "*":  return "opMul";
                    case "/":  return "opDiv";
                    case "%":  return "opMod";
                    case "==": return "opEquals";
                    case "!=": return "opNotEquals";
                    case "<":  return "opLess";
                    case ">":  return "opGreater";
                    case "<=": return "opLessEqual";
                    case ">=": return "opGreaterEqual";
                    default: return null;
                }
        }

        Type tryResolveOperator(StructType st, string op, Type inputType, BinaryExpr expr, bool isRight)
        {
            string methodName = getOpName(op, isRight);
            StructMethod* method = null;

            if (methodName !is null && st.hasMethod(methodName))
            {
                method = st.findMethod(methodName, [inputType]);
                if (method !is null)
                {
                    expr.mangledName = method.funcDecl.mangledName;
                    expr.resolvedType = method.funcDecl.resolvedType;
                    if (isRight) expr.isRight = true; // Seta flag se necessário
                    return expr.resolvedType;
                }
            }
        
            if (st.hasMethod("opBinary"))
            {
                method = st.findMethod("opBinary", [new PointerType(new PrimitiveType(BaseType.Char)), inputType]);
                if (method !is null)
                {
                    expr.mangledName = method.funcDecl.mangledName;
                    expr.resolvedType = method.funcDecl.resolvedType;
                    expr.usesOpBinary = true;
                    if (isRight) expr.isRight = true;
                    return expr.resolvedType;
                }
            }

            return null;
        }

        if (auto st = cast(StructType) leftType)
        {
            Type result = tryResolveOperator(st, op, rightType, expr, false); // isRight = false
            if (result !is null) return result;

            reportError(format("Struct '%s' does not implement operator '%s' for type '%s'", 
                st.name, op, rightType.toStr()), expr.loc);
            return new PrimitiveType(BaseType.Any);
        }

        if (auto st = cast(StructType) rightType)
        {
            Type result = tryResolveOperator(st, op, leftType, expr, true); // isRight = true
            if (result !is null) return result;

            reportError(format("Struct '%s' does not implement operator '%s' for type '%s'", 
                st.name, op, leftType.toStr()), expr.loc);
            return new PrimitiveType(BaseType.Any);
        }

        // Operadores aritméticos: +, -, *, /, %
        if (op == "+" || op == "-" || op == "*" || op == "/" || op == "%")
        {
            if (!leftType.isNumeric() || !rightType.isNumeric())
            {
                reportError(format("The '%s' operator requires numeric operands.", op), expr.loc);
                return new PrimitiveType(BaseType.Any);
            }

            // Type promotion
            Type t = leftType.getPromotedType(rightType);
            expr.resolvedType = t;
            makeImplicitCast(expr.right, expr.resolvedType);
            makeImplicitCast(expr.left, expr.resolvedType);
            return t;
        }

        // Operadores de comparação: ==, !=, <, >, <=, >=
        if (op == "==" || op == "!=" || op == "<" || op == ">" || op == "<=" || op == ">=")
        {
            Type promo = leftType.getPromotedType(rightType);
            makeImplicitCast(expr.right, promo);
            makeImplicitCast(expr.left, promo);
            Type t = new PrimitiveType(BaseType.Bool);
            expr.resolvedType = t;
            return t;
        }

        // Operadores lógicos: &&, ||
        if (op == "&&" || op == "||")
        {
            auto boolType = new PrimitiveType(BaseType.Bool);
            checkTypeBoth(leftType, boolType, expr.loc);
            expr.resolvedType = boolType;
            return boolType;
        }

        // Operadores bitwise
        if (op == "&" || op == "|" || op == "^")
        {
            if (!leftType.isNumeric() || !rightType.isNumeric())
                reportError(format("The bitwise operator '%s' requires integer operands.", op), expr.loc);

            Type promo = leftType.getPromotedType(rightType);
            expr.resolvedType = promo;
            makeImplicitCast(expr.right, promo);
            makeImplicitCast(expr.left, promo);
            return promo;
        }

        // Shift operators (tipo do resultado = tipo da esquerda)
        if (op == "<<" || op == ">>" || op == ">>>")
        {
            if (!leftType.isNumeric() || !rightType.isNumeric())
                reportError(format("The shift operator '%s' requires integer operands.", op), expr.loc);

            expr.resolvedType = leftType;
            return leftType;
        }

        reportError(format("Unknown binary operator: '%s'", op), expr.loc);
        return new PrimitiveType(BaseType.Any);
    }

    Identifier getIdentifier(Node target)
    {
        if (Identifier id = cast(Identifier) target)
            return id;
        if (UnaryExpr expr = cast(UnaryExpr) target)
            if (expr.op == "*")
                return getIdentifier(expr.operand);
        return null;
    }

    Type checkUnaryExpr(UnaryExpr expr)
    {
        Type operandType = checkExpression(expr.operand);
        string op = expr.op;
        expr.resolvedType = operandType;

        // Negação: -x
        if (op == "-")
        {
            if (!operandType.isNumeric())
                reportError("The operator '-' requires a numeric operand.", expr.loc);
            return operandType;
        }

        // NOT lógico: !x
        if (op == "!")
        {
            auto boolType = new PrimitiveType(BaseType.Bool);
            if (!operandType.isCompatibleWith(boolType))
                reportError("The '!' operator requires a logical operand.", expr.loc);
            expr.resolvedType = boolType;
            return boolType;
        }

        // NOT bitwise: ~x
        if (op == "~")
        {
            if (!operandType.isNumeric())
                reportError("The '~' operator requires an integer operand.", expr.loc);
            return operandType;
        }

        // ++, --
        if (op == "++" || op == "--" ||
            op == "++_prefix" || op == "--_prefix" ||
            op == "++_postfix" || op == "--_postfix")
        {
            if (!operandType.isNumeric())
                reportError(format("The '%s' operator requires a numeric operand.",
                        op[0 .. 2]), expr.loc);
            return operandType;
        }

        bool refConst = false;
        Identifier id = getIdentifier(expr.operand);
        if (id !is null)
        {
            VarSymbol sym = ctx.lookupVariable(id.value.get!string);
            if (sym.isConst)
                refConst = sym.isConst;
            else
                refConst = sym.type.refConst;
        }

        if ((op == "&" || op == "*") && (expr.operand.kind != NodeKind.Identifier 
            && expr.operand.kind != NodeKind.MemberExpr && expr.operand.kind != NodeKind.UnaryExpr))
        {
            // invalid pointer
            reportError(format("The '%s' operator requires the operand to be either an identifier or an expression" 
                ~ " member.", op), expr.loc);
            if (op == "&")
                return new PointerType(new PrimitiveType(BaseType.Any));
            return new PrimitiveType(BaseType.Any);
        }

        if (op == "&")
        {
            expr.resolvedType = new PointerType(operandType, refConst);
            expr.refConst = refConst;
            expr.resolvedType.refConst = refConst;
            return expr.resolvedType;
        }

        if (op == "*")
        {
            if (!operandType.isPointer())
            {
                reportError("The '*' operator requires a pointer.", expr.loc);
                return operandType;
            }
            PointerType opType = (cast(PointerType) operandType);
            expr.resolvedType = opType.pointeeType;
            expr.resolvedType.refConst = refConst;
            expr.refConst = refConst;
            return opType.pointeeType;
        }

        return operandType;
    }

    Type checkAssignDecl(AssignDecl expr)
    {
        Type targetType = checkExpression(expr.left);
        Type valueType = checkExpression(expr.right);
            
        // Verifica se pode atribuir
        if (auto ident = cast(Identifier) expr.left)
        {
            string id = ident.value.get!string;
            if (!ctx.canAssign(id))
                reportError(format("'%s' is a constant and cannot be modified.", id), expr.loc);
        }

        // Atribuição simples: =
        if (expr.op == "=")
        {
            if (!valueType.isCompatibleWith(targetType))
                reportError(format("Incompatible type: cannot assign '%s' to '%s'.",
                        valueType.toStr(), targetType.toStr()), expr.loc);
            return targetType;
        }

        // Atribuições compostas: +=, -=, *=, etc
        // Checa como operação binária
        string binOp = expr.op[0 .. $ - 1]; // remove '='
        auto binaryType = checkBinaryExpr(
            new BinaryExpr(expr.left, expr.right, binOp, expr.loc), null
        );

        if (!binaryType.isCompatibleWith(targetType))
            reportError(format("Incompatible type in compound assignment '%s'.", expr.op), expr.loc);
    
        return targetType;
    }


    Type checkCallExpr(CallExpr expr)
    {
        if (MemberExpr mem = cast(MemberExpr) expr.id)
        {
            Type targetType = checkExpression(mem.target);

            StructType structType;
            if (auto pt = cast(PointerType) targetType)
                structType = cast(StructType) pt.pointeeType;
            else
                structType = cast(StructType) targetType;

            if (structType is null)
            {
                reportError("Methods can only be called on structs or pointers to structs.", mem.loc);
                return new PrimitiveType(BaseType.Any);
            }

            Type[] argTypes;
            foreach (arg; expr.args)
                argTypes ~= checkExpression(arg);

            StructMethod* method = structType.findMethod(mem.member, argTypes);
            if (method is null)
            {
                reportError(
                    format("Method '%s' not found in struct '%s' with signature (%s).",
                        mem.member,
                        structType.name,
                        argTypes.map!(t => t.toStr()).join(", ")),
                    mem.loc,
                    [Suggestion(format("Available methods: %s", getAvailableMethods(structType)))]
                );
                return new PrimitiveType(BaseType.Any);
            }

            FuncDecl funcDecl = method.funcDecl;

            bool hasVariadic = false;
            size_t minArgs = funcDecl.args.length;

            if (funcDecl.args.length > 0)
            {
                auto lastArg = funcDecl.args[$-1];
                if (lastArg.name == "..." || lastArg.resolvedType is null)
                {
                    hasVariadic = true;
                    minArgs = cast(int) funcDecl.args.length - 1;
                }
            }

            size_t expectedArgs = hasVariadic ? minArgs : funcDecl.args.length;
            size_t providedArgs = expr.args.length + 1; // +1 do 'this' implícito

            if (hasVariadic)
            {
                if (providedArgs < expectedArgs)
                {
                    reportError(
                        format("Method '%s' expects at least %d arguments (including self), but received %d.",
                            mem.member, expectedArgs, providedArgs),
                        expr.loc
                    );
                    return funcDecl.resolvedType;
                }
            }
            else
                if (providedArgs != expectedArgs)
                {
                    reportError(
                        format("Method '%s' expects exactly %d arguments (including self), but received %d.",
                            mem.member, expectedArgs, providedArgs),
                        expr.loc
                    );
                    return funcDecl.resolvedType;
                }

            Type thisParamType = funcDecl.args[0].resolvedType;
            Type passedType = targetType.isStruct() ? new PointerType(targetType) : targetType;

            if (!thisParamType.isCompatibleWith(passedType))
                reportError(
                    format("Type mismatch for 'self': method expects '%s', but object is '%s'",
                        thisParamType.toStr(), targetType.toStr()),
                    mem.loc
                );

            size_t argsToCheck = hasVariadic ? minArgs - 1 : argTypes.length;
            foreach (i; 0 .. argsToCheck)
            {
                Type paramType = funcDecl.args[i + 1].resolvedType;
                Type argType = argTypes[i];

                bool result = paramType.isPointer() 
                    ? (cast(PointerType)paramType).isCompatibleWith(argType, true)
                    : paramType.isCompatibleWith(argType);

                if (!result)
                    reportError(
                        format("Argument #%d type mismatch: expected '%s', but received '%s'",
                            i + 1, paramType.toStr(), argType.toStr()),
                        expr.args[i].loc
                    );
            }

            expr.resolvedType = funcDecl.resolvedType;
            expr.mangledName = funcDecl.mangledName;
            expr.isVarArg = hasVariadic;
            return funcDecl.resolvedType;
        }

        Type[] argTypes;
        foreach (arg; expr.args)
            argTypes ~= checkExpression(arg);

        string funcName = "";
        Node ident = null;

        if (auto id_ = cast(Identifier) expr.id)
        {
            funcName = id_.value.get!string;
            ident = id_;
        }
        else if (auto str = cast(StringLit) expr.id)
        {
            funcName = str.value.get!string;
            ident = str;
        }
        else
        {
            reportError("Function call must use an identifier or string literal.", expr.loc);
            return new PrimitiveType(BaseType.Any);
        }

        FunctionSymbol funcSym = ctx.findFunction(funcName, argTypes, null);
        if (funcSym is null)
        {
            Symbol sym = ctx.lookup(funcName);
            if (sym !is null)
            {
                if (auto fnType = cast(FunctionType) sym.type)
                {
                    expr.mangledName = fnType.mangled != "" ? fnType.mangled : funcName;
                    expr.isRef = true;
                    expr.refType = fnType;
                    expr.resolvedType = fnType.returnType;

                    if (fnType.paramTypes.length != argTypes.length)
                        reportError(
                            format("Function reference expects %d arguments, but received %d.",
                                fnType.paramTypes.length, argTypes.length),
                            expr.loc
                        );

                    return fnType.returnType;
                }
            }

            auto candidates = ctx.currentScope.resolveFunctions(funcName);
            if (candidates.length > 0)
            {
                string[] signatures;
                foreach (cand; candidates)
                {
                    bool candIsOrnVarArgs = cand.declaration !is null && cand.declaration.isOrnVarArgs;
                    size_t realParamCount = cand.paramTypes.length;

                    if (candIsOrnVarArgs && realParamCount >= 2 && cand.paramTypes[$-1] is null)
                        realParamCount -= 2;
                    else if (realParamCount > 0 && cand.paramTypes[$-1] is null)
                        realParamCount -= 1; // Apenas o varargs normal

                    string sig = "";
                    if (realParamCount > 0)
                    {
                        sig = cand.paramTypes[0 .. realParamCount]
                            .filter!(t => t !is null)
                            .map!(t => t.toStr())
                            .join(", ");
                    }

                    if (cand.paramTypes.length > 0 && cand.paramTypes[$-1] is null)
                        sig ~= sig.length > 0 ? ", ..." : "...";

                    signatures ~= format("  %s(%s) -> %s", 
                        cand.name, sig, cand.returnType.toStr());
                }

                reportError(
                    format("No matching function '%s' for arguments (%s).",
                        funcName,
                        argTypes.map!(t => t.toStr()).join(", ")),
                    expr.loc,
                    [Suggestion(format("Available overloads:\n%s", signatures.join("\n")))]
                );
            }
            else
                reportError(
                    format("Function '%s' not declared in this scope.", funcName),
                    expr.loc,
                    [Suggestion("Check if the function is defined or imported correctly")]
                );

            return new PrimitiveType(BaseType.Any);
        }

        bool hasVariadic = false;
        bool isOrnVarArgs = funcSym.declaration !is null && funcSym.declaration.isOrnVarArgs;
        size_t minArgs = funcSym.paramTypes.length;
        size_t paramsToSkip = 0;

        if (funcSym.paramTypes.length > 0 && funcSym.paramTypes[$-1] is null)
        {
            hasVariadic = true;
            minArgs = cast(int) funcSym.paramTypes.length - 1;

            // Se for OrnVarArgs, ignora o parâmetro OrnVarArgs sintético
            if (isOrnVarArgs && minArgs > 0)
            {
                paramsToSkip = 1;
                minArgs--;
            }
        }

        if (hasVariadic)
        {
            if (expr.args.length < minArgs)
            {
                reportError(
                    format("Function '%s' expects at least %d arguments, but received %d.",
                        funcName, minArgs, expr.args.length),
                    expr.loc,
                    [Suggestion(format("Add %d more argument(s)", minArgs - expr.args.length))]
                );
                return funcSym.returnType;
            }
        }
        else
            if (expr.args.length != funcSym.paramTypes.length - paramsToSkip)
            {
                reportError(
                    format("Function '%s' expects exactly %d arguments, but received %d.",
                        funcName, funcSym.paramTypes.length - paramsToSkip, expr.args.length),
                    expr.loc
                );
                return funcSym.returnType;
            }

        // Valida tipos dos argumentos fixos
        foreach (i; 0 .. minArgs)
        {
            Type paramType = funcSym.paramTypes[i];
            Type argType = argTypes[i];

            if (!paramType.isCompatibleWith(argType))
                reportError(
                    format("Argument #%d type mismatch: expected '%s', but received '%s'",
                        i + 1, paramType.toStr(), argType.toStr()),
                    expr.args[i].loc
                );
        }

        // Configura flags de varargs
        expr.isVarArg = hasVariadic;
        expr.isOrnVarArgs = funcSym.declaration.isOrnVarArgs;
        expr.isVarArgAt = funcSym.declaration.isVarArgAt;
        expr.isExternalCall = funcSym.isExternal;

        expr.mangledName = funcSym.declaration.mangledName;
        expr.resolvedType = funcSym.returnType;
        return funcSym.returnType;
    }

    string getAvailableMethods(StructType structType)
    {
        if (structType.methods.length == 0)
            return "(none)";
        string[] methodList;
        foreach (methodName, overloads; structType.methods)
            foreach (overload; overloads)
            {
                string sig = overload.funcDecl.args
                    .filter!(a => a.resolvedType !is null)
                    .map!(a => a.resolvedType.toStr())
                    .join(", ");
                methodList ~= format("%s(%s)", methodName, sig);
            }
        return methodList.join(", ");
    }

    // TODO: melhorar o sistema para validar casos mais profundos
    Node getIntLiteral(Node target)
    {
        if (IntLit intLit = cast(IntLit) target)
                return intLit;
        if (Identifier id = cast(Identifier) target) {
            VarSymbol sym = cast(VarSymbol) ctx.lookup(id.value.get!string);
            return getIntLiteral(sym.value);
        }
        return null;
    }

    Type checkIndexExpr(IndexExpr expr)
    {
        Type targetType = checkExpression(expr.target);
        Type indexType = checkExpression(expr.index);

        if (!indexType.isNumeric())
            reportError(format("The index must be an integer, it was obtained as '%s'.",
                    indexType.toStr()), expr.index.loc);
        
        if (ArrayType arrType = cast(ArrayType) targetType)
        {
            Node value = getIntLiteral(expr.index);
            if (value !is null)
            {
                int val = value.value.get!int;
                if ((cast(int)arrType.length - 1) < val)
                    reportError(format("Array index '%d' out of bounds '%d'.",
                        val, arrType.length), expr.loc);
            }
            expr.resolvedType = arrType.elementType;
            return arrType.elementType;
        }

        if (auto ptrType = cast(PointerType) targetType)
        {
            expr.resolvedType = ptrType.pointeeType;
            return ptrType.pointeeType;
        }

        if (PrimitiveType primitive = cast(PrimitiveType) targetType)
        {
            if (primitive.baseType == BaseType.String) {
                expr.resolvedType = new PrimitiveType(BaseType.Char);
                return new PrimitiveType(BaseType.Char);
            }
        }

        if (StructType st = cast(StructType) targetType)
        {
            if (st.hasMethod("opIndex"))
            {
                StructMethod* method = st.getMethod("opIndex");
                expr.mangledName = method.funcDecl.mangledName;
                return method.funcDecl.resolvedType;
            }
        }

        reportError(format("'%s' is not indexable.", targetType.toStr()), expr.target.loc);
        return new PrimitiveType(BaseType.Any);
    }

    Type checkArrayLiteral(ArrayLit expr)
    {
        if (expr.elements.length == 0)
            // Array vazio - tipo genérico
            return new ArrayType(new PrimitiveType(BaseType.Any));
        
        // Infere tipo do primeiro elemento
        Type elemType = checkExpression(expr.elements[0]);

        // Verifica que todos elementos são compatíveis
        foreach (i, elem; expr.elements[1 .. $])
        {
            Type thisType = checkExpression(elem);
            if (!thisType.isCompatibleWith(elemType))
                reportError(format(
                        "Element %d of the array has an incompatible type: expected '%s', got '%s'",
                        i + 2, elemType.toStr(), thisType.toStr()), elem.loc);
        }

        ArrayType t = new ArrayType(elemType);
        t.length = expr.elements.length;
        expr.resolvedType = t;
        return t;
    }
}
