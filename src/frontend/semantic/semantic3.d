module frontend.semantic.semantic3;

import frontend;
import common.reporter;

class Semantic3
{
    Context ctx;
    TypeChecker checker;
    DiagnosticError error;
    FunctionAnalyzer funcAnalyzer;
    TypeRegistry registry;
    string pathRoot;

    this(Context ctx, DiagnosticError error, TypeRegistry registry, string pathRoot, TypeChecker checker)
    {
        this.ctx = ctx;
        this.error = error;
        this.checker = checker;
        this.funcAnalyzer = new FunctionAnalyzer(ctx, this.checker, this.error, this);
        this.checker.funcAnalyzer = this.funcAnalyzer;
        this.registry = registry;
        this.pathRoot = pathRoot;
    }

    pragma(inline, true)
    void reportError(string message, Loc loc, Suggestion[] suggestions = null)
    {
        error.addError(Diagnostic(message, loc, suggestions));
    }

    void analyze(Program program)
    {
        foreach (node; program.body)
            analyzeDeclaration(node);

        Node[] body;
        if (checker.structs.length > 0)
            body ~= checker.structs;
        
        if (checker.funcs.length > 0)
            body ~= checker.funcs;

        body ~= program.body;
        program.body = body;
    }

    Node analyzeDeclaration(Node node)
    {
        if (auto varDecl = cast(VarDecl) node)
            analyzeVarDecl(varDecl, true);
        else if (auto funcDecl = cast(FuncDecl) node)
            this.funcAnalyzer.analyzeFunction(funcDecl);
        else if (auto structDecl = cast(StructDecl) node)
            analyzeStructDecl(structDecl);
        else if (auto enm = cast(EnumDecl) node)
            return enm;
        else if (auto un = cast(UnionDecl) node)
            analyzeUnionDecl(un);
        else
            analyzeStatement(node);
        return node;
    }

    void analyzeUnionDecl(UnionDecl decl)
    {
        UnionSymbol sym = ctx.lookupUnion(decl.name);
        if (sym is null)
        {
            reportError(format("Union '%s' not found in context.", decl.name), decl.loc);
            return;
        }

        foreach (ref field; decl.fields)
        {
            if (field.defaultValue !is null)
            {
                Type valueType = checker.checkExpression(field.defaultValue);
                if (!field.resolvedType.isCompatibleWith(valueType))
                    reportError(
                        format("Default value for field '%s' has incompatible type: expected '%s', got '%s'",
                               field.name, field.resolvedType.toStr(), valueType.toStr()),
                        field.defaultValue.loc
                    );
            }
        }
    }

    void analyzeStructDecl(StructDecl decl)
    {
        StructSymbol structSym = ctx.lookupStruct(decl.name);
        if (structSym is null)
        {
            reportError(format("Struct '%s' not found in context.", decl.name), decl.loc);
            return;
        }

        if (decl.isTemplate)
            return;
        
        foreach (ref field; decl.fields)
        {
            Node value = field.defaultValue;
            if (value !is null)
            {
                Type valueType = checker.checkExpression(value);
                checker.makeImplicitCast(value, field.resolvedType);
                field.defaultValue = value;
                if (!field.resolvedType.isCompatibleWith(valueType))
                {
                    reportError(
                        format("Default value for field '%s' has incompatible type: expected '%s', got '%s'",
                               field.name, field.resolvedType.toStr(), valueType.toStr()),
                        value.loc
                    );
                }
            }
        }

        foreach (methodName, overloads; decl.methods)
            foreach (ref method; overloads)
                analyzeStructMethod(structSym, method);
    }

    void analyzeStructMethod(StructSymbol structSym, ref StructMethod method)
    {
        FuncDecl funcDecl = method.funcDecl;
        foreach (ref param; funcDecl.args)
        {
            if (param.value !is null)
            {
                // Type check do valor padrão
                Type valueType = checker.checkExpression(param.value);
                // Verifica compatibilidade
                if (!param.resolvedType.isCompatibleWith(valueType))
                {
                    reportError(
                        format("Default value for parameter '%s' has incompatible type: expected '%s', got '%s'",
                               param.name, param.resolvedType.toStr(), valueType.toStr()),
                        param.value.loc
                    );
                }
            }
        }
    
        if (funcDecl.body !is null)
        {
            ctx.enterStruct(structSym);
            
            Type[] paramTypes;
            foreach (param; funcDecl.args)
                paramTypes ~= param.resolvedType;
            
            auto funcSym = new FunctionSymbol(
                funcDecl.name,
                paramTypes,
                funcDecl.resolvedType,
                funcDecl,
                funcDecl.loc
            );
            
            ctx.enterFunction(funcSym);
            
            foreach (i, param; funcDecl.args)
            {
                if (!ctx.addVariable(param.name, param.resolvedType, false, param.loc))
                {
                    reportError(
                        format("Parameter '%s' already defined", param.name),
                        param.loc
                    );
                }
            }
            
            Program program = new Program(funcDecl.body.statements);
            new Semantic1(ctx, registry, error, pathRoot, checker).analyze(program);
            new Semantic2(ctx, error, registry, null, checker).analyze(program);
        
            analyzeBlockStmt(funcDecl.body);
            
            if (!method.isConstructor && !funcDecl.resolvedType.isVoid())
                if (!hasReturn(funcDecl.body))
                    reportError(
                        format("Method '%s' must return a value of type '%s'",
                               funcDecl.name, funcDecl.resolvedType.toStr()),
                        funcDecl.loc
                    );
            
            ctx.exitFunction();
            ctx.exitStruct();
        }
    }

    Identifier getIdentifier(Node target)
    {
        if (Identifier id = cast(Identifier) target)
            return id;
        if (UnaryExpr expr = cast(UnaryExpr) target)
            if (expr.op == "*")
                return getIdentifier(expr.operand);
        if (MemberExpr member = cast(MemberExpr) target)
            return getIdentifier(member.target);
        if (IndexExpr idx = cast(IndexExpr) target)
            return getIdentifier(idx.target);
        return null;
    }

    Node analyzeAssignDecl(Node node)
    {
        AssignDecl decl = cast(AssignDecl) node;
        Type leftType = checker.checkExpression(decl.left);
        Type rightType = checker.checkExpression(decl.right);

        // caso seja o refConst mas a variavel seja um let, permite o assign
        Identifier id = getIdentifier(decl.left);
        string varName = id !is null ? id.value.get!string : "";
        VarSymbol sym = varName != "" ? ctx.lookupVariable(varName) : null;

        // if (leftType.refConst)
        // {
        //     writeln("Inside");
        //     if (varName != "")
        //     {
        //         if (sym !is null)
        //             if (sym.isConst || decl.left.kind == NodeKind.UnaryExpr)
        //             {
        //                 reportError(
        //                     format("Cannot assign to constant '%s'.", varName), decl.loc, 
        //                         [Suggestion("constants cannot be modified after initialization")]
        //                 );
        //                 return decl;
        //             }
        //     }
        //     else
        //         reportError(
        //             format("Cannot assign to constant '%s'.", varName), decl.loc, 
        //                 [Suggestion("constants cannot be modified after initialization")]
        //         );
        //         return decl;
        // } else if (sym.isConst)
        // {
        //     reportError(
        //         format("Cannot assign to constant '%s'.", varName), decl.loc, 
        //             [Suggestion("constants cannot be modified after initialization")]
        //     );
        //     return decl;
        // }

        if (UnaryExpr unary = cast(UnaryExpr) decl.left)
        {
            if (unary.op == "*") 
            {
                Type ptrType = checker.checkExpression(unary.operand);
                if (ptrType.refConst)
                {
                    reportError(
                        format("Cannot modify value through pointer '%s' because it points to a constant (refConst).", 
                        ptrType.toStr()), 
                        decl.loc
                    );
                    decl.resolvedType = new PrimitiveType(BaseType.Any);
                    return decl;
                }
            }
        }

        if (sym.isConst)
        {
            reportError(
                format("Cannot assign to constant '%s'.", varName), decl.loc, 
                    [Suggestion("constants cannot be modified after initialization")]
            );
            return decl;
        }
        
        checker.makeImplicitCast(decl.right, leftType);
        rightType = decl.right.resolvedType;
        checker.checkTypeComp(leftType, rightType, decl.loc);

        if (decl.op != "=")
            if (!isValidCompoundAssignment(leftType, rightType, decl.op))
                reportError(format("The operator '%s' is invalid for types '%s' and '%s'.",
                        decl.op, leftType.toStr(), rightType.toStr()), decl.loc);

        if (sym !is null)
        {
            sym.value = decl.right;
            sym.type.refConst = rightType.refConst;
        }
        
        decl.resolvedType = leftType;
        return decl;
    }

    bool isValidCompoundAssignment(Type left, Type right, string op)
    {
        PrimitiveType primLeft = cast(PrimitiveType) left;
        PrimitiveType primRight = cast(PrimitiveType) right;

        if (primLeft is null || primRight is null)
            return false;

        if (op == "+=" || op == "-=" || op == "*=" || op == "/=" || op == "%=")
            return primLeft.isNumeric() && primRight.isNumeric();

        if (op == "&=" || op == "|=" || op == "^=" || op == "<<=" || op == ">>=")
            return isIntegerType(primLeft.baseType) && isIntegerType(primRight.baseType);

        return false;
    }

    bool isIntegerType(BaseType type)
    {
        return type == BaseType.Int || type == BaseType.Long;
    }

    void analyzeVarDecl(VarDecl decl, bool isGlobal = false)
    {
        decl.isGlobal = isGlobal;
        Node init_ = decl.value.get!Node;
        VarSymbol sym = ctx.lookupVariable(decl.id);

        if (init_ !is null)
        {
            Type initType = checker.checkExpression(init_);
            if (sym !is null)
                sym.value = init_;

            if (decl.resolvedType !is null)
            {
                // writeln("I: ", initType.refConst);
                // writeln("D: ", decl.resolvedType.refConst);
                checker.makeImplicitCast(init_, decl.resolvedType);
                decl.value = Variant(init_);
                initType = init_.resolvedType;
                if (!decl.resolvedType.isCompatibleWith(initType))
                {
                    reportError(
                        format("Incompatible type: expected '%s', got '%s'",
                            decl.resolvedType.toStr(), initType.toStr()), 
                        init_.loc
                    );
                }

                if (sym !is null)
                {
                    sym.value = init_;
                    // if (!decl.isConst)
                    //     sym.isConst = initType.refConst;
                    sym.type = decl.resolvedType;
                    sym.type.refConst = initType.refConst;
                }
            }
            else
            {
                decl.resolvedType = initType;
                if (sym !is null)
                    sym.type = initType;
            }
        }
        else if (decl.resolvedType is null)
        {
            reportError(
                format("The variable '%s' needs a type or initializer.", decl.id), 
                decl.loc
            );
        }
    }

    void analyzeBlockStmtSema(BlockStmt stmt)
    {
        // corpo analisado
        Program program = new Program(stmt.statements);
        new Semantic1(ctx, registry, error, pathRoot, checker).analyze(program);
        new Semantic2(ctx, error, registry, null, checker).analyze(program);
    }

    void analyzeBlockStmt(BlockStmt stmt, bool sema = false, bool isGlobal = false)
    {
        if (sema)
            analyzeBlockStmtSema(stmt);

        ctx.enterScope("block");

        foreach (i, node; stmt.statements)
            stmt.statements[i] = analyzeStatement(node, sema, isGlobal);

        ctx.exitScope();
    }

    Node analyzeStatement(Node stmt, bool sema = false, bool isGlobal = false)
    {
        if (auto varDecl = cast(VarDecl) stmt)
            analyzeVarDecl(varDecl, isGlobal);
        else if (auto ifStmt = cast(IfStmt) stmt)
            analyzeIfStmt(ifStmt);
        else if (auto whileStmt = cast(WhileStmt) stmt)
            analyzeWhileStmt(whileStmt);
        else if (auto forStmt = cast(ForStmt) stmt)
            analyzeForStmt(forStmt);
        else if (auto returnStmt = cast(ReturnStmt) stmt)
            analyzeReturnStmt(returnStmt);
        else if (auto call = cast(CallExpr) stmt)
            analyzeCall(call);
        else if (auto brkc = cast(BrkOrCntStmt) stmt)
            analyzeBrkOrCntStmt(brkc);
        else if (auto blockStmt = cast(BlockStmt) stmt)
            analyzeBlockStmt(blockStmt, sema);
        else if (auto assign = cast(AssignDecl) stmt)
            return analyzeAssignDecl(assign);
        else if (UnaryExpr unary = cast(UnaryExpr) stmt) {
            unary.operand.resolvedType = checker.checkExpression(unary.operand);
            unary.resolvedType = unary.operand.resolvedType;
        }
        else if (auto defer = cast(DeferStmt) stmt)
            defer.stmt = analyzeStatement(defer.stmt);
        return stmt;
    }

    pragma(inline, true);
    void analyzeCall(CallExpr call)
    {
        for (long i; i < call.args.length; i++)
            call.args[i].resolvedType = checker.checkExpression(call.args[i]);
        call.resolvedType = checker.checkExpression(call);
    }

    void analyzeIfStmt(IfStmt stmt)
    {
        // Verifica condição
        if (stmt.condition !is null)
        {
            Type condType = checker.checkExpression(stmt.condition);
            stmt.condition.resolvedType = condType;

            if (!condType.isCompatibleWith(new PrimitiveType(BaseType.Bool)))
                reportError(format("The 'if' condition must be logical, it was obtained by '%s'.",
                        condType.toStr()), stmt.condition.loc);
        }


        if (stmt.thenBranch !is null)
            analyzeStatement(stmt.thenBranch, true);

        if (stmt.elseBranch !is null)
            analyzeStatement(stmt.elseBranch, true);
    }

    void analyzeWhileStmt(WhileStmt stmt)
    {
        Type condType = checker.checkExpression(stmt.condition);
        if (!condType.isCompatibleWith(new PrimitiveType(BaseType.Bool)))
            reportError("The condition in the 'while' loop must be logical.", stmt.condition.loc);

        ctx.enterLoop();
        analyzeStatement(stmt.body, true);
        ctx.exitLoop();
    }

    void analyzeForStmt(ForStmt stmt)
    {
        VarDecl decl = null;

        // Analisa inicializador
        if (stmt.init_ !is null) {
            analyzeStatement(stmt.init_);
            if (stmt.init_.kind == NodeKind.VarDecl)
                decl = cast(VarDecl) stmt.init_;
                ctx.addVariable(decl.id, stmt.init_.resolvedType, decl.isConst, decl.loc);
        }

        // Analisa condição
        if (stmt.condition !is null)
        {
            Type condType = checker.checkExpression(stmt.condition);
            if (!condType.isCompatibleWith(new PrimitiveType(BaseType.Bool)))
                reportError("The condition in the 'for' loop must be logical.", stmt.condition.loc);
        }

        // Analisa incremento
        if (stmt.increment !is null)
            checker.checkExpression(stmt.increment);

        // Analisa corpo
        ctx.enterLoop();
        analyzeStatement(stmt.body, true);
        ctx.exitLoop();
    }

    void analyzeReturnStmt(ReturnStmt stmt)
    {
        if (!ctx.isInFunction())
        {
            reportError("Using 'return' outside of a function.", stmt.loc);
            return;
        }

        Type returnType = stmt.value !is null ?
            checker.checkExpression(stmt.value) : VoidType.instance();
        Type expectedType = ctx.currentFunction.returnType;

        if (expectedType.toStr() != returnType.toStr())
            if (!expectedType.isCompatibleWith(returnType))
                reportError(format("Incompatible return type: expected '%s', received '%s'",
                        expectedType.toStr(), returnType.toStr()), stmt.loc);
    }

    void analyzeBrkOrCntStmt(BrkOrCntStmt stmt)
    {
        if (!ctx.isInLoop())
            reportError(format("'%s' cannot be used outside of a loop..", stmt.isBreak ? "break" : "continue"), 
                stmt.loc);
    }

    bool hasReturn(BlockStmt block)
    {
        foreach (stmt; block.statements)
        {
            if (cast(ReturnStmt) stmt)
                return true;

            // Verifica em if/else
            if (auto ifStmt = cast(IfStmt) stmt)
            {
                if (ifStmt.elseBranch !is null)
                {
                    bool thenHas = false, elseHas = false;

                    if (auto thenBlock = cast(BlockStmt) ifStmt.thenBranch)
                        thenHas = hasReturn(thenBlock);

                    if (auto elseBlock = cast(BlockStmt) ifStmt.elseBranch)
                        elseHas = hasReturn(elseBlock);

                    if (thenHas && elseHas)
                        return true;
                }
            }
        }
        return false;
    }
}
