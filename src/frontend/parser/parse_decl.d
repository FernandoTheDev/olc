module frontend.parser.parse_decl;
import frontend;

mixin template ParseDecl()
{
    Node parseDeclaration()
    {
        switch (this.peek().kind)
        {
        case TokenKind.Struct:
            return this.parseStructDecl();

        case TokenKind.Let:
        case TokenKind.Const:
            return this.parseVarDecl(advance().loc, this.previous().kind == TokenKind.Const);

        case TokenKind.Fn:
            return this.parseFuncDecl();

        case TokenKind.Enum:
            return this.parseEnumDecl();

        case TokenKind.Union:
            return this.parseUnionDecl();

        default:
            reportError("Unrecognized declaration.", this.peek().loc);
            return null;
        }
    }

    Node parseUnionDecl()
    {
        Loc start = advance().loc; // consome 'union'
        string id = this.consume(TokenKind.Identifier, "Expected an identifier for union name.").value.get!string;
        bool noMangle;
    
        StructField[] fields;
        
        if (!registry.typeExists(id))
            registry.registerType(id, new UnionType(id, fields)); 
            
        this.consume(TokenKind.LBrace, "Expected '{' after union name.");
        while (!this.isAtEnd() && !this.check(TokenKind.RBrace))
        {
            Token field = this.consume(TokenKind.Identifier, "Expected an identifier to field name.");
            this.consume(TokenKind.Colon, "Expected ':' after field name.");
            TypeExpr type = this.parseType();
            Node value = null;
            if (this.match([TokenKind.Equals]))
                value = this.parseExpression();
            fields ~= StructField(field.value.get!string, type, null, value, field.loc);
        }

        this.consume(TokenKind.RBrace, "Expected '}' after union body.");
        return new UnionDecl(id, fields, this.getLoc(start, this.previous().loc), noMangle);
    }

    Node parseEnumDecl()
    {
        Loc start = advance().loc;
        
        Token idToken = this.consume(TokenKind.Identifier, "Expected an identifier for enum name.");
        string id = idToken.value.get!string;
        bool noMangle;
        TypeExpr type = new NamedTypeExpr(BaseType.Int, Loc.init);

        if (this.match([TokenKind.Colon]))
            type = this.parseType();

        this.consume(TokenKind.LBrace, "Expected '{' after enum name.");
        
        long[string] members;
        long currentValue = 0; // Contador automático para valores implícitos

        while (!this.check(TokenKind.RBrace) && !this.isAtEnd())
        {
            Token memberId = this.consume(TokenKind.Identifier, "Expected enum member name.");
            string memberName = memberId.value.get!string;
            
            if (this.match([TokenKind.Equals]))
            {
                Node expr = this.parseExpression();
                if (auto intLit = cast(IntLit) expr)
                    currentValue = to!long(intLit.value.get!int);
                else if (auto longLit = cast(LongLit) expr)
                    currentValue = longLit.value.get!long;
                else
                    reportError("Enum member value must be an integer literal.", expr.loc);
            }
            
            members[memberName] = currentValue;
            currentValue++; // Prepara para o próximo (auto-incremento)
            
            // Consome vírgula opcional (permite trailing comma)
            this.match([TokenKind.Comma]);
        }
        
        this.consume(TokenKind.RBrace, "Expected '}' after enum body.");
        
        if (!registry.typeExists(id))
            registry.registerType(id, new EnumType(id, members));
            
        return new EnumDecl(id, members, this.getLoc(start, this.previous().loc), type, noMangle);
    }

    StructDecl parseStructDecl()
    {
        // struct User { string name; int age = 17; User next = null; }
        Loc start = advance().loc;
        string id = this.consume(TokenKind.Identifier, "Expected an identifier to struct name.").value.get!string;
        bool noMangle = false;

        StructField[] fields;
        StructMethod[][string] methods;
        // precisa criar um tipo do usuario temporariamente pra conseguir identificar isso como uma struct
        // o typeresolver corrigirá isso no futuro
        if (!registry.typeExists(id))
            registry.registerType(id, new StructType(id, fields, methods)); // registra uma base temporariamente

        this.consume(TokenKind.LBrace, "Expected '{' after struct name.");

        // o corpo é basicamente composto por declarações de variaveis e de declarações de funções
        while (!this.isAtEnd() && !this.check(TokenKind.RBrace))
        {
            if (this.check(TokenKind.Fn))
            {
                FuncDecl fn = cast(FuncDecl) this.parseDeclaration();
                methods[fn.name] ~= StructMethod(fn, false, fn.loc);
                continue;
            }
            Token fid = this.consume(TokenKind.Identifier, "Expected an identifier to field name.");
            Node fvalue = null;
            this.consume(TokenKind.Colon, "Expected ':' after field name.");
            TypeExpr ftype = this.parseType();
            if (this.match([TokenKind.Equals]))
                fvalue = this.parseExpression();
            fields ~= StructField(fid.value.get!string, ftype, null, fvalue, fid.loc);
        }

        this.consume(TokenKind.RBrace, "Expected '}' after struct body.");
        StructDecl decl = new StructDecl(id, fields, methods, this.getLoc(start, this.previous().loc), noMangle);
        return decl;
    }

    FuncDecl parseFuncDecl()
    {
        advance();
        bool isVarArg, isExtern, noMangle, isOrnVarArgs;
        Token id = this.consume(TokenKind.Identifier, "Expected an identifier to function name.");
        FuncArgument[] arguments;
        this.consume(TokenKind.LParen, "Expected '(' after the function name.");
        while (!this.check(TokenKind.RParen))
        {
            if (this.match([TokenKind.Variadic]))
            {
                if (this.match([TokenKind.Identifier]))
                {
                    isOrnVarArgs = true;
                    Token arg = this.previous();
                    arguments ~= FuncArgument(arg.value.get!string, new NamedTypeExpr("OrnVarArgs", arg.loc), Type.init, 
                        null, arg.loc);        
                }
                arguments ~= FuncArgument("...", null, null, null, this.previous().loc, true);
                isVarArg = true;
                // Varargs deve ser o último argumento
                if (!this.check(TokenKind.RParen))
                    reportError("Variadic arguments must be the last parameter.", this.peek().loc);
                break;
            }
            Token argId = this.consume(TokenKind.Identifier, "An identifier is expected for the argument name.");
            this.consume(TokenKind.Colon, "Expected ':' after argument name.");
            TypeExpr type = this.parseType();
            Node valueDefault = null;
            if (this.match([TokenKind.Equals]))
                valueDefault = this.parseExpression();
            arguments ~= FuncArgument(argId.value.get!string, type, Type.init, valueDefault,
                argId.loc);
            this.match([TokenKind.Comma]);
        }
        this.consume(TokenKind.RParen, "Expected ')' after the function arguments.");
        TypeExpr funcType = new NamedTypeExpr(BaseType.Void, Loc.init); 
        if (this.match([TokenKind.Arrow]))
            funcType = this.parseType();
        // this.consume(TokenKind.Arrow, "Expected '->' after arguments.");
        Node[] body = [];
        if (this.match([TokenKind.SemiColon]))
            isExtern = true;
        else
            body = parseBody();

        return new FuncDecl(id.value.get!string, arguments, body, funcType, id.loc, isVarArg, isExtern, noMangle, 
            isOrnVarArgs);
    }

    VarDecl parseVarDecl(Loc start, bool isConst = false)
    {
        Token id = Token.init;
        TypeExpr type = TypeExpr.init;
        bool isOd = false;
        
        if (this.match([TokenKind.LBrace]))
            isOd = true;
        
        id = this.consume(TokenKind.Identifier, "Expected an identifier to variable name.");
        this.consume(TokenKind.Colon, "Expected ':' after variable name.");
        type = this.parseType();
        type.constant = isConst;
        type.refConst = isConst;
        Node value = null;

        if (isOd)
            this.consume(TokenKind.RBrace, "Expected '}' after object destructuring.");

        if (!this.check(TokenKind.SemiColon))
        {
            this.consume(TokenKind.Equals, "Expected '=' after the variable declaration.");
            value = this.parseExpression();
        }
        else if (!isOd)
            this.advance();

        // transforma em um memberExpr
        if (isOd && value !is null)
            value = new MemberExpr(value, id.value.get!string, value.loc);

        return new VarDecl(id.value.get!string, type, value, isConst, this.getLoc(start, 
            (value is null ? this.previous().loc : value.loc)));
    }
}
