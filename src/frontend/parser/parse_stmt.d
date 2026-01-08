module frontend.parser.parse_stmt;
import frontend;

mixin template ParseStmt()
{
    Node parseStatement()
    {
        switch (this.peek().kind)
        {
        case TokenKind.Return:
            return parseReturn();
        case TokenKind.If:
            return parseIfStmt();
        case TokenKind.For:
            return parseForStmt();
        case TokenKind.While:
            return parseWhileStmt();
        case TokenKind.Break:
            return new BrkOrCntStmt(true, this.advance().loc);
        case TokenKind.Continue:
            return new BrkOrCntStmt(false, this.advance().loc);
        case TokenKind.Import:
            return parseImportStmt();
        default:
            return null;
        }
    }

    ImportStmt parseImportStmt()
    {
        string[] symbols = null;
        string aliasname = "";
        this.advance();

        if (this.match([TokenKind.LBrace]))
        {
            while (!this.check(TokenKind.RBrace) && !this.isAtEnd())
            {
                symbols ~= this.consume(TokenKind.Identifier, "").value.get!string;
                this.match([TokenKind.Comma]);
            }
            this.consume(TokenKind.RBrace, "Expected '}' after selective import.");
            this.consume(TokenKind.From, "Expected 'from' after selective import.");
        }

        Node file = this.parseExpression();
        import std.path : buildPath, absolutePath, dirName, extension;

        ImportStmt node = new ImportStmt(file.loc, symbols, aliasname).setModulePath(file);

        string filename = node.modulePath;
        if (filename.extension != ".orn") filename ~= ".orn";
        
        string resolvedPath = buildPath(pathRoot, filename);
        if (!exists(resolvedPath))
        {
            loadEnv();
            string stdPath = buildPath(MAIN_DIR, filename);
            
            if (exists(stdPath))
                resolvedPath = stdPath;
            else
            {
                reportError("File not found (neither local nor stdlib): " ~ filename, node.loc);
                return node;
            }
        }

        string fullPath = absolutePath(resolvedPath);
        if (fullPath in modulesCache)
            return node;

        // o que interessa são as structs apenas, apenas StructDecl, o resto se foda
        TypeRegistry tr;
        tr = registry;

        string src = readText(fullPath);
        Lexer lexer = new Lexer(fullPath, src, dirName(fullPath), this.error);
        Token[] tokens = lexer.tokenize();
        Parser parser = new Parser(tokens, this.error, tr, pathRoot);
        parser.parseProgram();

        modulesCache[fullPath] = true;

        return node;
    }

    WhileStmt parseWhileStmt()
    {
        Loc loc = this.advance().loc;
        Node condition = this.parseExpression();
        Node[] body = this.parseBody(true);
        return new WhileStmt(condition, body, this.getLoc(loc, this.previous().loc));
    }

    ForStmt parseForStmt()
    {
        bool loop = false;
        if (this.inForLoopHeader)
            loop = true;
        else
            this.inForLoopHeader = true;
        Loc start = this.advance().loc;
        Node init, condition, increment = null;
        Node[] body;

        if (!this.match([TokenKind.SemiColon]))
        {
            init = this.parse();
            this.consume(TokenKind.SemiColon, "Expected ';'.");
        }
        if (!this.match([TokenKind.SemiColon]))
        {
            condition = this.parseExpression();
            this.consume(TokenKind.SemiColon, "Expected ';'.");
        }
        if (!this.match([TokenKind.SemiColon]))
        {
            increment = this.parseExpression();
            body = this.parseBody(true);
        }
        if (this.inForLoopHeader && loop)
            this.inForLoopHeader = false;
        return new ForStmt(init, condition, increment, body, this.getLoc(start, increment.loc));
    }

    IfStmt parseIfStmt()
    {
        Loc loc = this.advance().loc;
        Node condition = this.parseExpression();
        Node[] body = this.parseBody(true);
        Node else_ = null;

        if (this.peek().kind == TokenKind.Else)
        {
            Loc elseLoc = this.advance().loc;

            if (this.peek().kind == TokenKind.If)
            {
                Node ifStmt = this.parseIfStmt();
                else_ = ifStmt;
            }
            else
            {
                Node[] elseBody = this.parseBody(true);
                Node elseStmt = new IfStmt(null, new BlockStmt(elseBody, elseLoc), null, elseLoc);
                else_ = elseStmt;
            }
        }

        return new IfStmt(condition, new BlockStmt(body, loc), else_, loc);
    }

    ReturnStmt parseReturn()
    {
        Loc loc = this.advance().loc;
        Node value = null;
        if (!this.match([TokenKind.SemiColon]))
            value = this.parseExpression();
        return new ReturnStmt(value, loc);
    }

    Node[] parseBody(bool uniqueStmt = false)
    {
        Node[] body_ = [];
        if (!this.check(TokenKind.LBrace) && !uniqueStmt)
        {
            reportError("'{' was expected to start the body.", this.peek().loc);
            return body_;
        }
        if (this.check(TokenKind.LBrace))
        {
            this.consume(TokenKind.LBrace, "It was expected to use '{' to start the body.");
            while (!this.check(TokenKind.RBrace) && !this.isAtEnd())
                body_ ~= this.parse();
            this.consume(TokenKind.RBrace, "It was expected '}' after the body.");
        }
        else
            body_ ~= this.parse();

        return body_;
    }
}
