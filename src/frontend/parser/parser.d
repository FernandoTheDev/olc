module frontend.parser.parser;
import frontend, common.reporter;

enum Precedence
{
    LOWEST = 1,
    ASSIGN = 2, // =, +=, -=, |=, &=, <<=, >>=
    EQUALS = 3, // ==, !=
    OR = 4, // ||
    AND = 5, // &&
    BIT_OR = 6, // |
    BIT_XOR = 7, // ^
    BIT_AND = 8, // &
    SUM = 9, // +, -
    MUL = 10, // *, /
    BIT_SHIFT = 11, // <<, >>
    CALL = 12, // funções, index
    HIGHEST = 13,
}

class Parser
{
private:
    Token[] tokens;
    ulong pos = 0; // offset
    DiagnosticError error;
    TypeRegistry registry;
    bool[string] modulesCache;
    string pathRoot;
    public bool inForLoopHeader = false;

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

    mixin ParseDecl!();
    mixin ParseExpr!();
    mixin ParseStmt!();
    mixin ParseType!();

    Node parse()
    {
        immutable ulong startPos = this.pos;

        // Só consome semicolons opcionais se NÃO estivermos em um for loop header
        // E só tenta parse novamente se NÃO for um } (fim de bloco) ou EOF
        if (!inForLoopHeader && this.match([TokenKind.SemiColon]))
        {
            if (!this.check(TokenKind.RBrace) && !this.isAtEnd())
                return parse();
            else
                return null; // Semicolon redundante antes de } ou EOF
        }

        if (this.isDeclaration())
        {
            if (auto node = this.parseDeclaration())
            {
                // Só consome semicolons opcionais se NÃO estivermos em um for loop header
                if (!inForLoopHeader)
                    this.match([TokenKind.SemiColon]);
                return node;
            }
            this.pos = startPos;
        }

        if (auto node = this.parseStatement())
        {
            // Só consome semicolons opcionais se NÃO estivermos em um for loop header
            if (!inForLoopHeader)
                this.match([TokenKind.SemiColon]);
            return node;
        }

        this.pos = startPos;

        if (auto node = this.parseExpression())
        {
            // Só consome semicolons opcionais se NÃO estivermos em um for loop header
            if (!inForLoopHeader)
                this.match([TokenKind.SemiColon]);
            return node;
        }

        throw new Exception("Error parsing.");
    }

    bool isDeclaration()
    {
        Token current = this.peek();
        switch (current.kind)
        {
        case TokenKind.Struct:
        case TokenKind.Let:
        case TokenKind.Const:
        case TokenKind.Fn:
        case TokenKind.Enum:
        case TokenKind.Union:
            return true;
        default:
            return false;
        }
    }

    pragma(inline, true);
    bool isAtEnd()
    {
        return this.peek().kind == TokenKind.Eof;
    }

    Variant next()
    {
        if (this.isAtEnd())
            return Variant(false);
        return Variant(this.tokens[this.pos + 1]);
    }

    pragma(inline, true);
    Token peek()
    {
        return this.tokens[this.pos];
    }

    pragma(inline, true);
    Token previous(ulong i = 1)
    {
        return this.tokens[this.pos - i];
    }

    pragma(inline, true);
    Token future(ulong i = 1)
    {
        return this.tokens[this.pos + i];
    }

    Token advance()
    {
        if (!this.isAtEnd())
            this.pos++;
        return this.previous();
    }

    bool match(TokenKind[] kinds)
    {
        foreach (kind; kinds)
        {
            if (this.check(kind))
            {
                this.advance();
                return true;
            }
        }
        return false;
    }

    bool check(TokenKind kind)
    {
        if (this.isAtEnd())
            return false;
        return this.peek().kind == kind;
    }

    Token consume(TokenKind expected, string message)
    {
        if (this.check(expected))
            return this.advance();
        reportError(format("Parsing error: %s", message), this.peek().loc);
        throw new Exception(format("Parsing error: %s", message));
    }

    // Método auxiliar para consumir semicolon obrigatório (usado em for loops)
    Token consumeSemicolon(string context = "semicolon")
    {
        return this.consume(TokenKind.SemiColon, format("Expected ';' in %s", context));
    }

    // Método auxiliar para consumir semicolon opcional (usado no resto do código)
    bool matchOptionalSemicolon()
    {
        if (!inForLoopHeader)
            return this.match([TokenKind.SemiColon]);
        return false;
    }

    Precedence getPrecedence(TokenKind kind)
    {
        switch (kind)
        {
        case TokenKind.Equals:
        case TokenKind.PlusEquals:
        case TokenKind.MinusEquals:
        case TokenKind.StarEquals:
        case TokenKind.SlashEquals:
        case TokenKind.ModuloEquals:
        case TokenKind.BitAndEquals:
        case TokenKind.BitOrEquals:
        case TokenKind.BitXorEquals:
        case TokenKind.BitSHLEquals:
        case TokenKind.BitSHREquals:
        case TokenKind.TildeEquals:
        case TokenKind.Or:
        case TokenKind.And:
            return Precedence.ASSIGN;

        case TokenKind.EqualsEquals:
        case TokenKind.NotEquals:
        case TokenKind.GreaterThan:
        case TokenKind.LessThan:
        case TokenKind.LessThanEquals:
        case TokenKind.GreaterThanEquals:
            return Precedence.EQUALS;

        case TokenKind.BitOr:
            return Precedence.BIT_OR;
        case TokenKind.BitXor:
            return Precedence.BIT_XOR;
        case TokenKind.BitAnd:
            return Precedence.BIT_AND;

        case TokenKind.Plus:
        case TokenKind.Minus:
        case TokenKind.PlusPlus:
        case TokenKind.MinusMinus:
        case TokenKind.Question:
            return Precedence.SUM;

        case TokenKind.Star:
        case TokenKind.Slash:
        case TokenKind.Modulo:
            return Precedence.MUL;

        case TokenKind.BitSHL:
        case TokenKind.BitSHR:
        case TokenKind.BitSAR:
            return Precedence.BIT_SHIFT;

        case TokenKind.LParen:
        case TokenKind.LBracket:
        case TokenKind.Dot:
        case TokenKind.As:
            return Precedence.CALL;

        default:
            return Precedence.LOWEST;
        }
    }

    pragma(inline, true);
    Precedence peekPrecedence()
    {
        return this.getPrecedence(this.peek().kind);
    }

    pragma(inline, true);
    Loc getLoc(ref Loc start, ref Loc end)
    {
        return Loc(start.filename, start.dir, start.start, end.end);
    }

public:
    this(Token[] tokens = [], DiagnosticError error, TypeRegistry registry, string pathRoot)
    {
        this.error = error;
        this.tokens = tokens;
        this.registry = registry;
        this.pathRoot = pathRoot;
    }

    Program parseProgram()
    {
        Program program = new Program([]);
        try
        {
            while (!this.isAtEnd())
            {
                Node node = this.parse();
                // Só adiciona se não for null (semicolons redundantes)
                if (node !is null)
                    program.body ~= node;
            }
            if (this.tokens.length == 0)
                return program;
        }
        catch (Exception e)
            throw e; // propaga
        return program;
    }
}
