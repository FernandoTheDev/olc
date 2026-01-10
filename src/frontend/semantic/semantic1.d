module frontend.semantic.semantic1;

import frontend;
import std.file : exists, readText;
import std.path : buildPath, absolutePath, dirName, extension;
import std.algorithm : canFind;
import common.reporter, env;

struct ModuleCacheEntry {
    Context ctx;
    Program program;
}

class Semantic1
{
    Context ctx;
    DiagnosticError error;
    Node[] importedASTs;
    static ModuleCacheEntry[string] modulesCache;
    TypeRegistry registry;
    TypeChecker checker;
    string pathRoot;

    this(Context ctx, TypeRegistry registry, DiagnosticError error, string pathRoot, TypeChecker checker)
    {
        this.ctx = ctx;
        this.error = error;
        this.registry = registry;
        this.pathRoot = pathRoot;
        this.checker = checker;
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

    void analyze(Program program)
    {
        foreach (node; program.body)
            collectDeclaration(node);

        if (importedASTs.length > 0)
        {
            program.body = importedASTs ~ program.body;
            importedASTs = []; 
        }
    }

    void collectDeclaration(Node node)
    {
        if (auto varDecl = cast(VarDecl) node)
            collectVarDecl(varDecl);
        else if (auto decl = cast(StructDecl) node)
            collectStructDecl(decl);
        else if (auto mod = cast(ImportStmt) node)
            collectImportStmt(mod);
        else if (auto enm = cast(EnumDecl) node)
            collectEnumDecl(enm);
        else if (auto un = cast(UnionDecl) node)
            collectUnionDecl(un);
    }

    void collectImportStmt(ImportStmt node)
    {
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
                return;
            }
        }

        string fullPath = absolutePath(resolvedPath);
        Context importedCtx;
        Program importedProgram;

        if (fullPath in modulesCache)
        {
            importedCtx = modulesCache[fullPath].ctx;
            importedProgram = modulesCache[fullPath].program;

            reportWarning("The module has been imported more than once; this time it will be imported from the cache.", 
                node.loc);
        }
        else
        {
            importedCtx = new Context(error);
            try 
            {
                string src = readText(fullPath);
                Lexer lexer = new Lexer(fullPath, src, dirName(fullPath), this.error);
                Token[] tokens = lexer.tokenize();
                Parser parser = new Parser(tokens, this.error, registry, pathRoot); // Assumindo construtor compatível
                importedProgram = parser.parseProgram();
                TypeChecker importedChecker = new TypeChecker(importedCtx, error, registry);
                
                new Semantic1(importedCtx, registry, error, pathRoot, importedChecker).analyze(importedProgram);
                new Semantic2(importedCtx, error, registry, null, importedChecker).analyze(importedProgram);

                modulesCache[fullPath] = ModuleCacheEntry(importedCtx, importedProgram);
            }
            catch (Exception e)
            {
                reportError("Fatal error during import: " ~ e.msg, node.loc);
                return;
            }
        }

        bool isSelective = node.symbols.length > 0;
        foreach (importedNode; importedProgram.body)
        {
            if (importedNode.kind == NodeKind.ImportStmt) continue;

            string nodeName = getNodeName(importedNode);
            if (nodeName == "")
                continue;

            // Verifica se o símbolo é público no contexto original
            Symbol originalSym = importedCtx.lookup(nodeName);
            if (originalSym is null || !originalSym.isPublic) continue;
            bool shouldImport = false;

            if (isSelective)
            {
                if (node.symbols.canFind(nodeName))
                    shouldImport = true;
            }
            else
                shouldImport = true;

            if (shouldImport)
            {
                if (!ctx.importSymbol(originalSym, node.aliasname))
                {
                    // se falhou e foi seletivo, apenas ignore o erro
                    if (isSelective)
                    {
                        reportError(format("The symbol '%s' already exists in the context..", originalSym.name), 
                            node.loc);
                        continue;
                    }
                    continue;
                }
                // importedNode.print();
                importedASTs ~= importedNode;
            }
        }
        
        if (isSelective)
        {
            foreach (reqSym; node.symbols)
            {
                Symbol sym = importedCtx.lookupLocal(reqSym);
                if (sym is null)
                    reportError("The symbol '" ~ reqSym ~ "' does not exist in " ~ filename, node.loc);
                else if (!sym.isPublic)
                    reportError("The symbol '" ~ reqSym ~ "' is private.", node.loc);
            }
        }
    }

    string getNodeName(Node node)
    {
        if (auto fd = cast(FuncDecl) node) return fd.name;
        if (auto sd = cast(StructDecl) node) return sd.name;
        if (auto sd = cast(EnumDecl) node) return sd.name;
        if (auto sd = cast(UnionDecl) node) return sd.name;
        if (auto vd = cast(VarDecl) node) return vd.id;
        return "";
    }

    void collectUnionDecl(UnionDecl decl)
    {
        if (ctx.isDefined(decl.name))
        {
            reportError(format("Union redefinition '%s'", decl.name), decl.loc);
            return;
        }
        UnionType realType = new UnionType(decl.name, decl.fields);
        UnionSymbol symbol = new UnionSymbol(decl.name, realType, decl, decl.loc);
        ctx.addUnion(symbol);
    }

    void collectEnumDecl(EnumDecl decl)
    {
        if (ctx.isDefined(decl.name))
        {
            reportError(format("Enum redefinition '%s'", decl.name), decl.loc);
            return;
        }
        EnumType realType = new EnumType(decl.name, decl.members);
        EnumSymbol symbol = new EnumSymbol(decl.name, realType, decl, decl.loc);
        ctx.addEnum(symbol);
    }

    void collectStructDecl(StructDecl decl)
    {
        if (ctx.isDefined(decl.name))
        {
            reportError(format("Struct redefinition '%s'", decl.name), decl.loc);
            return;
        }
        StructType realType = new StructType(decl.name, decl.fields, decl.methods);
        StructSymbol symbol = new StructSymbol(decl.name, realType, decl, decl.loc);
        if (!registry.typeExists(decl.name))
            registry.registerType(decl.name, realType);
        decl.resolvedType = realType;
        ctx.addStruct(symbol);
    }

    void collectVarDecl(VarDecl decl)
    {
        if (ctx.isDefined(decl.id))
        {
            reportError(format("Redefining '%s'", decl.id), decl.loc);
            return;
        }

        Type tempType = null;
        if (!ctx.addVariable(decl.id, tempType, decl.isConst ? decl.isConst : decl.type.refConst, decl.loc))
            reportError(format("Error adding variable '%s'", decl.id), decl.loc);
    }
}
