module frontend.semantic.context;

import frontend;
import common.reporter, std.format : format;
import std.stdio : writeln, writefln;
import std.array : replicate;

enum SymbolKind
{
    Variable,
    Function,
    Struct,
    Enum,
    Union
}

abstract class Symbol
{
    string name;
    SymbolKind kind;
    Loc loc;
    Type type;
    bool isPublic = true; // por padrão, símbolos são públicos
    bool isExternal, isTemplate;

    this(string name, SymbolKind kind, Type type, Loc loc)
    {
        this.name = name;
        this.kind = kind;
        this.type = type;
        this.loc = loc;
    }

    // Clona o símbolo (útil para imports)
    abstract Symbol clone();
}

class VarSymbol : Symbol
{
    bool isConst;
    bool isGlobal;
    Node value = null;

    this(string name, Type type, bool isConst, bool isGlobal, Loc loc)
    {
        super(name, SymbolKind.Variable, type, loc);
        this.isConst = isConst;
        this.isGlobal = isGlobal;
    }

    override Symbol clone()
    {
        auto cloned = new VarSymbol(name, type, isConst, isGlobal, loc);
        cloned.isPublic = this.isPublic;
        cloned.isExternal = true;
        return cloned;
    }
}

class FunctionSymbol : Symbol
{
    Type[] paramTypes;
    Type returnType;
    FuncDecl declaration;

    this(string name, Type[] paramTypes, Type returnType,
        FuncDecl declaration, Loc loc)
    {
        super(name, SymbolKind.Function, returnType, loc);
        this.paramTypes = paramTypes;
        this.returnType = returnType;
        this.declaration = declaration;
    }

    override Symbol clone()
    {
        auto cloned = new FunctionSymbol(name, paramTypes.dup, returnType, 
                                        declaration, loc);
        cloned.isPublic = this.isPublic;
        cloned.isExternal = true;
        return cloned;
    }
}

class StructSymbol : Symbol
{
    StructDecl declaration;
    StructType structType;

    this(string name, StructType structType, StructDecl declaration, Loc loc)
    {
        super(name, SymbolKind.Struct, structType, loc);
        this.structType = structType;
        this.declaration = declaration;
    }

    override Symbol clone()
    {
        auto cloned = new StructSymbol(name, structType, declaration, loc);
        cloned.isPublic = this.isPublic;
        cloned.isExternal = true;
        return cloned;
    }

    bool hasField(string fieldName)
    {
        return structType.hasField(fieldName);
    }

    Type getFieldType(string fieldName)
    {
        return structType.getFieldType(fieldName);
    }

    bool hasMethod(string methodName)
    {
        return structType.hasMethod(methodName);
    }

    StructMethod* getConstructor()
    {
        return structType.getConstructor();
    }

    StructMethod* getMethod(string methodName)
    {
        return structType.getMethod(methodName);
    }
}

class EnumSymbol : Symbol
{
    EnumDecl declaration;
    EnumType enumType;

    this(string name, EnumType enumType, EnumDecl declaration, Loc loc)
    {
        super(name, SymbolKind.Enum, enumType, loc);
        this.enumType = enumType;
        this.declaration = declaration;
    }

    override Symbol clone()
    {
        auto cloned = new EnumSymbol(name, enumType, declaration, loc);
        cloned.isPublic = this.isPublic;
        cloned.isExternal = true;
        return cloned;
    }

    bool hasMember(string fieldName)
    {
        return enumType.hasMember(fieldName);
    }
}

class UnionSymbol : Symbol
{
    UnionDecl declaration;
    UnionType unionType;

    this(string name, UnionType unionType, UnionDecl declaration, Loc loc)
    {
        super(name, SymbolKind.Union, unionType, loc);
        this.unionType = unionType;
        this.declaration = declaration;
    }

    override Symbol clone()
    {
        auto cloned = new UnionSymbol(name, unionType, declaration, loc);
        cloned.isPublic = this.isPublic;
        cloned.isExternal = true;
        return cloned;
    }

    bool hasField(string fieldName)
    {
        return unionType.hasField(fieldName);
    }

    Type getFieldType(string fieldName)
    {
        return unionType.getFieldType(fieldName);
    }
}

class Scope
{
    Scope parent;
    Symbol[string] symbols;
    Scope[string] namespaces;
    string name;
    FunctionSymbol[][string] functionOverloads;
    DiagnosticError error;

    this(Scope parent, DiagnosticError error, string name = "")
    {
        this.parent = parent;
        this.error = error;
        this.name = name;
    }

    bool addToNamespace(string namespaceName, Symbol symbol)
    {
        if (namespaceName !in namespaces)
            namespaces[namespaceName] = new Scope(null, error, namespaceName);
        
        return namespaces[namespaceName].define(symbol);
    }

    Scope getNamespace(string namespaceName)
    {
        return namespaces.get(namespaceName, null);
    }

    bool define(Symbol symbol)
    {
        if (symbol.name in symbols)
            return false;

        symbols[symbol.name] = symbol;
        return true;
    }

    Symbol lookupLocal(string name)
    {
        return symbols.get(name, null);
    }

    Symbol lookupInNamespace(string namespaceName, string symbolName)
    {
        auto ns = getNamespace(namespaceName);
        if (ns is null)
            return null;
        return ns.lookupLocal(symbolName);
    }

    Symbol lookup(string name)
    {
        // Verifica se é uma referência qualificada (namespace.symbol)
        import std.algorithm : canFind;
        import std.string : indexOf;
        
        auto colonPos = name.indexOf("::");
        if (colonPos != -1)
        {
            string nsName = name[0 .. colonPos];
            string symName = name[colonPos + 2 .. $];
            return lookupInNamespace(nsName, symName);
        }

        // Busca normal
        Symbol sym = lookupLocal(name);
        if (sym !is null)
            return sym;

        if (parent !is null)
            return parent.lookup(name);

        return null;
    }

    bool isDefined(string name)
    {
        return lookup(name) !is null;
    }

    // Retorna todos os símbolos públicos (útil para imports)
    Symbol[] getPublicSymbols()
    {
        Symbol[] result;
        foreach (sym; symbols)
            result ~= sym;
        return result;
    }

    FunctionSymbol[] resolveFunctions(string name)
    {
        FunctionSymbol[] candidates;
        
        // Pega as locais
        if (name in functionOverloads)
            candidates ~= functionOverloads[name];
        
        // Pega as do pai (shadowing ou merge? Normalmente merge em overloads)
        if (parent)
            candidates ~= parent.resolveFunctions(name);
            
        return candidates;
    }

    bool defineFunction(FunctionSymbol newSym)
    {
        if (newSym.name in functionOverloads)
        {
            auto existingOverloads = functionOverloads[newSym.name];

            foreach (FunctionSymbol existing; existingOverloads)
            {
                if (isSameSignature(newSym, existing))
                {
                    reportError("Ambiguous redeclaration of the function '" ~ newSym.name ~ "'. " ~
                        "A function with the same argument types has already been defined in:" ~ 
                        existing.loc.toStr(), newSym.loc);
                    return false; 
                }
                
                // Validação extern(C) / noMangle
                // Se sua AST usa isExternC ou noMangle, ajuste aqui
                if (newSym.declaration.noMangle && existing.declaration.noMangle)
                {
                    reportError("Conflict: Multiple '@nomangle' functions with the name '" ~ newSym.name ~ 
                        "' are not allowed.", newSym.loc);
                    return false;
                }
            }

            functionOverloads[newSym.name] ~= newSym;
        }
        else
        {
            functionOverloads[newSym.name] = [newSym];
            // Também adiciona no mapa genérico para lookups simples (pega o primeiro)
            symbols[newSym.name] = newSym; 
        }
        return true;
    }

    // Compara assinaturas usando os tipos armazenados no SÍMBOLO
    private bool isSameSignature(FunctionSymbol a, FunctionSymbol b)
    {
        if (a.paramTypes.length != b.paramTypes.length) return false;

        foreach (i, typeA; a.paramTypes)
        {
            Type typeB = b.paramTypes[i];
            if (typeA.toStr() != typeB.toStr()) return false;
        }

        return true;
    }

    private void reportError(string msg, Loc loc)
    {
        error.addError(Diagnostic(msg, loc));    
    }
}

class Context
{
    Scope currentScope;
    Scope globalScope;
    FunctionSymbol currentFunction;
    StructSymbol currentStruct;
    int loopDepth;
    DiagnosticError error;

    this(DiagnosticError error)
    {
        this.error = error;
        this.globalScope = new Scope(null, error, "global");
        this.currentScope = globalScope;
        this.currentFunction = null;
        this.currentStruct = null;
        this.loopDepth = 0;
    }

    void enterScope(string name = "")
    {
        currentScope = new Scope(currentScope, error, name);
    }

    void exitScope()
    {
        if (currentScope.parent is null)
            throw new Exception("Tentativa de sair do escopo global");
        currentScope = currentScope.parent;
    }

    void enterFunction(FunctionSymbol func)
    {
        currentFunction = func;
        enterScope(format("func:%s", func.name));
    }

    void exitFunction()
    {
        currentFunction = null;
        exitScope();
    }

    void enterStruct(StructSymbol structSym)
    {
        currentStruct = structSym;
        enterScope(format("struct:%s", structSym.name));
    }

    void exitStruct()
    {
        currentStruct = null;
        exitScope();
    }

    void enterLoop()
    {
        loopDepth++;
        enterScope("loop");
    }

    void exitLoop()
    {
        loopDepth--;
        exitScope();
    }

    bool isInLoop()
    {
        return loopDepth > 0;
    }

    bool isInFunction()
    {
        return currentFunction !is null;
    }

    bool isInStruct()
    {
        return currentStruct !is null;
    }

    bool addSymbol(Symbol symbol)
    {
        return currentScope.define(symbol);
    }

    bool addVariable(string name, Type type, bool isConst, Loc loc, Node value = null)
    {
        auto sym = new VarSymbol(name, type, isConst,
            currentScope == globalScope, loc);
        sym.value = value;
        return addSymbol(sym);
    }

    bool addFunction(FunctionSymbol func)
    {
        return globalScope.defineFunction(func);
    }

    bool addStruct(StructSymbol structSym)
    {
        return globalScope.define(structSym);
    }

    bool addEnum(EnumSymbol sym)
    {
        return globalScope.define(sym);
    }

    bool addUnion(UnionSymbol sym)
    {
        return globalScope.define(sym);
    }

    // Importa um símbolo de outro contexto
    bool importSymbol(Symbol symbol, string aliasName = "")
    {
        Symbol cloned = symbol.clone();
        if (aliasName != "")
            return globalScope.addToNamespace(aliasName, cloned);
        else
        {
            if (globalScope.lookupLocal(cloned.name) !is null)
                return false; // Já existe
            return globalScope.define(cloned);
        }
    }

    // Importa múltiplos símbolos
    void importSymbols(Symbol[] symbols, string aliasName = "")
    {
        foreach (sym; symbols)
            importSymbol(sym, aliasName);
    }

    Symbol lookup(string name)
    {
        return currentScope.lookup(name);
    }

    Symbol lookupLocal(string name)
    {
        return currentScope.lookupLocal(name);
    }

    VarSymbol lookupVariable(string name)
    {
        Symbol sym = lookup(name);
        return cast(VarSymbol) sym;
    }

    FunctionSymbol lookupFunction(string name)
    {
        Symbol sym = globalScope.lookup(name);
        return cast(FunctionSymbol) sym;
    }

    StructSymbol lookupStruct(string name)
    {
        Symbol sym = globalScope.lookup(name);
        return cast(StructSymbol) sym;
    }

    UnionSymbol lookupUnion(string name)
    {
        Symbol sym = globalScope.lookup(name);
        return cast(UnionSymbol) sym;
    }

    EnumSymbol lookupEnum(string name)
    {
        Symbol sym = globalScope.lookup(name);
        return cast(EnumSymbol) sym;
    }

    bool canAssign(string varName)
    {
        VarSymbol var = lookupVariable(varName);
        if (var is null)
            return false;
        return !var.isConst;
    }

    bool isDefined(string name)
    {
        return currentScope.isDefined(name);
    }

    Symbol[] getPublicSymbols()
    {
        return globalScope.getPublicSymbols();
    }

    FunctionSymbol findFunction(string name, Type[] argTypes, Type returnType = null)
    {
        auto candidates = currentScope.resolveFunctions(name);
        if (candidates.length == 0) return null;

        FunctionSymbol bestMatch = null;
        int bestScore = 999_999;

        foreach (FunctionSymbol cand; candidates)
        {    
            bool hasVariadic = false;
            bool isOrnVarArgs = cand.declaration !is null && cand.declaration.isOrnVarArgs;
            size_t minParams = cand.paramTypes.length;
            size_t paramsToSkip = 0;

            if (cand.paramTypes.length > 0 && cand.paramTypes[$-1] is null)
            {
                hasVariadic = true;
                minParams = cast(int) cand.paramTypes.length - 1;

                if (isOrnVarArgs && minParams > 0)
                {
                    paramsToSkip = 1; // OrnVarArgs não deve ser passado pelo usuário
                    minParams--; // Decrementa para não contar na validação
                }
            }

            if (hasVariadic)
            {
                // Varargs: aceita >= minParams argumentos
                if (argTypes.length < minParams) continue;
            }
            else
                // Função normal: número exato (descontando sintéticos)
                if (cand.paramTypes.length - paramsToSkip != argTypes.length) continue;

            // Verifica tipo de retorno se especificado
            if (returnType !is null && cand.returnType !is null)
                if (!cand.returnType.isCompatibleWith(returnType)) continue;

            int currentScore = 0;
            bool compatible = true;

            foreach (i; 0 .. minParams)
            {
                Type expected = cand.paramTypes[i];
                Type received = argTypes[i];

                if (received is null || expected is null)
                {
                    compatible = false;
                    break;
                }

                if (expected.toStr() == received.toStr())
                    currentScore += 0; // Match exato
                else if (expected.isCompatibleWith(received))
                    currentScore += 1; // Match com conversão implícita
                else
                {
                    compatible = false;
                    break;
                }
            }

            // Se compatível e melhor que o anterior, seleciona
            if (compatible && currentScore < bestScore)
            {
                bestScore = currentScore;
                bestMatch = cand;
            }
        }

        return bestMatch;
    }

    void dump()
    {
        writeln("=== CONTEXT DUMP ===");
        dumpScope(globalScope, 0);
    }

    void dumpScope(Scope scope_, int indent)
    {
        string prefix = " ".replicate(indent * 2);
        writefln("%sScope: %s", prefix, scope_.name);
        Type type;

        foreach (name, sym; scope_.symbols)
        {
            if (sym.kind == SymbolKind.Function)
                type = (cast(FunctionSymbol) sym).returnType;
            else if (sym.kind == SymbolKind.Struct)
                type = (cast(StructSymbol) sym).structType;
            else
                type = sym.type;
            
            string extMarker = sym.isExternal ? " [external]" : "";
            string pubMarker = sym.isPublic ? " [public]" : " [private]";
            
            writefln("%s  - %s: %s (%s)%s%s",
                prefix, name, sym.kind,
                type !is null ? type.toStr() : "no-type",
                extMarker, pubMarker);
            
            if (sym.kind == SymbolKind.Struct)
            {
                auto structSym = cast(StructSymbol) sym;
                if (structSym.structType.fields.length > 0)
                {
                    writefln("%s    Fields:", prefix);
                    foreach (field; structSym.structType.fields)
                    {
                        writefln("%s      - %s: %s", prefix, field.name, 
                                field.resolvedType !is null ? field.resolvedType.toStr() : "unresolved");
                    }
                }
                // if (structSym.structType.methods.length > 0)
                // {
                //     writefln("%s    Methods:", prefix);
                //     foreach (method; structSym.structType.methods)
                //     {
                //         string methodType = method.isConstructor ? "constructor" : "method";
                //         writefln("%s      - %s (%s)", prefix, method.funcDecl.name, methodType);
                //     }
                // }
            }
        }

        // Dump namespaces
        if (scope_.namespaces.length > 0)
        {
            writefln("%sNamespaces:", prefix);
            foreach (nsName, ns; scope_.namespaces)
            {
                writefln("%s  Namespace: %s", prefix, nsName);
                dumpScope(ns, indent + 2);
            }
        }
    }
}
