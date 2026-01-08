module frontend.semantic.semantic2;

import frontend;
import common.reporter;

class Semantic2
{
    Context ctx;
    TypeResolver resolver;
    DiagnosticError error;
    TypeRegistry registry;
    TypeChecker checker;

    this(Context ctx, DiagnosticError error, TypeRegistry registry, TypeResolver res = null, TypeChecker checker = null)
    {
        this.ctx = ctx;
        this.error = error;
        this.checker = checker;
        if (res is null)
            this.resolver = new TypeResolver(ctx, error, registry);
        else
            this.resolver = res;
        this.registry = registry;
    }

    pragma(inline, true)
    void reportError(string message, Loc loc, Suggestion[] suggestions = null)
    {
        error.addError(Diagnostic(message, loc, suggestions));
    }

    void analyze(Program program)
    {
        foreach (node; program.body)
            resolveDeclaration(node);

        Node[] body;
        if (resolver.structs.length > 0)
            body ~= resolver.structs;

        if (checker !is null && checker.structs.length > 0)
            body ~= checker.structs;

        body ~= program.body;
        program.body = body;
    }

    void resolveDeclaration(Node node)
    {
        if (auto varDecl = cast(VarDecl) node)
            resolveVarDecl(varDecl);
        else if (auto funcDecl = cast(FuncDecl) node)
            resolveFunctionDecl(funcDecl);
        else if (auto decl = cast(StructDecl) node)
            resolveStructDecl(decl);
        else if (auto decl = cast(EnumDecl) node)
            resolveEnumDecl(decl);
    }

    void resolveEnumDecl(EnumDecl decl)
    {
        EnumSymbol sym = ctx.lookupEnum(decl.name);
        if (sym is null)
        {
            reportError(format("Enum '%s' not found in the context.", decl.name), decl.loc);
            return;
        }
        decl.resolvedType = sym.enumType;
    }

    void resolveStructDecl(StructDecl decl)
    {
        StructSymbol structSym = ctx.lookupStruct(decl.name);
        if (structSym is null)
        {
            reportError(format("Struct '%s' not found in the context.", decl.name), decl.loc);
            return;
        }

        // Resolve tipos dos campos
        foreach (ref field; decl.fields)
        {
            field.resolvedType = resolver.resolve(field.type);
            if (field.resolvedType is null)
            {
                reportError(format("Could not resolve type '%s' for field '%s'.", field.type.toStr(), field.name), 
                    field.loc);
                field.resolvedType = new PrimitiveType(BaseType.Any);
            }
        }

        StructType existingType = cast(StructType) registry.lookupType(decl.name);
        decl.mangledName = mangleName(decl);
        existingType.mangledName = decl.mangledName;

        // Atualiza campos no structType
        structSym.structType.fields = decl.fields;
        // NÃO limpa! Cria um novo array associativo
        StructMethod[][string] newMethods;
        // Processa cada método e valida overloads
        foreach (methodName, overloads; decl.methods)
        {
            StructMethod[] validatedOverloads;
            foreach (ref method; overloads)
            {
                foreach (ref param; method.funcDecl.args)
                {
                    param.resolvedType = resolver.resolve(param.type);
                    if (param.resolvedType is null)
                    {
                        reportError(format("Could not resolve type '%s' for parameter '%s' in method '%s'.", 
                            param.type.toStr(), param.name, method.funcDecl.name), param.loc);
                        param.resolvedType = new PrimitiveType(BaseType.Any);
                    }
                }

                // Resolve tipo de retorno
                if (method.isConstructor)
                    method.funcDecl.resolvedType = structSym.structType;
                else
                {
                    method.funcDecl.resolvedType = resolver.resolve(method.funcDecl.type);
                    if (method.funcDecl.resolvedType is null)
                    {
                        reportError(format("Could not resolve return type '%s' for method '%s'.", 
                            method.funcDecl.type.toStr(), method.funcDecl.name), method.funcDecl.loc);
                        method.funcDecl.resolvedType = new PrimitiveType(BaseType.Void);
                    }
                    else
                        method.funcDecl.mangledName = mangleName(method.funcDecl);
                }

                // Verifica duplicação dentro dos overloads
                bool isDuplicate = false;
                foreach (existing; validatedOverloads)
                {
                    if (structSym.structType.isSameMethodSignature(method, existing))
                    {
                        reportError(format("Ambiguous redeclaration of method '%s'. " ~
                            "A method with the same signature already exists.", method.funcDecl.name), 
                            method.funcDecl.loc);
                        isDuplicate = true;
                        break;
                    }

                    // Validação @nomangle para métodos
                    if (method.funcDecl.noMangle && existing.funcDecl.noMangle)
                    {
                        reportError(format("Multiple @nomangle methods with name '%s'.", method.funcDecl.name), 
                            method.funcDecl.loc);
                        isDuplicate = true;
                        break;
                    }
                }

                if (!isDuplicate)
                    validatedOverloads ~= method;
            }

            // Adiciona todos os overloads validados ao NOVO array associativo
            if (validatedOverloads.length > 0)
                newMethods[methodName] = validatedOverloads;
        }

        // Agora atribui o novo array associativo
        structSym.structType.methods = newMethods;

        // Reconstrói o mapa de índices de campos
        structSym.structType.rebuildFieldIndexMap();
        decl.resolvedType = structSym.structType;

        existingType.fields = structSym.structType.fields;
        existingType.methods = structSym.structType.methods;
        existingType.rebuildFieldIndexMap();
        registry.updateType(decl.name, existingType);

        structSym.declaration = decl;
    }

    void resolveVarDecl(VarDecl decl)
    {
        VarSymbol sym = ctx.lookupVariable(decl.id);

        if (sym !is null)
            sym.value = decl.value.get!Node;
        
        // Se tem anotação de tipo, resolve
        if (decl.type !is null)
            decl.resolvedType = resolver.resolve(decl.type);
        // Se não tem anotação mas tem inicializador, deixa null para inferir depois
        else if (decl.value.get!Node !is null)
            decl.resolvedType = null; // será inferido no Semantic3
        // Se não tem nem tipo nem inicializador, erro
        else
        {
            reportError(format("The variable '%s' needs a type or initializer.", decl.id), decl.loc);
            decl.resolvedType = new PrimitiveType(BaseType.Any);
            return;
        }

        if (sym !is null && decl.resolvedType !is null)
            sym.type = decl.resolvedType;
    }

    void resolveFunctionDecl(FuncDecl decl)
    {
        Type[] paramTypes;
        foreach (i, ref param; decl.args)
        {
            Type paramType = null;
            paramType = resolver.resolve(param.type);
            paramTypes ~= paramType;
            param.resolvedType = paramType;
        }

        if (decl.resolvedType is null)
            decl.resolvedType = resolver.resolve(decl.type);

        auto sym = new FunctionSymbol(
            decl.name,
            paramTypes,
            decl.resolvedType,
            decl,
            decl.loc
        );

        sym.isExternal = decl.isExtern;
        ctx.addFunction(sym);
        decl.mangledName = mangleName(decl);
    }

    string generateID(string input)
    {
        ulong hash = 14_695_981_039_346_656_037UL;
        foreach (char c; input) {
            hash ^= c;
            hash *= 1_099_511_628_211UL;
        }
        return format("%08X", hash & 0xFFFFFFFF);
    }

    string mangleName(FuncDecl func)
    {
        if (func.isExtern || func.name == "main" || func.noMangle) 
            return func.name;

        string modulePath = func.loc.dir ~ func.loc.filename;
        string mangled = "_OLC";
        mangled ~= "_" ~ func.resolvedType.toStr(); // Ex: _int
        mangled ~= "_" ~ func.name; // Ex: _soma
        
        foreach (arg; func.args)
        {
            if (arg.variadic)
                mangled ~= "_variadic";
            else
                mangled ~= "_" ~ arg.resolvedType.toStr(); // Ex: _int_int
        }

        string uniqueID = generateID(modulePath);
        mangled ~= "_" ~ uniqueID;

        return mangled;
    }

    string mangleName(StructDecl decl)
    {
        if (decl.noMangle) 
            return decl.name;

        string modulePath = decl.loc.dir ~ decl.loc.filename;
        string mangled = "_OLC";
        mangled ~= "_" ~ decl.name;
        
        foreach (StructField field; decl.fields)
                mangled ~= "_" ~ field.resolvedType.toStr();

        string uniqueID = generateID(modulePath);
        mangled ~= "_" ~ uniqueID;

        return mangled;
    }
}
