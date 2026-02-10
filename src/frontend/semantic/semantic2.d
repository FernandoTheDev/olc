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
        else if (auto decl = cast(UnionDecl) node)
            resolveUnionDecl(decl);
    }

    void resolveEnumDecl(EnumDecl decl)
    {
        EnumSymbol sym = ctx.lookupEnum(decl.name);
        if (sym is null)
        {
            reportError(format("Enum '%s' not found in the context.", decl.name), decl.loc);
            return;
        }
        Type resolved = resolver.resolve(decl.type);
        sym.enumType.baseType = resolved;
        decl.resolvedType = sym.enumType;
        // writeln("ENUM: ", decl.name);
        // writeln("NOVO TIPO: ", sym.enumType.baseType.toStr());
        // writeln("ENUM TIPO: ", resolved.toStr());
        registry.updateType(decl.name, decl.resolvedType);
    }

    void resolveUnionDecl(UnionDecl decl)
    {
        UnionSymbol sym = ctx.lookupUnion(decl.name);
        if (sym is null)
        {
            reportError(format("Union '%s' not found in the context.", decl.name), decl.loc);
            return;
        }

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

        sym.unionType.fields = decl.fields;
        sym.unionType.rebuildFieldIndexMap();
        decl.resolvedType = sym.unionType;

        UnionType existingType = cast(UnionType) registry.lookupType(decl.name);
        existingType.fields = sym.unionType.fields;
        decl.mangledName = mangleName(decl);
        existingType.mangledName = decl.mangledName;
        
        existingType.rebuildFieldIndexMap();
        registry.updateType(decl.name, existingType);
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
        // 1. Casos especiais que não devem sofrer mangling
        if (func.isExtern || func.name == "main" || func.noMangle) 
            return func.name;

        // 2. Início do mangling
        string mangled = "_O";

        // 3. Nome Qualificado (Módulo + Nome da Função)
        // Ex: modulo math, func sum -> 4math3sum
        mangled ~= mangleQualifiedName(func.loc, func.name);

        // 4. Argumentos (Signature)
        // Isso permite Function Overloading
        if (func.args.length == 0)
        {
            mangled ~= "v"; // void args (sem argumentos)
        }
        else
        {
            foreach (arg; func.args)
            {
                if (arg.variadic)
                    mangled ~= "z"; // 'z' para varargs (ellipsis)
                else
                    mangled ~= mangleType(arg.resolvedType);
            }
        }

        return mangled;
    }

    string mangleName(StructDecl decl)
    {
        if (decl.noMangle) return decl.name;
        // Structs não incluem campos no nome, apenas sua localização
        return "_O" ~ mangleQualifiedName(decl.loc, decl.name);
    }

    string mangleName(UnionDecl decl)
    {
        if (decl.noMangle) return decl.name;
        return "_O" ~ mangleQualifiedName(decl.loc, decl.name);
    }

    // Gera o nome qualificado: N + len + nome + ... + E
    // Tenta extrair 'pacote.modulo' do caminho do arquivo
    string mangleQualifiedName(Loc loc, string name)
    {
        import std.path : baseName, stripExtension;
        import std.array : split;
        
        string moduleName = loc.filename.baseName.stripExtension;
        
        // Se você tiver um sistema de módulos real no 'Context', use-o aqui.
        // Por enquanto, vou usar o nome do arquivo como módulo.
        
        string res = "N"; // Nested name start
        
        // Codifica o nome do módulo
        res ~= encodeLength(moduleName.length) ~ moduleName;
        
        // Codifica o nome do símbolo
        res ~= encodeLength(name.length) ~ name;
        
        res ~= "E"; // End of nested name
        return res;
    }

    // Codifica o tipo recursivamente baseando-se na AST de tipos, não em strings
    string mangleType(Type type)
    {
        if (type is null) return "v"; // void/error

        // 1. Primitivos
        if (auto prim = cast(PrimitiveType) type)
        {
            switch(prim.baseType)
            {
                case BaseType.Void:   return "v";
                case BaseType.Bool:   return "b";
                case BaseType.Char:   return "c";
                case BaseType.Byte:   return "a"; // signed char (a = apple/int8)
                case BaseType.Ubyte:  return "h"; // unsigned char (h = hex/uint8)
                case BaseType.Short:  return "s";
                case BaseType.Ushort: return "t";
                case BaseType.Int:    return "i";
                case BaseType.Uint:   return "j";
                case BaseType.Long:   return "l"; // 64-bit
                case BaseType.Ulong:  return "m"; // unsigned long
                case BaseType.Float:  return "f";
                case BaseType.Double: return "d";
                case BaseType.String: return "Ps"; // Pointer to char/string struct
                case BaseType.Any:    return "z";  // Ellipsis/Any
                default: return "i";
            }
        }

        // 2. Ponteiros: P + tipo
        if (auto ptr = cast(PointerType) type)
        {
            return "P" ~ mangleType(ptr.pointeeType);
        }

        // 3. Arrays: A + tamanho + tipo (ex: A5i -> int[5])
        if (auto arr = cast(ArrayType) type)
        {
            if (arr.length > 0)
                return "A" ~ encodeLength(arr.length) ~ mangleType(arr.elementType);
            else
                return "A_" ~ mangleType(arr.elementType); // Slice ou array dinâmico
        }

        // 4. Tipos de Usuário (Structs/Enums/Unions)
        // Codifica como: S + len + nome (Simplificado)
        // O ideal seria usar o nome qualificado completo aqui também
        if (auto st = cast(StructType) type)
        {
            // Para structs, usamos o nome mangled se já existir, ou geramos um simples
            // Se for um tipo complexo, geralmente usamos 'S' + tamanho + nome
            string sName = st.name;
            return "S" ~ encodeLength(sName.length) ~ sName;
        }

        if (auto ut = cast(UnionType) type)
        {
            return "U" ~ encodeLength(ut.name.length) ~ ut.name;
        }
        
        if (auto et = cast(EnumType) type)
        {
            // Enums geralmente são tratados como o tipo base (int) na ABI C, 
            // mas para segurança de tipo na sua linguagem, vamos manglar pelo nome.
            return "E" ~ encodeLength(et.name.length) ~ et.name;
        }

        // 5. Funções (Ponteiro de função): F + return + args + E
        if (auto fn = cast(FunctionType) type)
        {
            string s = "F"; 
            s ~= mangleType(fn.returnType);
            foreach(arg; fn.paramTypes)
                s ~= mangleType(arg);
            s ~= "E";
            return s;
        }

        return "v"; // Fallback void
    }

    string encodeLength(size_t len)
    {
        import std.conv : to;
        return to!string(len);
    }
}
