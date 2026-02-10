module frontend.semantic.type_resolution;

import std.algorithm;
import frontend;
import common.reporter;

class TypeResolver
{
    Context ctx;
    DiagnosticError error;
    TypeRegistry registry;
    StructDecl[] structs;

    this(Context ctx, DiagnosticError error, TypeRegistry registry)
    {
        this.ctx = ctx;
        this.error = error;
        this.registry = registry;
    }

    Type resolve(TypeExpr typeExpr)
    {
        if (typeExpr is null)
            return null;

        return this.resolveInternal(typeExpr);
    }

private:
    pragma(inline, true)
    void reportError(string message, Loc loc, Suggestion[] suggestions = null)
    {
        error.addError(Diagnostic(message, loc, suggestions));
    }

    Type resolveInternal(TypeExpr typeExpr)
    {
        if (auto named = cast(NamedTypeExpr) typeExpr)
            return resolveNamed(named);

        if (auto arr = cast(ArrayTypeExpr) typeExpr)
            return resolveArray(arr);

        if (auto ptr = cast(PointerTypeExpr) typeExpr)
            return resolvePointer(ptr);

        if (auto fn = cast(FunctionTypeExpr) typeExpr)
            return resolveFuncType(fn);

        reportError("Unknown type in the resolution.", typeExpr.loc);
        return new PrimitiveType(BaseType.Any);
    }

    Type resolveFuncType(FunctionTypeExpr fn)
    {
        Type[] types = fn.paramTypes.map!(t => resolve(t)).array;
        return new FunctionType(types, resolve(fn.returnType));
    }

    Type resolveNamed(NamedTypeExpr named)
    {
        string name = named.name;
        if (!registry.typeExists(name))
        {
            writeln(registry.listAllTypes()); // debug
            reportError(format("The type '%s' does not exist.", name), named.loc);
            return new PrimitiveType(BaseType.Any);
        }
        Type t = registry.lookupType(name);
        if (EnumType e = cast(EnumType) t)
            if (e.baseType !is null)
                t = e.baseType;
        return t;
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

    Type resolveArray(ArrayTypeExpr arr)
    {
        Type elemType = resolve(arr.elementType);
        long len = arr.length;
        Node node = arr.node;

        if (node !is null)
        {
            Node value = getIntLiteral(node);
            if (value !is null)
                len = cast(long) value.value.get!int;
        }

        if (elemType is null)
        {
            reportError("The type of the array element cannot be resolved.", arr.loc);
            return new PrimitiveType(BaseType.Any);
        }

        return new ArrayType(elemType, 1, len, arr.constant);
    }

    Type resolvePointer(PointerTypeExpr ptr)
    {
        Type pointeeType = resolve(ptr.pointeeType);

        if (pointeeType is null)
        {
            reportError("The specified type cannot be resolved.", ptr.loc);
            return new PrimitiveType(BaseType.Any);
        }

        auto pointerType = new PointerType(pointeeType);
        // Se o tipo apontado é constante (ex: const int), então o ponteiro
        // é um "refConst" (referência para constante).
        if (pointeeType.constant || pointeeType.refConst) 
            pointerType.refConst = true;

        return pointerType;
    }
}
