module frontend.types.type_expr;

import frontend;

abstract class TypeExpr : Node
{
    Loc loc;
    bool constant = false;

    abstract string toStr();
}

class NamedTypeExpr : TypeExpr
{
    string name;

    this(string name, Loc loc)
    {
        this.name = name;
        this.loc = loc;
    }

    override string toStr()
    {
        return name;
    }

    override TypeExpr clone()
    {
        return new NamedTypeExpr(name, loc);
    }

    override void print(ulong ident = 0, bool isLast = false)
    {
    }
}

class ArrayTypeExpr : TypeExpr
{
    TypeExpr elementType;
    long length = 0;
    Node node = null;

    this(TypeExpr elementType, Loc loc, long length = 0)
    {
        this.elementType = elementType;
        this.loc = loc;
        this.length = length;
    }

    override string toStr()
    {
        return elementType.toStr() ~ "[" ~ to!string(length) ~ "]";
    }

    override TypeExpr clone()
    {
        return new ArrayTypeExpr(cast(TypeExpr)elementType.clone(), loc);
    }

    override void print(ulong ident = 0, bool isLast = false)
    {
    }
}

class QualifiedTypeExpr : TypeExpr
{
    string[] parts;

    this(string[] parts, Loc loc)
    {
        this.parts = parts;
        this.loc = loc;
    }

    override string toStr()
    {
        import std.array : join;

        return parts.join(".");
    }

    override TypeExpr clone()
    {
        return new QualifiedTypeExpr(parts.dup, loc);
    }

    override void print(ulong ident = 0, bool isLast = false)
    {
    }
}

class GenericTypeExpr : TypeExpr
{
    TypeExpr baseType;
    TypeExpr[] typeArgs;

    this(TypeExpr baseType, TypeExpr[] typeArgs, Loc loc)
    {
        this.baseType = baseType;
        this.typeArgs = typeArgs;
        this.loc = loc;
    }

    override string toStr()
    {
        import std.algorithm : map;
        import std.array : join;
        import std.conv : to;

        string args = typeArgs.map!(t => t.toStr()).join(", ");
        return baseType.toStr() ~ "!" ~ args;
    }

    override TypeExpr clone()
    {
        import std.algorithm : map;
        import std.array : array;

        return new GenericTypeExpr(
            cast(TypeExpr) baseType.clone(),
            typeArgs.map!(t => cast(TypeExpr) t.clone()).array,
            loc
        );
    }

    override void print(ulong ident = 0, bool isLast = false)
    {
    }
}

class FunctionTypeExpr : TypeExpr
{
    TypeExpr[] paramTypes;
    TypeExpr returnType;

    this(TypeExpr[] paramTypes, TypeExpr returnType, Loc loc)
    {
        this.paramTypes = paramTypes;
        this.returnType = returnType;
        this.loc = loc;
    }

    override string toStr()
    {
        import std.algorithm : map;
        import std.array : join;

        string params = paramTypes.map!(t => t.toStr()).join(", ");
        return "(" ~ params ~ ") -> " ~ returnType.toStr();
    }

    override TypeExpr clone()
    {
        import std.algorithm : map;
        import std.array : array;

        return new FunctionTypeExpr(
            paramTypes.map!(t => cast(TypeExpr) t.clone()).array,
            cast(TypeExpr) returnType.clone(),
            loc
        );
    }

    override void print(ulong ident = 0, bool isLast = false)
    {
        import std.stdio : write, writeln;
        import std.array : replicate;

        string prefix = "  ".replicate(cast(size_t) ident);
        string branch = isLast ? "└── " : "├── ";

        writeln(prefix, branch, "FunctionTypeExpr: ");
        writeln(prefix, branch, toStr());
    }
}

class PointerTypeExpr : TypeExpr
{
    TypeExpr pointeeType;

    this(TypeExpr pointeeType, Loc loc)
    {
        this.pointeeType = pointeeType;
        this.loc = loc;
    }

    override string toStr()
    {
        return pointeeType.toStr() ~ "*";
    }

    override TypeExpr clone()
    {
        return new PointerTypeExpr(cast(TypeExpr) pointeeType.clone(), loc);
    }

    override void print(ulong ident = 0, bool isLast = false)
    {
        import std.stdio : write, writeln;
        import std.array : replicate;

        string prefix = "  ".replicate(cast(size_t) ident);
        string branch = isLast ? "└── " : "├── ";

        writeln(prefix, branch, "PointerTypeExpr");
        writeln(prefix, branch, toStr());
    }
}

class StructTypeExpr : TypeExpr
{
    string structName;

    this(string structName, Loc loc)
    {
        this.structName = structName;
        this.loc = loc;
    }

    override string toStr()
    {
        return structName;
    }

    override TypeExpr clone()
    {
        return new StructTypeExpr(structName, loc);
    }

    override void print(ulong ident = 0, bool isLast = false)
    {
        import std.stdio : write, writeln;
        import std.array : replicate;

        string prefix = "  ".replicate(cast(size_t) ident);
        string branch = isLast ? "└── " : "├── ";

        writeln(prefix, branch, "StructTypeExpr: ", structName);
    }
}

class EnumTypeExpr : TypeExpr
{
    string enumName;

    this(string enumName, Loc loc)
    {
        this.enumName = enumName;
        this.loc = loc;
    }

    override string toStr()
    {
        return enumName; // Or "enum " ~ enumName
    }

    override TypeExpr clone()
    {
        return new EnumTypeExpr(enumName, loc);
    }

    override void print(ulong ident = 0, bool isLast = false)
    {
        import std.stdio : write, writeln;
        import std.array : replicate;

        string prefix = "  ".replicate(cast(size_t) ident);
        string branch = isLast ? "└── " : "├── ";

        writeln(prefix, branch, "EnumTypeExpr: ", enumName);
    }
}

class UnionTypeExpr : TypeExpr
{
    string unionName;

    this(string unionName, Loc loc)
    {
        this.unionName = unionName;
        this.loc = loc;
    }

    override string toStr()
    {
        return unionName; // Or "union " ~ unionName
    }

    override TypeExpr clone()
    {
        return new UnionTypeExpr(unionName, loc);
    }

    override void print(ulong ident = 0, bool isLast = false)
    {
        import std.stdio : write, writeln;
        import std.array : replicate;

        string prefix = "  ".replicate(cast(size_t) ident);
        string branch = isLast ? "└── " : "├── ";

        writeln(prefix, branch, "UnionTypeExpr: ", unionName);
    }
}
