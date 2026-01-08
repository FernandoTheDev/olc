module app;

import std.stdio, std.file, std.getopt, std.path, std.format;
import frontend, common.reporter, env, cli, middle, target;
import middle.hir.hir, middle.hir.lowering, middle.hir.dump;
import middle.mir.mir, middle.mir.lowering, middle.mir.dump;
import backend.llvm.codegen, backend.builder;
import core.stdc.stdlib : exit;
import cli;

CompilerConfig config;

void fatal(Args...)(string fmt, Args args)
{
    stderr.writefln(fmt, args);
    exit(1);
}

void checkErrors(DiagnosticError erro)
{
    if (erro.hasErrors() || erro.hasWarnings())
    {
        erro.printDiagnostics();
        if (erro.hasErrors()) exit(1);
        erro.clear();
    }
}

void printVersion()
{
    writeln("Orn LLVM Compiler v", VERSION);
}

void printHelp()
{
    writeln("Usage: olc [options] <file.orn>");
    writeln("\nOptions:");
    writeln("  --of, --output <file>  Specify output binary filename (default: a.out)");
    writeln("  -O, --opt-level <n>    Set optimization level (0=Debug, 3=Release)");
    writeln("  --emit-llvm            Generate human-readable .ll file");
    writeln("  --dump-hir             Print HIR representation to stdout");
    writeln("  --dump-mir             Print MIR representation to stdout");
    writeln("  --target <triple>      Compile for a specific target (e.g., x86_64-linux-gnu)");
    writeln("  -v, --version          Show compiler version");
    writeln("  -h, --help             Show this help message");
    writeln("\nExamples:");
    writeln("  olc main.orn");
    writeln("  olc -O 3 --of myapp main.orn");
    writeln("  olc --vm main.orn");
}

string extractDir(string path)
{
	string dir = dirName(path);
	return dir == "." || dir == "" ? "." : dir;
}

void main(string[] args)
{
    DiagnosticError error = new DiagnosticError();
    bool helpWanted = false;
    bool versionWanted = false;

    try
    {
        getopt(
            args,
            std.getopt.config.passThrough,
            "of|output",     &config.outputFile,
            "O|opt-level",  &config.optLevel,
            "emit-llvm",    &config.emitLLVM,
            "dump-mir",     &config.dumpMir,
            "dump-hir",     &config.dumpHir,
            "llvm",     &config.backendLLVM,
            "vm",     &config.backendVM,
            "asm",     &config.backendASM,
            "jit",     &config.backendJIT,
            "target",       &config.targetTriple,
            "v|version",      &versionWanted,
            "h|help",         &helpWanted,
            "C|clang",      &config.compilerArg,
        );

        if (helpWanted)
        {
            printHelp();
            return;
        }

        if (versionWanted)
        {
            printVersion();
            return;
        }

        if (args.length < 2)
            fatal("No input file provided. Run 'olc --help' for usage.");

        config.inputFile = args[1];

        if (!exists(config.inputFile))
            fatal("File '%s' not found.", config.inputFile);

        string src = readText(config.inputFile);
        string pathRoot = extractDir(config.inputFile);
        Token[] tokens = new Lexer(config.inputFile, src, pathRoot, error).tokenize();
        TypeRegistry registry = new TypeRegistry();

        Program program = new Parser(tokens, error, registry, pathRoot).parseProgram();
        checkErrors(error);

        Context ctx = new Context(error);
        TypeChecker checker = new TypeChecker(ctx, error, registry);

        new Semantic1(ctx, registry, error, pathRoot, checker).analyze(program);
        checkErrors(error);

        new Semantic2(ctx, error, registry, null, checker).analyze(program);
        checkErrors(error);

        new Semantic3(ctx, error, registry, pathRoot, checker).analyze(program);
        checkErrors(error);

        // program.print();

        HirProgram hir = new AstLowerer().lower(program);
        
        if (config.dumpHir)
        {
            writeln("\n=== HIR DUMP ===");
            dumpHir(hir); 
            writeln("================\n");
        }

        if (config.backendVM)
        {
            BackendBuilder.buildVM(hir);
            return;
        }

        MirProgram mir = new HirToMir(registry).lower(hir);

        if (config.dumpMir)
        {
            writeln("\n=== MIR DUMP ===");
            dumpMir(mir);
            writeln("================\n");
        }

        TargetInfo triple = config.targetTriple == ""? getTarget() : TargetInfo(config.targetTriple);
        BackendBuilder.buildLLVM(
            mir, 
            hir, 
            triple, 
            config
        );
    }
    catch (Exception e)
    {
        if (!error.hasErrors())
        {
            stderr.writeln("\n\033[1;31m[INTERNAL COMPILER ERROR]\033[0m");
            stderr.writeln(e); // Mensagem curta
            if (config.verbose) stderr.writeln(e.info); // Stack trace só no verbose
            exit(1);
        }
        error.printDiagnostics();
        if (config.verbose) writeln(e);
    }
}
