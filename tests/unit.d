module tests.unit;

import std.stdio;
import std.process;
import std.file;
import std.path;
import std.array;
import std.string;
import std.conv;
import std.algorithm;
import std.datetime.stopwatch; // Better for benchmarking

struct TestCase
{
    string name;
    string inputPath;
    string[] expectedOutput;
    bool shouldCompile;
    bool isPerformanceTest;
    int timeoutSeconds;
}

class OlcTester
{
    private TestCase[] testCases;
    private int passedCount = 0;
    private int failedCount = 0;
    
    // Configurable paths
    private string olcExecutable = "olc"; 
    private string binDir = "bin_tests";

    this()
    {
        configureTests();
        setupEnvironment();
    }

    private void setupEnvironment()
    {
        if (!exists(binDir))
        {
            mkdir(binDir);
        }
    }

    private void configureTests()
    {
        testCases = [
            TestCase(
                "Hello World",
                buildPath("examples", "hello.orn"),
                ["Hello World!"],
                true,
                false,
                5
            ),
            // --- Arrays & Pointers ---
            TestCase(
                "Array Pointer Arithmetic",
                buildPath("examples", "arr.orn"),
                ["z = 69"],
                true,
                false,
                5
            ),
            TestCase(
                "Pointer Arithmetic Indexing",
                buildPath("examples", "pointer_arithmetic.orn"),
                [
                    "0 Name: Blopa",
                    "1 Name: Fernando",
                    "2 Name: Orn"
                ],
                true,
                false,
                5
            ),
            TestCase(
                "Manual Memory Dereference",
                buildPath("examples", "name.orn"),
                ["69"],
                true,
                false,
                5
            ),

            // --- Structs & Methods ---
            TestCase(
                "Struct Initialization",
                buildPath("examples", "block.orn"),
                ["size = 1", "color = green"],
                true,
                false,
                5
            ),
            TestCase(
                "Struct Modification",
                buildPath("examples", "point.orn"),
                ["100 20"],
                true,
                false,
                5
            ),
            TestCase(
                "Struct Methods",
                buildPath("examples", "string.orn"),
                ["Fernando"],
                true,
                false,
                5
            ),
            TestCase(
                "Struct Logic",
                buildPath("examples", "person.orn"),
                ["25", "110"],
                true,
                false,
                5
            ),
            TestCase(
                "Object Destructuring",
                buildPath("examples", "od.orn"),
                ["20"],
                true,
                false,
                5
            ),

            // --- Types, Casts & Enums ---
            TestCase(
                "Explicit Casts",
                buildPath("examples", "cast.orn"),
                ["x = 3.140000", "y = 10"],
                true,
                false,
                5
            ),
            TestCase(
                "Implicit Casts",
                buildPath("examples", "implicit_cast.orn"),
                [
                    "a: 10", "b: 10", "c: 255", "d: 127",
                    "e: 69", "f: 67", "g: 80", "h: 99"
                ],
                true,
                false,
                5
            ),
            TestCase(
                "Enums",
                buildPath("examples", "enum.orn"),
                ["0", "2"], // Red=0, Green=2
                true,
                false,
                5
            ),

            // --- Logic & Bitwise ---
            TestCase(
                "Bitwise Encoding",
                buildPath("examples", "encode.orn"),
                ["resultado: 69"],
                true,
                false,
                5
            ),
            TestCase(
                "Bitwise Encoding",
                buildPath("examples", "encode2.orn"),
                ["resultado: 69"],
                true,
                false,
                5
            ),
            TestCase(
                "Bitwise Flags",
                buildPath("examples", "flags.orn"),
                ["pode ler", "pode escrever"],
                true,
                false,
                5
            ),

            // --- Algorithms & Strings ---
            TestCase(
                "String Comparison",
                buildPath("examples", "compare_str.orn"),
                [
                    "Fernando: 8",
                    "Blopa: 5",
                    "0", // false
                    "1"  // true
                ],
                true,
                false,
                5
            ),
            TestCase(
                "Pyramid Loop",
                buildPath("examples", "pyramid.orn"),
                // Nota: O compareOutput usa .strip(), então os espaços iniciais 
                // da pirâmide serão ignorados na verificação, validando apenas os asteriscos.
                [
                    "* ",
                    "* * ",
                    "* * * ",
                    "* * * * ",
                    "* * * * * ",
                    "* * * * * * ",
                    "* * * * * * * "
                ],
                true,
                false,
                5
            ),
            TestCase(
                "Http test",
                buildPath("examples", "http.orn"),
                [
                    "🧪 HTTP Test Suite",
                    "",
                    "[TEST 1] GET example.com",
                    "  ✓ PASS",
                    "",
                    "[TEST 2] GET status 200",
                    "  ✓ PASS",
                    "",
                    "[TEST 3] POST with JSON",
                    "  ✓ PASS",
                    "",
                    "==========================================",
                    "Results: 3/3 tests passed"
                ],
                true,
                false,
                5
            ),

            // --- Performance ---
            TestCase(
                "Fibonacci Recursive",
                buildPath("examples", "fibo.orn"),
                ["102334155"],
                true,
                true, // Performance Test
                10    // Timeout maior para fib(40)
            ),
            // Add more tests here
        ];
    }

    public void runAllTests()
    {
        writeln("🔬 OLC Automated Test Suite");
        writeln("===========================");
        writeln();

        foreach (test; testCases)
        {
            runTest(test);
        }

        printSummary();
    }

    private void runTest(TestCase test)
    {
        write("🧪 Testing ", test.name, "... ");
        stdout.flush();

        try
        {
            // Generates a clean binary name: "hello_world"
            string binaryName = test.name.replace(" ", "_").toLower();
            string outputPath = buildPath(binDir, binaryName);

            // 1. Compilation Step
            // Using --of to specify output path
            string compileCmd = format("%s %s --of %s", olcExecutable, test.inputPath, outputPath);
            
            auto compileResult = executeShell(compileCmd);

            if (compileResult.status != 0)
            {
                failTest(test.name, "Compilation failed:\n" ~ compileResult.output);
                return;
            }

            auto runResult = executeShell("./" ~ outputPath);

            // 3. Output Verification
            if (test.expectedOutput.length > 0)
            {
                string rawOutput = runResult.output.strip();
                string[] actualLines = rawOutput.split('\n');

                if (!compareOutput(actualLines, test.expectedOutput))
                {
                    failTest(test.name,
                        format("Output mismatch!\nExpected:\n%s\n\nActual:\n%s", 
                        test.expectedOutput.join("\n"), rawOutput)
                    );
                    return;
                }
            }

            // Cleanup binary after success
            if (exists(outputPath))
            {
                remove(outputPath);
            }

            passTest(test.isPerformanceTest);

        }
        catch (Exception e)
        {
            failTest(test.name, "Exception thrown: " ~ e.msg);
        }
    }

    private bool compareOutput(string[] actual, string[] expected)
    {
        // Filter empty trailing lines if necessary, or keep strict count
        if (actual.length != expected.length)
        {
            return false;
        }

        foreach (i, line; actual)
        {
            if (line.strip() != expected[i].strip())
            {
                return false;
            }
        }
        return true;
    }

    private void passTest(bool isPerformance = false)
    {
        writeln("✅ PASSED", (isPerformance ? " (Performance Test)" : ""));
        passedCount++;
    }

    private void failTest(string name, string reason)
    {
        writeln("❌ FAILED");
        writeln("   Reason: ", reason);
        writeln();
        failedCount++;
    }

    private void printSummary()
    {
        writeln();
        writeln("📊 Test Results");
        writeln("===============");
        writeln("✅ Passed: ", passedCount);
        writeln("❌ Failed: ", failedCount);
        writeln("📊 Total:  ", passedCount + failedCount);
        writeln();

        if (failedCount == 0)
            writeln("🎉 ALL TESTS PASSED! OLC is working perfectly! 🚀");
        else
            writeln("⚠️  Some tests failed. Check the output above.");
    }

    public void runPerformanceBenchmark()
    {
        writeln();
        writeln("🚀 Running Performance Benchmark...");
        writeln("===================================");

        StopWatch sw = StopWatch(AutoStart.yes);
        
        foreach (test; testCases)
        {
            // Assuming the compiler has a 'transpilar' or similar subcommand, 
            // or just compiling without linking for speed test.
            // Adjust "transpilar" to "build" or "emit-ir" as per your CLI.
            executeShell(olcExecutable ~ " " ~ test.inputPath);
            
            // Clean up generated .d file (if transpiling to D)
            // Safer path handling:
            string generatedFile = test.inputPath.baseName.stripExtension ~ ".d";
            if (exists(generatedFile))
            {
                remove(generatedFile);
            }
        }

        sw.stop();
        long totalMs = sw.peek.total!"msecs";
        
        // Avoid division by zero
        long avg = testCases.length > 0 ? totalMs / testCases.length : 0;

        writeln("⚡ Processed ", testCases.length, " files in: ", totalMs, "ms");
        writeln("📈 Average: ", avg, "ms per file");
    }
}

void main()
{
    // Ensure we are in the right directory
    if (!exists("./olc") && !exists("olc.exe")) // Windows compatibility check
    {
        writeln("❌ Error: './olc' executable not found!");
        writeln("   Make sure to run this from the OLC project root.");
        return;
    }

    if (!exists("examples"))
    {
        writeln("❌ Error: 'examples/' directory not found!");
        return;
    }

    auto tester = new OlcTester();
    tester.runAllTests();
    tester.runPerformanceBenchmark();

    writeln();
    writeln("🎯 Test suite finished!");
}
