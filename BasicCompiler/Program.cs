using System;
using System.Diagnostics;
using System.IO;
using System.Text;
using CommandLine;
using System.Collections.Generic;

namespace BasicCompiler
{
    class Options
    {
        [ValueList(typeof(List<string>), MaximumElements = 1)]
        public IList<string> Files { get; set; }

        [Option("l", "emit-lexer", HelpText = "Emit the result of the lexing stage")]
        public bool EmitLexer { get; set; }

        [Option("p", "emit-parser", HelpText = "Emit the result of the parsing stage")]
        public bool EmitParser { get; set; }

        [Option("v", "emit-llvm", HelpText = "Emit the resulting LLVM intermediary code")]
        public bool EmitLLVM { get; set; }

        [Option("a", "emit-asm", HelpText = "Emit the resulting assembly")]
        public bool EmitASM { get; set; }

        [Option("s", "stdout", HelpText = "Write to stdout instead of a file")]
        public bool Stdout { get; set; }

        [HelpOption("h", "help", HelpText = "Display the help screen")]
        public string GetUsage()
        {
            var help = new StringBuilder();
            help.AppendLine("BasicCompiler 0.1 by Callum Rogers");
            return help.ToString();
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            var options = new Options();
            var parser = new CommandLineParser();
            if (parser.ParseArguments(args, options)) {
                string input = File.ReadAllText(options.Files[0]);

                string output = "No output selected!";

                if (options.EmitLexer) {
                    output = Compiler.lexText(input);
                }
                else if (options.EmitParser) {
                    output = Print.fmt(Compiler.parseText(input));
                }
                else if (options.EmitLLVM || options.EmitASM) {
                    var module = Compiler.compile(input);
                    using (var tmpBc = new TmpFile())
                    using (var tmpOut = new TmpFile()) {
                        Compiler.writeModuleToFile(tmpBc.FilePath, module);

                        var program = options.EmitLLVM ? "llvm-dis" : "llc";
                        var argsFmt = options.EmitLLVM ? "-o={0} {1}" : "-x86-asm-syntax=intel -o={0} {1}";
                        var proc = Process.Start(program, string.Format(argsFmt, tmpOut.FilePath, tmpBc.FilePath));
                        proc.WaitForExit();

                        output = File.ReadAllText(tmpOut.FilePath);
                    }
                }

                if (options.Stdout) {
                    Console.Write(output);
                }
                else {
                    File.WriteAllText("a.out", output);
                }

                return;
            }

            Console.WriteLine("Can't parse command line args!");
        }
    }
}
