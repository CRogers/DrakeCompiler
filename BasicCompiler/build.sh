#!/bin/sh

fslex --unicode Lexer.fsl -o Lexer.fs
fsyacc Parser.fsy -o Parser.fs --module Parser

fsc --nologo --debug --target:exe -r:../lib/FSharp.PowerPack.dll --out:bin/build/BasicCompiler.exe \
	Print.fs \
	Tree.fs \
	Parser.fs \
	Lexer.fs \
	Program.fs
	
