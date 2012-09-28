#!/bin/sh

FSPP=../lib/FSharp.PowerPack/bin
OUT=bin/build

echo ::Building lexer
"$FSPP/fslex" --unicode Lexer.fsl -o Lexer.fs

echo
echo ::Building parser
"$FSPP/fsyacc" Parser.fsy -o Parser.fs --module Parser

echo ::Attempting to create output directory
mkdir -p $OUT

echo ::Compiling source files
fsc --nologo --debug --target:exe -r:"$FSPP/FSharp.PowerPack.dll" -r:"../lib/LLVMFSharp.dll" --out:"$OUT/BasicCompiler.exe" \
	Print.fs \
	Tree.fs \
	Parser.fs \
	Lexer.fs \
	Gen.fs \
	Program.fs
	
