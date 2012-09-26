#!/bin/sh

FSPP=../lib/FSharp.PowerPack/bin
OUT=bin/build

"$FSPP/fslex" --unicode Lexer.fsl -o Lexer.fs
"$FSPP/fsyacc" Parser.fsy -o Parser.fs --module Parser

mkdir -p $OUT

fsc --nologo --debug --target:exe -r:"$FSPP/FSharp.PowerPack.dll" -r:"../lib/LLVMFSharp.dll" --out:"$OUT/BasicCompiler.exe" \
	Print.fs \
	Tree.fs \
	Parser.fs \
	Lexer.fs \
	Gen.fs \
	Program.fs
	
