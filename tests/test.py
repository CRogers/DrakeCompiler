#!python

import re
import sys
import os, tempfile
import subprocess as sub
from colorama import init, Fore, Style

init()

COMPILERLOC = "../BasicCompiler/bin/Debug/BasicCompiler.exe"

def run(program, args):
	p = sub.Popen(program + ' ' + args, stdout=sub.PIPE, stderr=sub.PIPE)
	output, errors = p.communicate()
	return output

def writeTemp(text):
	fd, tmp = tempfile.mkstemp()
	os.write(fd, text)
	os.close(fd)
	return tmp

def stripRs(text):
	return text.replace('\r', '')

def printHex(s):
	return ":".join("{0:x}".format(ord(c)) for c in s)

def readFile(path):
	return open(path, 'r').read()

def getAnswer(text):
	ans = []
	add = False
	for x in text.split('\n'):
		if x == ">>>*/":
			return '\n'.join(ans)
		if add == True:
			ans.append(x)
		if x == "/*<<<":
			add = True
	return "failed to find answer"

def processTest(path):
	sys.stdout.write('Testing ' + path + '... ')
	text = stripRs(readFile(path))
	ans = getAnswer(text)
	compiled = run(COMPILERLOC, '-s -v ' + path)
	compiledLoc = writeTemp(compiled)
	output = stripRs(run('lli', compiledLoc))
	if output == ans:
		print Fore.GREEN + "Success" + Fore.RESET
	else:
		print Fore.RED + "Fail"
		print Fore.MAGENTA + "------------------------------------------------------------------------------"
		print Fore.CYAN + output
		print ans
		print printHex(output)
		print printHex(ans)
		print Fore.MAGENTA + "------------------------------------------------------------------------------" + Fore.RESET

for x in sys.argv[1:]:
	processTest(x)