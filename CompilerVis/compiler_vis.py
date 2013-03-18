import subprocess as sub
import json, os
import bottle
from bottle import get, post, static_file, abort, request, response
from hamlpy import hamlpy
from scss import parser
import re
import coffeescript
import tempfile

TESTSDIR = '../tests/'
COMPILERLOC = "../BasicCompiler/bin/Debug/BasicCompiler.exe"

def run(program, args):
	print(program + ' ' + args)
	p = sub.Popen(program + ' ' + args, stdout=sub.PIPE, stderr=sub.PIPE)
	output, errors = p.communicate()
	return output

@get('/')
def getIndex():
	with open('web/index.haml') as f:
		return hamlpy.Compiler().process(f.read())

@get('/styles/<name>')
def getScript(name=''):
	response.content_type = 'text/css'

	path = 'web/styles/' + name
	sassPath = re.sub('.css', '.scss', path)
	if os.path.isfile(path):
		return static_file(name, 'web/styles/')
	elif os.path.isfile(sassPath):
		return parser.load(sassPath)
	abort(404)

@get('/scripts/<name>')
def getScript(name=''):
	response.content_type = 'text/javascript'

	path = 'web/scripts/' + name
	coffeePath = re.sub('.js', '.coffee', path)
	print("ispath: " + path + " : " + str(os.path.isfile(path)))
	if os.path.isfile(path):
		return static_file(name, 'web/scripts/')
	elif os.path.isfile(coffeePath):
		with open(coffeePath) as f:
			return coffeescript.compile(f.read())
	abort(404)

@get('/api/tests')
def getTests():
	response.content_type = 'text/json'
	return json.dumps(os.listdir(TESTSDIR))

@get('/api/tests/<name>')
def getTest(name=''):
	print 'returning test ' + TESTSDIR + '/' + name 
	return static_file(name, TESTSDIR)

@post('/api/compiler/lexer')
def postCompilerLex():
	return postCompiler(response, request, "lexer", "-s -l")

@post('/api/compiler/parser')
def postCompilerLex():
	return postCompiler(response, request, "parser", "-s -p")

@post('/api/compiler/parserAnnot')
def postCompilerLex():
	return postCompiler(response, request, "parserAnnot", "-s -q")

@post('/api/compiler/llvm')
def postCompilerLex():
	return postCompiler(response, request, "llvm", "-s -v")

@post('/api/compiler/asm')
def postCompilerLex():
	return postCompiler(response, request, "asm", "-s -a")

@post('/api/compiler/exe')
def postCompilerExe():
	response.content_type = 'text/json'
	code = request.forms.get('code')
	llvm = runCompiler(code, "-s -v")

	fd, tmp = tempfile.mkstemp()
	os.write(fd, llvm)
	os.close(fd)

	return json.dumps({"exe": run("lli", tmp)})

def postCompiler(response, request, name, switches):
	response.content_type = 'text/json'
	code = request.forms.get('code')

	return json.dumps({name: runCompiler(code, switches)})


def runCompiler(code, switches):
	fd, tmp = tempfile.mkstemp()
	os.write(fd, code)
	os.close(fd)

	return run(COMPILERLOC, switches + ' ' + tmp)

bottle.run(host='localhost', port='8900')
