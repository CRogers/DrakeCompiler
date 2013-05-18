import subprocess as sub
import json, os, shutil
import bottle
from bottle import get, post, static_file, abort, request, response
from hamlpy import hamlpy
from scss import parser
import re
import coffeescript
import tempfile
import glob
import base64

TESTSDIR = '../tests/'
COMPILERLOC = "../DrakeCompiler/bin/Debug/DrakeCompiler.exe"

def run(program, args):
	print(program + ' ' + args)
	p = sub.Popen(program + ' ' + args, stdout=sub.PIPE, stderr=sub.PIPE)
	output, errors = p.communicate()
	return output

def writeTemp(text):
	fd, tmp = tempfile.mkstemp()
	os.write(fd, text)
	os.close(fd)
	return tmp

def makeTempDir():
	return tempfile.mkdtemp()

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
	cwd = os.getcwd()
	os.chdir(TESTSDIR)
	ret = json.dumps(glob.glob('*.b'))
	os.chdir(cwd)

	return ret

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

	tmp = writeTemp(llvm)
	ret = json.dumps({"exe": run("lli", tmp)})
	os.remove(tmp)

	return ret

def dotFiles(optionname, start):
	run('opt', '-dot-' + optionname + ' llvm.bc')

	pngs = []
	for f in glob.glob(start + '*.dot'):
		pngs.append('data:image/png;base64,' + base64.b64encode(run('dot', ' -Tpng ' + f)))

	return pngs

@post('/api/compiler/dots')
def postCompilerCfg():
	response.content_type = 'text/json'
	code = request.forms.get('code')
	llvm = runCompiler(code, "-s -v")
	tmpDir = makeTempDir()
	print tmpDir

	owd = os.getcwd()
	os.chdir(tmpDir)

	with open('llvm.bc', 'w') as f:
		f.write(llvm)

	cfg = dotFiles('cfg-only', 'cfg')
	dom = dotFiles('dom', 'dom')
	reg = dotFiles('regions-only', 'reg')
	cg  = dotFiles('callgraph', 'callgraph')
	
	os.chdir(owd)
	shutil.rmtree(tmpDir)

	return json.dumps({'cfg': cfg, 'dom': dom, 'reg': reg, 'cg': cg})



def postCompiler(response, request, name, switches):
	response.content_type = 'text/json'
	code = request.forms.get('code')

	return json.dumps({name: runCompiler(code, switches)})


def runCompiler(code, switches):
	tmp = writeTemp(code)
	ret = run(COMPILERLOC, switches + ' ' + tmp)
	os.remove(tmp)

	return ret

bottle.run(host='localhost', port='8900')
