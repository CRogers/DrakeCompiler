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
COMPILERLOC = "bin/BasicCompiler"

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
	if os.path.isfile(path):
		return static_file(name, 'web/scripts/')
	elif os.path.isfile(coffeePath):
		with open(coffeePath) as f:
			return coffeescript.compile(f.read())
	abort(404)

@get('/api/tests')
def getTests():
	response.content_type = 'test/json'
	return json.dumps(os.listdir(TESTSDIR))

@get('/api/tests/<name>')
def getTest(name=''):
	return static_file(name, TESTSDIR)

@post('/api/compiler')
def postCompiler():
	response.content_type = 'test/json'
	code = request.forms.get('code')

	fd, tmp = tempfile.mkstemp()
	os.write(fd, code)
	os.close(fd)

	lexer = run(COMPILERLOC, '-s -l ' + tmp)
	parser = run(COMPILERLOC, '-s -p ' + tmp)
	llvm = run(COMPILERLOC, '-s -v ' + tmp)
	asm = run(COMPILERLOC, '-s -a ' + tmp)

	return json.dumps({'lexer':lexer, 'parser':parser, 'llvm':llvm, 'asm':asm})

bottle.run(host='localhost', port='8900')