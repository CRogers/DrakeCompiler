#!/usr/bin/env python

import os
import os.path
import inspect

import expanderparser as EP

import sys # only for stderr-write

# ---------------------------------------------
# file search utilities
# ---------------------------------------------

# ---------------------------------------------
# dump utilities
# ---------------------------------------------

def _set2str(val):
    elms= sorted(list(val))
    return "set(%s)" % repr(elms)

def _pr_set(val):
    print _set2str(val)

def find_file(filename, include_paths):
    """find a file in a list of include paths.

    include_paths MUST CONTAIN "" in order to search the 
    local directory.
    """
    for path in include_paths:
        p= os.path.join(path, filename)
        if os.path.exists(p):
            if os.access(p, os.R_OK):
                return p
            print "warning: file \"%s\" found but it is not readable" %\
                  p
    return None

# ---------------------------------------------
# all
# ---------------------------------------------

def parseString(st):
    return EP.parseAll(EP.IndexedString(st), 0)

def parseFile(filename):
    f= open(filename, "r")
    st= f.read()
    f.close()
    return parseString(st)

# ---------------------------------------------
# process parse-list
# ---------------------------------------------

class Block(object):
    """class that represents a block in the expander language.

    Each block has a parent-pointer, which may be None. Blocks are used to
    represent syntactical blocks in the expander language. They manage the
    global variable dictionary and some more properties that are needed during
    execution of the program.
    """
    def _posmsg(self, msg=None, pos=None):
        parts=[]
        if msg is not None:
            parts.append(msg)
        if self.filename is not None:
            parts.append("file \"%s\"" % self.filename)
        try:
            p_elm= self.parse_list[self.start_pos]
            parts.append("line %d, col %d" % p_elm.rowcol(pos))
            return " ".join(parts)
        except IndexError, e:
            return "unknown position"
    def _append(self, lst, name, val=None):
        def elm2str(e):
            if isinstance(e,set):
                return _set2str(e)
            return str(e)
        if val is not None:
            lst.append("    %-20s: %s" % (name,val))
        else:
            lst.append("    %-20s= %s" % (name,elm2str(getattr(self,name))))
    def _strlist(self):
        """utility for __str__.

        This function can be used by descendent classes in order to implement
        the __str__ method in a simple way.

        Tested by the testcode in __str__.
        """
        lst= []
        self._append(lst, "has parent",self.previous is not None)
        self._append(lst, "filename")
        self._append(lst, "template")
        self._append(lst, "template_path")
        self._append(lst, "has template_parselist_cache", 
                          self.template_parselist_cache is not None)
        self._append(lst, "exported_syms")
        self._append(lst, "direct_vars")
        self._append(lst, "direct_funcs")
        self._append(lst, "new_scope")
        self._append(lst, "skip")
        self._append(lst, "new_parse_list")
        self._append(lst, "lst_pos")
        self._append(lst, "start_pos")
        if self.lst_pos>=0 and self.lst_pos<len(self.parse_list):
            self._append(lst, "current parse elm",
                         self.parse_list[self.lst_pos])
        return lst
    def __init__(self, 
                 previous= None,
                 new_scope= False,
                 filename= None,
                 parse_list=None,
                 external_definitions={}):
        """The Block constructor.

        Properties of a Block:
            parse_list    -- the list with parse objects
            lst_pos       -- the position in the parse_list
            start_pos     -- the start position of the block in the parse_list
            previous      -- the previous block
            new_scope     -- True if the block is also a new scope with
                             respect to global variables.
            skip          -- if True, skip a block in the text when the code
                             is interpreted
            globals_      -- a dictionary with the current global variables of
                             the interpreter.
            exported_syms -- a list of symbols (names in globals_) that are
                             copied to the globals_ dictionary of the parent
                             block when the pop() method is called.
            filename      -- name of the interpreted file, may be None
            template      -- name of the file that "$subst" would include,
                             usually None
            template_path -- complete path of template
                             usually None
            template_parselist_cache -- 
                             parselist of the template, usually None.
                             This is a kind of optimization

        Here is an example:
        >>> b= Block(parse_list=[])
        >>> print b
        Block{
            has parent          : False
            filename            = None
            template            = None
            template_path       = None
            has template_parselist_cache: False
            exported_syms       = []
            direct_vars         = set([])
            direct_funcs        = set([])
            new_scope           = False
            skip                = False
            new_parse_list      = True
            lst_pos             = -1
            start_pos           = 0
        }
        >>> b= Block(b,True,parse_list=[])
        >>> b.print_block_list()
        Block{
            has parent          : False
            filename            = None
            template            = None
            template_path       = None
            has template_parselist_cache: False
            exported_syms       = []
            direct_vars         = set([])
            direct_funcs        = set([])
            new_scope           = False
            skip                = False
            new_parse_list      = True
            lst_pos             = -1
            start_pos           = 0
        }
        Block{
            has parent          : True
            filename            = None
            template            = None
            template_path       = None
            has template_parselist_cache: False
            exported_syms       = []
            direct_vars         = set([])
            direct_funcs        = set([])
            new_scope           = True
            skip                = False
            new_parse_list      = True
            lst_pos             = -1
            start_pos           = 0
        }
        """
        if (filename is None) or (filename==""):
            self.filename= None
        else:
            self.filename= filename
        self.previous= previous
        if previous is None:
            self.new_scope= False
            self.skip= False
            self.globals_= dict()
            self.direct_vars=set()
            self.direct_funcs=set()
            self.exported_syms=[]
            if parse_list is None:
                raise AssertionError, "without previous, parse_list "+\
                                      "is mandatory"
            self.new_parse_list= True
            self.template= None
            self.template_path= None
            self.template_parselist_cache= None
            self.parse_list= parse_list
            self.lst_pos= -1
            self.start_pos= 0
        else:
            if not isinstance(previous,Block):
                raise AssertionError, "previous is not a block: %s" %\
                                      str(previous)
            if self.filename is None:
                self.filename= previous.filename
            if parse_list is None:
                self.new_parse_list= False
                self.parse_list= previous.parse_list
                self.lst_pos= previous.lst_pos
                if previous.lst_pos<0:
                    self.start_pos= 0
                else:
                    self.start_pos= previous.lst_pos
            else:
                self.new_parse_list= True
                self.parse_list= parse_list
                self.lst_pos= -1
                self.start_pos= 0
            self.template= previous.template
            self.template_path= previous.template_path
            self.template_parselist_cache= previous.template_parselist_cache
            self.new_scope= new_scope
            self.skip= previous.skip
            if new_scope:
                self.globals_= dict(previous.globals_)
                self.direct_vars= set(previous.direct_vars)
                self.direct_funcs= set(previous.direct_funcs)
                self.exported_syms=[]
            else:
                self.globals_= previous.globals_
                self.direct_vars= previous.direct_vars
                self.direct_funcs= previous.direct_funcs
                self.exported_syms= previous.exported_syms
        for (k,v) in external_definitions.items():
            self.globals_[k]= v
    def parse_loop(self):
        self.lst_pos+= 1
        return self.lst_pos < len(self.parse_list)
    def parse_elm(self):
        return self.parse_list[self.lst_pos]
    def eval_(self, st):
        """perform eval with the globals_ dictionary of the block.

        Here is an example:
        >>> b= Block(parse_list=[])
        >>> b.exec_("a=2")
        >>> b.eval_("3*a")
        6
        """
        try:
            return eval(st, self.globals_)
        except SyntaxError, e:
            raise SyntaxError, "%s at %s" % (str(e), self._posmsg())
        except NameError, e:
            raise NameError, "%s at %s" % (str(e), self._posmsg())
        except IndexError, e:
            raise IndexError, "%s at %s" % (str(e), self._posmsg())
        except TypeError, e:
            raise TypeError, "%s at %s" % (str(e), self._posmsg())
        except Exception, e:
            sys.stderr.write("error at %s:\n" % self._posmsg())
            raise
    def str_eval(self, st):
        """perform eval with the globals_ dictionary of the block.

        Here is an example:
        >>> b= Block(parse_list=[])
        >>> b.exec_("a=2")
        >>> b.str_eval("3*a")
        '6'
        """
        val= self.eval_(st)
        try:
            return(str(val))
        except Exception, e:
            sys.stderr.write("error at %s:\n" % self._posmsg())
            raise
    def exec_(self, st):
        """perform exec with the globals_ dictionary of the block.

        Here is an example:
        >>> b= Block(parse_list=[])
        >>> b.exec_("a=1")
        >>> b["a"]
        1
        >>> b= Block(b,True)
        >>> b["a"]
        1
        >>> b.exec_("a=2")
        >>> b["a"]
        2
        >>> b.previous["a"]
        1
        """
        try:
            exec st in self.globals_
        except SyntaxError, e:
            raise SyntaxError, "%s at %s" % (str(e), self._posmsg())
        except NameError, e:
            raise NameError, "%s at %s" % (str(e), self._posmsg())
        except IndexError, e:
            raise IndexError, "%s at %s" % (str(e), self._posmsg())
        except TypeError, e:
            raise TypeError, "%s at %s" % (str(e), self._posmsg())
        except Exception, e:
            sys.stderr.write("error at %s:\n" % self._posmsg())
            raise
    def __getitem__(self, name):
        """looks up a value in the globals_ dictionary of the block.

        Here is an example:
        >>> b= Block(parse_list=[])
        >>> b.globals_["a"]= 5
        >>> b["a"]
        5
        """
        try:
            return self.globals_[name]
        except KeyError, e:
            raise KeyError, "%s at %s" % (str(e),self._posmsg())
    def __setitem__(self, name, val):
        """sets a value in the globals_ dictionary of the block.

        Here is an example:
        >>> b= Block(parse_list=[])
        >>> b["a"]= 5
        >>> b.globals_["a"]
        5
        """
        try:
            self.globals_[name]= val
        except KeyError, e:
            raise KeyError, "%s at %s" % (str(e),self._posmsg())
    def setdefault(self, name, val):
        """return a value from globals, set to a default if it's not defined.

        Here are some examples:
        >>> b= Block(parse_list=[])
        >>> b["a"]
        Traceback (most recent call last):
            ...
        KeyError: "'a' at unknown position"
        >>> b.setdefault("a",10)
        10
        >>> b["a"]
        10
        >>> b["a"]=11
        >>> b.setdefault("a",10)
        11
        """
        try:
            return self.globals_[name]
        except KeyError, e:
            self.globals_[name]= val
        return val
  
    def set_substfile(self, filename):
        if not isinstance(filename, str):
            raise EP.ParseException(
                    block._posmsg("filename must be a string at",
                                  pos=tp.start()))
        if filename == self.template:
            return
        self.template_parselist_cache= None
        self.template_path= None
        self.template= filename
    def substfile_parselist(self, include_paths):
        if self.template_parselist_cache is not None:
            return (self.template_path,self.template_parselist_cache)
        if self.template is None:
            raise ValueError, \
                  self._posmsg("substitition file name missing at")
        self.template_path= find_file(self.template, include_paths)
        if self.template_path is None:
            raise ValueError, \
                  self._posmsg("file \"%s\" not found in" % \
                               self.template)
        else:
            self.template_parselist_cache= parseFile(self.template_path)
        return (self.template_path,self.template_parselist_cache)
    def export_symbols(self, lst):
        """appends items to the export_symbols list.

        This list is used by the pop() method in order to copy values from the
        current globals_ dictionary to the globals_ dictionary of the previous
        block.

        Here is an example:
        >>> b= Block(parse_list=[])
        >>> b.export_symbols(["a","b"])
        >>> b.exported_syms
        ['a', 'b']
        >>> b.export_symbols(["d","e"])
        >>> b.exported_syms
        ['a', 'b', 'd', 'e']
        """
        self.exported_syms.extend(lst)
    def extend(self, lst):
        """adds items to the list of expander functions or variables.

        Here is an example:
        >>> a=1
        >>> b=2
        >>> def t(x):
        ...   return x+1
        ... 
        >>> block= Block(parse_list=[],external_definitions=globals())
        >>> block.extend(["a","b","t"])
        >>> _pr_set(block.direct_vars)
        set(['a', 'b'])
        >>> _pr_set(block.direct_funcs)
        set(['t'])
        """
        for elm in lst:
            obj= self.globals_[elm]
            if inspect.isbuiltin(obj):
                self.direct_funcs.add(elm)
                continue
            if inspect.isfunction(obj):
                self.direct_funcs.add(elm)
                continue
            # assume elm to be a variable:
            self.direct_vars.add(elm)
    def pop(self):
        """removes the current block and returns the previous one.

        Here is an example:
        >>> b= Block(parse_list=[])
        >>> b["a"]=1
        >>> b["b"]=2
        >>> b= Block(b,True)
        >>> b["a"]=10
        >>> b["b"]=20
        >>> b.export_symbols(["a"])
        >>> b= b.pop()
        >>> b["a"]
        10
        >>> b["b"]
        2
        """
        if self.previous is None:
            raise AssertionError,\
                  self._posmsg("block underflow (assertion) at")
        if self.new_scope:
            old= self.previous.globals_
            for elm in self.exported_syms:
                old[elm]= self.globals_[elm]
        # set the lst_pos in the parent Block:
        if not self.new_parse_list:
            self.previous.lst_pos= self.lst_pos
        return self.previous
    def __str__(self):
        """returns a string representation of the block.

        Here is an example:
        >>> b= Block(parse_list=[])
        >>> print b
        Block{
            has parent          : False
            filename            = None
            template            = None
            template_path       = None
            has template_parselist_cache: False
            exported_syms       = []
            direct_vars         = set([])
            direct_funcs        = set([])
            new_scope           = False
            skip                = False
            new_parse_list      = True
            lst_pos             = -1
            start_pos           = 0
        }
        >>> b= Block(b,True)
        >>> print b
        Block{
            has parent          : True
            filename            = None
            template            = None
            template_path       = None
            has template_parselist_cache: False
            exported_syms       = []
            direct_vars         = set([])
            direct_funcs        = set([])
            new_scope           = True
            skip                = False
            new_parse_list      = False
            lst_pos             = -1
            start_pos           = 0
        }
        """
        lst=["%s{" % "Block"]
        lst.extend(self._strlist())
        lst.append("}")
        return "\n".join(lst)
    def get_block_list(self):
        """returns all blocks of the list.

        The list is returned with the oldest block first.
        """
        lst=[]
        block= self
        while block is not None:
            lst.append(block)
            block= block.previous
        lst.reverse()
        return lst
    def str_block_list(self):
        """returns a string representation of all blocks in the list.

        The list is returned with the oldest block first.
        """
        return [str(elm) for elm in self.get_block_list()]
    def print_block_list(self):
        """print all blocks in the list.

        The list is returned with the oldest block first.

        Here is an example:
        >>> b= Block(parse_list=[])
        >>> print b
        Block{
            has parent          : False
            filename            = None
            template            = None
            template_path       = None
            has template_parselist_cache: False
            exported_syms       = []
            direct_vars         = set([])
            direct_funcs        = set([])
            new_scope           = False
            skip                = False
            new_parse_list      = True
            lst_pos             = -1
            start_pos           = 0
        }
        >>> b= Block(b,True)
        >>> print b
        Block{
            has parent          : True
            filename            = None
            template            = None
            template_path       = None
            has template_parselist_cache: False
            exported_syms       = []
            direct_vars         = set([])
            direct_funcs        = set([])
            new_scope           = True
            skip                = False
            new_parse_list      = False
            lst_pos             = -1
            start_pos           = 0
        }
        >>> b.print_block_list()
        Block{
            has parent          : False
            filename            = None
            template            = None
            template_path       = None
            has template_parselist_cache: False
            exported_syms       = []
            direct_vars         = set([])
            direct_funcs        = set([])
            new_scope           = False
            skip                = False
            new_parse_list      = True
            lst_pos             = -1
            start_pos           = 0
        }
        Block{
            has parent          : True
            filename            = None
            template            = None
            template_path       = None
            has template_parselist_cache: False
            exported_syms       = []
            direct_vars         = set([])
            direct_funcs        = set([])
            new_scope           = True
            skip                = False
            new_parse_list      = False
            lst_pos             = -1
            start_pos           = 0
        }
        """
        print "\n".join(self.str_block_list())

class IncludeBlock(Block):
    """implements a $include(filename) block.

    This block is simply a variable scope, so it is derived from Block where
    the constructor is called with new_scope=True.
    """
    def __init__(self,
                 previous= None,
                 new_scope= False,
                 filename= None,
                 include_paths=[]):
        path= find_file(filename, include_paths)
        if path is None:
            parse_list= []
        else:
            parse_list= parseFile(path)
        Block.__init__(self, previous, new_scope, 
                       path, 
                       parse_list)
        if path is None:
            raise ValueError, \
                  self._posmsg("file \"%s\" not found in" % filename)
    def __str__(self):
        lst=["%s{" % "IncludeBlock"]
        lst.extend(self._strlist())
        lst.append("}")
        return "\n".join(lst)

class SubstBlock(Block):
    """implements a $subst(parameters) block.

    This block is simply a variable scope, so it is derived from Block where
    the constructor is called with new_scope=True.
    """
    def __init__(self,
                 previous= None,
                 filename= None,
                 include_paths=[],
                 external_definitions={}):
        (path,parse_list)= previous.substfile_parselist(include_paths)
        Block.__init__(self, previous, True, # always new scope
                       path, 
                       parse_list,
                       external_definitions)
        if path is None:
            raise ValueError, \
                  self._posmsg("file \"%s\" not found in" % filename)
    def __str__(self):
        lst=["%s{" % "SubstBlock"]
        lst.extend(self._strlist())
        lst.append("}")
        return "\n".join(lst)

class PatternBlock(Block):
    """implements a $pattern(parameters) block.

    This block is simply a variable scope, so it is derived from Block where
    the constructor is called with new_scope=True.
    """
    def _strlist(self):
        lst= Block._strlist(self)
        self._append(lst, "heading")
        self._append(lst, "lines")
        self._append(lst, "curr_line")
        return lst
    def __init__(self,
                 previous= None,
                 filename= None,
                 include_paths=[],
                 heading=[],
                 lines=[]):
        (path,parse_list)= previous.substfile_parselist(include_paths)
        Block.__init__(self, previous, False, # no new scope
                       path, parse_list)
        if path is None:
            raise ValueError, \
                  self._posmsg("file \"%s\" not found in" % filename)
        self.heading= heading
        self.lines= lines
        if len(self.lines)<1:
            raise ValueError, \
                  self._posmsg("no instantiation data")
        self.curr_line= -1
        self.def_vars()
    def pop(self):
        if not self.def_vars():
            return Block.pop(self)
        # reset position in file
        self.lst_pos= -1
        return self
    def def_vars(self):
        self.curr_line+=1
        if self.curr_line >=len(self.lines):
            return False
        line= self.lines[self.curr_line]
        for i in xrange(0,len(self.heading)):
            self[self.heading[i]]= line[i]
        return True
    def __str__(self):
        lst=["%s{" % "PatternBlock"]
        lst.extend(self._strlist())
        lst.append("}")
        return "\n".join(lst)

class BeginBlock(Block):
    """implements a $begin .. $end block.

    This block is simply a variable scope, so it is derived from Block where
    the constructor is called with new_scope=True.
    """
    def __init__(self, 
                 previous= None,
                 filename= None):
        Block.__init__(self, previous, True)
        self.skip= previous.skip
    def __str__(self):
        lst=["%s{" % "BeginBlock"]
        lst.extend(self._strlist())
        lst.append("}")
        return "\n".join(lst)

class IfBlock(Block):
    """implements a $if .. $else .. $endif block.

    An $if block never has a variable scope, so the base Block object is
    called with new_scope=False.
    """
    def _strlist(self):
        lst= Block._strlist(self)
        self._append(lst, "prev_skip")
        self._append(lst, "in_else_part")
        self._append(lst, "found")
        return lst
    def __init__(self, 
                 previous= None,
                 condition= True):
        """constructs the $if block.

        condition is the boolean value of the $if condition.
        """
        Block.__init__(self, previous, False)
        self.prev_skip= previous.skip
        self.in_else_part= False
        if condition:
            self.found= True
            self.skip= self.prev_skip
        else:
            self.found= False
            self.skip= True
    def enter_elif(self, condition):
        if self.found:
            self.skip= True
        else:
            if condition:
                self.found= True
                self.skip= self.prev_skip
            else:
                self.skip= True
    def enter_else(self):
        """this should be called when $else is encountered.
        """
        if self.in_else_part:
            raise EP.ParseException(
                    self._posmsg("one \"else\" too many at"))
        self.in_else_part= True
        if not self.found:
            self.skip= self.prev_skip
        else:
            self.skip= True
    def __str__(self):
        lst=["%s{" % "IfBlock"]
        lst.extend(self._strlist())
        lst.append("}")
        return "\n".join(lst)

class ForBlock(Block):
    """implements a $for .. $endfor block.
    """
    def _strlist(self):
        lst= Block._strlist(self)
        self._append(lst, "value_list")
        self._append(lst, "index")
        self._append(lst, "var_expr")
        self._append(lst, "jump_lst_pos")
        self._append(lst, "jump parse elm",self.parse_list[self.jump_lst_pos])
        return lst
    def __init__(self, 
                 previous= None,
                 new_scope= False,
                 value_list=[],
                 var_expr=""):
        """constructor of the block.

        var_expr -- the expression that contains the loop variable or the
                    tuple with the loop variables.
        """
        Block.__init__(self, previous, new_scope)
        self.value_list= value_list
        self.index=0 # current index within self.value_list
        self.var_expr= var_expr
        self.jump_lst_pos= self.lst_pos
        if len(value_list)<=0:
            self.skip= True
        else:
            self.skip= previous.skip
    def set_loop_var(self):
        if not self.skip:
            self.exec_("%s=%s" % (self.var_expr,
                                  repr(self.value_list[self.index])))
    def next_loop(self):
        """performs next loop.

        returns:
          True when the loop is not yet finished.
        """
        self.index+=1
        do_loop= self.index< len(self.value_list)
        if do_loop:
            self.lst_pos= self.jump_lst_pos
        return do_loop
    def __str__(self):
        lst=["%s{" % "ForBlock"]
        lst.extend(self._strlist())
        lst.append("}")
        return "\n".join(lst)

class WhileBlock(Block):
    """implements a $while .. $endwhile block.
    """
    def _strlist(self):
        lst= Block._strlist(self)
        self._append(lst, "while_expr")
        self._append(lst, "jump parse elm",self.parse_list[self.jump_lst_pos])
        return lst
    def __init__(self, 
                 previous= None,
                 new_scope= False,
                 while_expr=""):
        """constructor of the block.

        while_expr -- the expression that contains the loop variable or the
                      tuple with the loop variables.
        """
        Block.__init__(self, previous, new_scope)
        self.while_expr= while_expr
        self.jump_lst_pos= self.lst_pos
        if not self.eval_(self.while_expr):
            self.skip= True
        else:
            self.skip= previous.skip
    def next_loop(self):
        """performs next loop.

        returns:
          True when the loop is not yet finished.
        """
        do_loop= self.eval_(self.while_expr)
        if do_loop:
            self.lst_pos= self.jump_lst_pos
        return do_loop
    def __str__(self):
        lst=["%s{" % "WhileBlock"]
        lst.extend(self._strlist())
        lst.append("}")
        return "\n".join(lst)

def __pyexpander_helper(*args, **kwargs):
    if len(args)==1:
        fn= args[0]
    elif len(args)>1:
        raise ValueError, "only one unnamed argument is allowed"
    else:
        fn= None
    return(fn, kwargs)

def __pyexpander_helper2(**kwargs):
    return(kwargs)

def processToList(parse_list, filename=None,
                  external_definitions={},
                  allow_nobracket_vars= False,
                  include_paths=[]):
    # accept None for include_paths too:
    if include_paths is None:
        include_paths= []
    # prepend the cwd to the list of search paths:
    include_paths.insert(0,"")
    # The initial block:
    external_definitions["__pyexpander_helper"] = globals()["__pyexpander_helper"]
    external_definitions["__pyexpander_helper2"]= globals()["__pyexpander_helper2"]
    block= Block(filename= filename, parse_list=parse_list,
                 external_definitions= external_definitions) 
    result= []
    while True:
        if not block.parse_loop():
            # no more data in the current block:
            if isinstance(block, IncludeBlock):
                # if current block is an IncludeBlock, go back to previous block:
                block= block.pop()
                continue
            elif isinstance(block, SubstBlock):
                # if current block is a SubstBlock, go back to previous block:
                block= block.pop()
                continue
            elif isinstance(block, PatternBlock):
                # if current block is a PatternBlock, go back to previous block:
                block= block.pop()
                continue
            else:
                # end of data, leave the loop:
                break
        # get the current parse element (base class: ParsedItem)
        tp= block.parse_elm()
        if isinstance(tp, EP.ParsedComment):
            # comments are ignored:
            continue
        if isinstance(tp, EP.ParsedLiteral):
            # literals are only taken if skip mode is off:
            if not block.skip:
                result.append(tp.string())
            continue
        if isinstance(tp, EP.ParsedVar):
            # if skip mode is off, insert the current value of the variable.
            # The current block can be used like a dict in order to get values
            # of variables:
            if not block.skip:
                result.append(str(block[tp.string()]))
            continue
        if isinstance(tp, EP.ParsedEval):
            # if skip mode is off, evaluate the eval expression,
            # convert it to a string and insert the result:
            if not block.skip:
                result.append(block.str_eval(tp.string()))
            continue
        if isinstance(tp, EP.ParsedCommand):
            # if ParsedItem is a ParsedCommand:
            if tp.ident=="py":
                # $py(...) : 
                # execute the string given within the brackets:
                if not block.skip:
                    block.exec_(tp.args())
            elif tp.ident=="include" or tp.ident=="include_begin":
                if not block.skip:
                    # $include(...) or $include_begin(...) :
                    # evaluate the filename of the file to include:
                    filename= block.str_eval(tp.args())
                    if not isinstance(filename, str):
                        raise EP.ParseException(
                                block._posmsg("filename must be a string at",
                                              pos=tp.start()))
                    # create an instance of an IncludeBlock:
                    block= IncludeBlock(previous= block,
                                        new_scope= (tp.ident=="include_begin"),
                                        filename= filename,
                                        include_paths= include_paths)
            elif tp.ident=="template":
                if not block.skip:
                    # $template(...) :
                    # evaluate the filename of substfile:
                    filename= block.str_eval(tp.args())
                    # remember this filename in the current block object:
                    block.set_substfile(filename)
            elif tp.ident=="subst":
                if not block.skip:
                    # $subst(...) :
                    # evaluate all the named arguments:
                    args= block.eval_("__pyexpander_helper(%s)" % tp.args())
                    # first argument may be the name of the substfile,
                    # this is undocumented and usually not used:
                    fn= args[0]
                    if fn is not None:
                        block.set_substfile(fn)
                    # create an instance of a SubstBlock:
                    block= SubstBlock(previous= block,
                                      filename= block.template,
                                      include_paths= include_paths,
                                      external_definitions= args[1])
            elif tp.ident=="pattern":
                if not block.skip:
                    # $pattern(...) :
                    # create a tuple of all arguments:
                    args= block.eval_("(%s)" % tp.args())
                    firstindex= 0
                    # first argument may be the name of the substfile,
                    # this is undocumented and usually not used:
                    if isinstance(args[0], str):
                        block.set_substfile(fn)
                        firstindex= 1
                    block= PatternBlock(previous= block,
                                        filename= block.template,
                                        include_paths= include_paths,
                                        heading= args[firstindex],
                                        lines= args[firstindex+1:])
            elif tp.ident=="default":
                if not block.skip:
                    # $default(...) :
                    # evaluate all the named arguments:
                    args= block.eval_("__pyexpander_helper2(%s)" % tp.args())
                    # set these defaults in the current block:
                    for (k,v) in args.items():
                        block.setdefault(k, v)
            elif tp.ident=="if":
                # $if(...) :
                # evaluate the condition:
                condition= block.eval_(tp.args())
                # create an instance of an IfBlock:
                block= IfBlock(previous= block, 
                               condition= condition)
            elif tp.ident=="elif":
                # elif(...) :
                # current block must be an IfBlock:
                if not isinstance(block,IfBlock):
                    raise EP.ParseException(
                            block._posmsg("unmatched elif at",
                                          pos=tp.start()))
                # evaluate the condition:
                condition= block.eval_(tp.args())
                # enter the "elif" part of the if block by 
                # calling enter_elif:
                block.enter_elif(condition)
            elif tp.ident=="for" or tp.ident=="for_begin":
                # $for(...) or $for_begin(...) :
                # assume the the parameters form a valid list comprehension
                try:
                    # try to parse the arguments of $for():
                    for_parts= EP.scanPyIn(tp.args())
                except EP.ParseException,e:
                    raise EP.ParseException(
                            block._posmsg("error in %s command at" % tp.ident,
                                          pos= tp.start()))
                # create a list of loop items by using pythons
                # list comprehension mechanism:
                for_list= block.eval_("[%s for %s in %s]" % \
                                     (for_parts[0],for_parts[0],for_parts[2]))
                # create an instance of a ForBlock:
                block= ForBlock(previous= block, 
                                new_scope= (tp.ident=="for_begin"), 
                                value_list= for_list,
                                var_expr= for_parts[0])
                block.set_loop_var()
            elif tp.ident=="while" or tp.ident=="while_begin":
                # $while(...) or $while_begin(...) :
                # create an instance of a WhileBlock:
                block= WhileBlock(previous= block,
                                  new_scope= (tp.ident=="while_begin"),
                                  while_expr= tp.args())
            elif tp.ident=="nonlocal":
                if not block.skip:
                    # $nonlocal(...) :
                    try:
                        # try to parse the arguments of $nonlocal():
                        identifiers= EP.scanPyIdentList(tp.args())
                    except EP.ParseException,e:
                        raise EP.ParseException(
                              block._posmsg("error in \"nonlocal\" command at",
                                            pos= tp.start()))
                    # mark them in the current block as exported symbols:
                    block.export_symbols(identifiers)
            elif tp.ident=="extend":
                if not block.skip:
                    # $extend(...) :
                    try:
                        # try to parse the arguments of $extend():
                        identifiers= EP.scanPyIdentList(tp.args())
                    except EP.ParseException,e:
                        raise EP.ParseException(
                              block._posmsg("error in \"extend\" command at",
                                            pos= tp.start()))
                    # mark them in the current block as "extended" identifiers:
                    block.extend(identifiers)
            elif tp.ident in block.direct_funcs:
                if not block.skip:
                    # $user-function-extended(...)
                    # apply the function directly:
                    result.append(block.str_eval("%s(%s)" % \
                                  (tp.ident,tp.args())))
            else:
                # everything else is a ParseException, this shouldn't happen
                # since all ParsedItem objects that the expanderparser can 
                # create should be handled here.
                raise EP.ParseException(
                        block._posmsg("unknown command \"%s\" at",
                                      pos= tp.start()))
            continue
        if isinstance(tp, EP.ParsedPureCommand):
            # a "pure" command, a command without arguments:
            ident= tp.string()
            if ident=="else":
                # $else :
                # current block must be an IfBlock:
                if not isinstance(block,IfBlock):
                    raise EP.ParseException(
                            block._posmsg("unmatched else at",
                                          pos= tp.start()))
                # enter the "else" part of the if block by 
                # calling enter_else:
                block.enter_else()
            elif ident=="endif":
                # $endif
                # current block must be an IfBlock:
                if not isinstance(block,IfBlock):
                    raise EP.ParseException(
                            block._posmsg("unmatched endif at",
                                          pos= tp.start()))
                # go back to previous block:
                block= block.pop()
            elif ident=="endfor":
                # $endfor
                # current block must be a ForBlock:
                if not isinstance(block,ForBlock):
                    raise EP.ParseException(
                            block._posmsg("unmatched endfor at",
                                          pos= tp.start()))
                # test if we have to perform the loop block again:
                if not block.next_loop():
                    # no further loops, go back to the previous block:
                    block= block.pop()
                else:
                    # further loops, give the loop variable a new value:
                    block.set_loop_var()
                    # go back to the position at the beginning of the block:
                    lst_pos= block.lst_pos
            elif ident=="endwhile":
                # $endwhile
                # current block must be a WhileBlock:
                if not isinstance(block,WhileBlock):
                    raise EP.ParseException(
                            block._posmsg("unmatched endwhile at",
                                          pos= tp.start()))
                # test if we have to loop again. next_loop also resets 
                # the position, if we have to loop again:
                if not block.next_loop():
                    # if loop condition is False, go back to previous block,
                    block= block.pop()
            elif ident=="begin":
                # $begin
                # create an instance of a BeginBlock:
                block= BeginBlock(previous= block)
            elif ident=="end":
                # $end
                # current block must be a BeginBlock:
                if not isinstance(block,BeginBlock):
                    raise EP.ParseException(
                            block._posmsg("unmatched end at",
                                          pos= tp.start()))
                # go back to previous block:
                block= block.pop()
            elif ident in block.direct_vars:
                # $user-variable-extended
                # if skip mode is off, insert the current value of the
                # variable. The current block can be used like a dict in order
                # to get values of variables:
                if not block.skip:
                    result.append(str(block.globals_[ident]))
            else:
                # if we are not in nobracket_vars mode, we have an
                # unknown command without arguments here:
                if not allow_nobracket_vars:
                    raise EP.ParseException(
                            block._posmsg("unknown command \"%s\" at" % ident,
                                          pos= tp.start()))
                else:
                    # if skip mode is off, insert the current value of the
                    # variable. The current block can be used like a dict in
                    # order to get values of variables:
                    if not block.skip:
                        result.append(str(block.globals_[ident]))
            continue
    if block.previous is not None:
        raise EP.ParseException(
              block._posmsg("unclosed block at"))
    return (result,block.globals_)

def processToPrint(parse_list, filename=None,
                   external_definitions={},
                   allow_nobracket_vars= False,
                   include_paths=[]):
    """returns the global dict."""
    # debug:
    # for elm in parse_list:
    #     print elm
    (result,exp_globals)= processToList(parse_list, filename, 
                                        external_definitions,
                                        allow_nobracket_vars,
                                        include_paths)
    print "".join(result),
    return exp_globals

def expand(st, filename=None,
           external_definitions={},
           allow_nobracket_vars= False,
           include_paths=[]):
    """returns the global dict."""
    return processToPrint(parseString(st), filename, 
                          external_definitions,
                          allow_nobracket_vars,
                          include_paths)

def expandFile(filename, 
               external_definitions={},
               allow_nobracket_vars= False,
               include_paths=[]):
    """returns the global dict."""
    return processToPrint(parseFile(filename), filename, 
                          external_definitions,
                          allow_nobracket_vars,
                          include_paths)

def _test():
    import doctest
    print "testing..."
    doctest.testmod()
    print "done"

if __name__ == "__main__":
    _test()
