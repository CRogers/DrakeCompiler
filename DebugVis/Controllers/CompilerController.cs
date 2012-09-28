using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Web.Http;
using DebugVis.Src;
using Newtonsoft.Json;

namespace DebugVis.Controllers
{
    public class CompilerController : ApiController
    {
        public class CodeParams
        {
            public String code { get; set; }
        }

        // GET api/compiler
        public IEnumerable<string> Get()
        {
            return new string[] { "value1", "value2" };
        }

        // GET api/compiler/5
        public string Get(string id)
        {
            return "compiler";
        }

        // POST api/compiler
        public string Post(CodeParams value)
        {
            var code = value.code;

            // Lex it
            var lex = Compiler.lexText(code);
            var parse = Compiler.parseText(code);
            var parseText = Print.fmt(parse);

            // compile to llvm
            var module = Compiler.compile(code);
            string llText = null;
            using (var tmpFile = new TempFile())
            {
                Compiler.writeModuleToFile(tmpFile.FilePath, module);
                var llOut = tmpFile.FilePath + ".ll";
                var proc = Process.Start("llvm-dis", string.Format("-o {0} {1}", llOut, tmpFile.FilePath));
                proc.WaitForExit();
                llText = File.ReadAllText(llOut);
                File.Delete(llOut);
            }

            return JsonConvert.SerializeObject(new
            {
                lexer = lex,
                parser = parseText,
                llvm = llText
            });
        }

        // PUT api/compiler/5
        public void Put(int id, [FromBody]string value)
        {
        }

        // DELETE api/compiler/5
        public void Delete(int id)
        {
        }
    }
}