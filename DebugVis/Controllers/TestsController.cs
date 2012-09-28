using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Web.Http;

namespace DebugVis.Controllers
{
    public class TestsController : ApiController
    {
        private const string testLoc = "C:\\Dropbox\\Programming\\Visual Studio 2012\\Projects\\BasicCompiler\\tests";

        // GET api/tests
        public IEnumerable<string> Get()
        {
            return new DirectoryInfo(testLoc).GetFiles().Select(fi => fi.Name);
        }

        // GET api/tests/5
        public string Get(string id)
        {
            string fileLoc = Path.Combine(testLoc, id);

            // Check this is actually inside the testLoc
            var fi = new FileInfo(fileLoc);
            if (fi.DirectoryName != testLoc) {
                throw new IOException("Can only load tests!");
            }

            return File.ReadAllText(fileLoc);
        }

        // POST api/tests
        public void Post([FromBody]string value)
        {
        }

        // PUT api/tests/5
        public void Put(int id, [FromBody]string value)
        {
        }

        // DELETE api/tests/5
        public void Delete(int id)
        {
        }
    }
}
