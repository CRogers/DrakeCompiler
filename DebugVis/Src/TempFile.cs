using System;
using System.IO;

namespace DebugVis.Src
{
    public sealed class TempFile : IDisposable
    {
        public TempFile() :
            this(Path.GetTempPath()) { }

        public TempFile(string directory)
        {
            Create(Path.Combine(directory, Path.GetRandomFileName()));
        }

        ~TempFile()
        {
            Delete();
        }

        public void Dispose()
        {
            Delete();
            GC.SuppressFinalize(this);
        }

        public string FilePath { get; private set; }

        private void Create(string path)
        {
            FilePath = path;
            using (File.Create(FilePath)) { };
        }

        private void Delete()
        {
            if (FilePath == null) return;
            File.Delete(FilePath);
            FilePath = null;
        }
    }
}