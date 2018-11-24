using System;
using System.Collections.Generic;
using System.IO;

namespace CompilerBenchmarker
{
    static class FileWalker
    {
        public static IEnumerable<FileInfo> WalkFiles(this DirectoryInfo root)
        {
            foreach (var dir in root.GetDirectories())
            {
                foreach (var file in dir.WalkFiles())
                {
                    yield return file;
                }
            }

            foreach (var file in root.GetFiles())
            {
                yield return file;
            }
        }
    }
}