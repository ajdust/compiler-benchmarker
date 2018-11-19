
using System;
using System.Collections.Generic;
using System.IO;

namespace CompilerBenchmarker
{
    static class FileWalker
    {
        public static Action<FileInfo> OnFileDoNothing => dirInfo => {};
        public static Action<DirectoryInfo> OnDirDoNothing => dirInfo => {};

        public static void Walk(
            string root, Action<FileInfo> onFileDo, Action<DirectoryInfo> onDirDo, bool recursive = false)
        {
            Walk(new DirectoryInfo(root), onFileDo, onDirDo, recursive);
        }

        public static void Walk(
            DirectoryInfo root, Action<FileInfo> onFileDo, Action<DirectoryInfo> onDirDo, bool recursive = false)
        {
            if (recursive)
            {
                foreach (var dir in root.GetDirectories())
                    Walk(dir, onFileDo, onDirDo, recursive);
            }

            foreach (var file in root.GetFiles())
                onFileDo(file);
        }
    }
}