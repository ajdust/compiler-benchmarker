using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using CsvHelper;

namespace CompilerBenchmarker
{
    static class UtilExtensions
    {
        public static string Join(this IEnumerable<string> s, string delimiter) =>
            string.Join(delimiter, s);
        
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

        public static List<T> ReadAllCsv<T>(this FileInfo file)
        {
            using var reader = new StreamReader("path\\to\\file.csv");
            using var csv = new CsvReader(reader, CultureInfo.InvariantCulture);
            return csv.GetRecords<T>().ToList();
        }
    }
}