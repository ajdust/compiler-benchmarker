using System.Collections.Generic;

namespace CompilerBenchmarker
{
    static class StringExtensions
    {
        public static string Join(this IEnumerable<string> s, string delimiter) =>
            string.Join(delimiter, s);
    }
}