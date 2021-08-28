
using System;
using System.Diagnostics;
using System.Runtime.InteropServices;
using System.Text;

namespace CompilerBenchmarker
{
    class BasicSystemInfo
    {
        private static string WaitAndRead(string exe, string args)
        {
            using (var p = new Process())
            {
                p.StartInfo.FileName = exe;
                p.StartInfo.Arguments = args;
                p.StartInfo.UseShellExecute = false;
                p.StartInfo.RedirectStandardOutput = true;
                p.StartInfo.RedirectStandardError = true;
                p.Start();

                var sout = p.StandardOutput.ReadToEnd();
                return string.IsNullOrWhiteSpace(sout)
                    ? $"No standard output found for `{exe} {args}`"
                    : sout;
            }
        }

        public static BasicSystemInfo Find()
        {
            string versCmd = "", versCmdArg = "", cpuCmd = "", cpuCmdArg = "", memCmd = "", memCmdArg = "";
            if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
            {
                versCmd = "wmic";
                versCmdArg = "os get Caption";
                cpuCmd = "wmic";
                cpuCmdArg = "cpu";
                memCmd = "wmic";
                memCmdArg = "computersystem get TotalPhysicalMemory";
            }
            else if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux))
            {
                versCmd = "uname";
                versCmdArg = "-r";
                cpuCmd = "lscpu";
                memCmd = "free";
                memCmdArg = "-m";
            }
            else if (RuntimeInformation.IsOSPlatform(OSPlatform.OSX))
            {
                versCmd = "sw_vers";
                cpuCmd = "sysctl";
                cpuCmdArg = "-n machdep.cpu.brand_string";
                memCmd = "vm_stat";
            }
            else
            {
                return new BasicSystemInfo
                {
                    OS = "Unknown",
                    CPU = "Unknown",
                    Memory = "Unknown"
                };
            }

            var sep = "\n-----------------------------\n";
            var vers = $"\n{sep}{versCmd} {versCmdArg}{sep}" + WaitAndRead(versCmd, versCmdArg);
            var cpu = $"\n{sep}{cpuCmd} {cpuCmdArg}{sep}" + WaitAndRead(cpuCmd, cpuCmdArg);
            var mem = $"\n{sep}{memCmd} {memCmdArg}{sep}" + WaitAndRead(memCmd, memCmdArg);
            return new BasicSystemInfo
            {
                OS = vers,
                CPU = cpu,
                Memory = mem
            };
        }

        private BasicSystemInfo() {}

        public string OS {get;set;}
        public string CPU {get;set;}
        public string Memory {get;set;}
    }
}