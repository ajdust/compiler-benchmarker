
using System;
using System.Diagnostics;
using System.Runtime.InteropServices;
using System.Text;

class BasicSystemInfo
{
    private static string waitAndRead(string exe, string args)
    {
        using (var p = new Process())
        {
            var sout = new StringBuilder();
            var serr = new StringBuilder();

            p.StartInfo.FileName = exe;
            p.StartInfo.Arguments = args;
            p.StartInfo.UseShellExecute = false;
            p.StartInfo.RedirectStandardOutput = true;
            p.StartInfo.RedirectStandardError = true;
            p.StartInfo.ErrorDialog = false;
            p.OutputDataReceived += (sender, outputLine) => { if (outputLine.Data != null) sout.Append(outputLine.Data + "\n"); };
            p.ErrorDataReceived += (sender, errorLine) => { if (errorLine.Data != null) serr.Append(errorLine.Data + "\n"); };
            p.Start();
            p.BeginOutputReadLine();
            p.BeginErrorReadLine();

            if (!p.WaitForExit(10000))
            {
                p.Kill();
                return "Timed out after 10 seconds";
            }

            return p.ExitCode == 0 ? sout.ToString() : serr.ToString();
        }
    }

    public static BasicSystemInfo Find()
    {
        string versCmd = "", versCmdArg = "", cpuCmd = "", cpuCmdArg = "", memCmd = "", memCmdArg = "";
        if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
        {
            versCmd = "ver";
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
        var vers = $"\n{sep}{versCmd} {versCmdArg}{sep}" + waitAndRead(versCmd, versCmdArg);
        var cpu = $"\n{sep}{cpuCmd} {cpuCmdArg}{sep}" + waitAndRead(cpuCmd, cpuCmdArg);
        var mem = $"\n{sep}{memCmd} {memCmdArg}{sep}" + waitAndRead(memCmd, memCmdArg);
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