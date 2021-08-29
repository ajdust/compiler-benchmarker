using System;

namespace CompilerBenchmarker
{
    struct TimingResult
    {
        public TimeSpan Elapsed;
        public int MaxResidentSetSizeKilobytes;

        public TimingResult(double elapsedSeconds, int maxResidentSetSizeKilobytes)
        {
            Elapsed = TimeSpan.FromSeconds(elapsedSeconds);
            MaxResidentSetSizeKilobytes = maxResidentSetSizeKilobytes;
        }

        public TimingResult(TimeSpan elapsed)
        {
            Elapsed = elapsed;
            MaxResidentSetSizeKilobytes = -1;
        }
    }
}