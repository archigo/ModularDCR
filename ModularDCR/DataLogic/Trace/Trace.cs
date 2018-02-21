using System;
using System.Collections.Generic;
using System.Linq;

namespace DataLogic.Trace
{
    public class Trace
    {
        public List<string> ActivitySequence { get; set; } = new List<string>();
        public DateTime Recorded { get; set; }
        public string Name { get; set; }
        public bool Status { get; set; } = true;
    }
}