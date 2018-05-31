using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DataLogic.Trace
{
    public class TraceActivity
    {
        public string Name { get; set; }
        public bool Strict { get; set; } = false;

        public TraceActivity(string name)
        {
            Name = name;
        }
    }
}
