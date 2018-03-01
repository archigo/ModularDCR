using System;
using System.Collections.Generic;
using System.Linq;

namespace DataLogic.Trace
{
    public class Trace
    {
        public List<string> ActivitySequence { get; set; } = new List<string>();
        public DateTime Recorded { get; set; } = DateTime.Now;
        public string Name { get; set; }
        public bool Status { get; set; } = true;
        public Context Context { get; set; }

        public Trace(string name, ContextType type)
        {
            Name = string.IsNullOrEmpty(name) ? DateTime.Now.ToString("g") : name;
            Context = new Context(type);
        }

        public void RecordActivityExecution(string activity)
        {
            ActivitySequence.Add(activity);
            if(Context.ContextType is ContextType.Defined)
                Context.ContextActivities.Add(activity);
        }

        public void AddToContext(string activity)
        {
            Context.ContextActivities.Add(activity);
        }
    }
}