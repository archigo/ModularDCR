using System;
using System.Collections.Generic;
using DataLogic.DcrGraph;

namespace DataLogic.Trace
{
    public class Context
    {
        public ContextType ContextType { get; set; }
        public List<TraceActivity> ContextActivities { get; set; }
        public bool Strict { get; set; }

        public Context(bool strict, ContextType type)
        {
            Strict = strict;
            switch (strict)
            {
                case true:
                    ContextActivities = null;
                    break;
                case false:
                    ContextActivities = new List<TraceActivity>();
                    break;
                default:
                    throw new Exception("A context strictness case was not handled");
            }

            ContextType = type;
        }

        public void AddToContext(string name)
        {
            if(ContextActivities != null && !ContextActivities.Exists(x => x.Name.Equals(name)))
                ContextActivities.Add(new TraceActivity(name));
        }
    }
    public enum ContextType { Local, EventLocal, Global }
}