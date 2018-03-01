using System;
using System.Collections.Generic;
using DataLogic.DcrGraph;

namespace DataLogic.Trace
{
    public class Context
    {
        public ContextType ContextType { get; set; }
        public HashSet<string> ContextActivities { get; set; }

        public Context(ContextType type)
        {
            switch (type)
            {
                case ContextType.All:
                    ContextActivities = null;
                    break;
                case ContextType.Defined:
                    ContextActivities = new HashSet<string>();
                    break;
                default:
                    throw new Exception("A contextType case was not handled");
            }
        }
    }
    public enum ContextType { All, Defined }
}