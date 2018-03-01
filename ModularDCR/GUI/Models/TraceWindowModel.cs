using DataLogic.Trace;

namespace GUI.Models
{
    public class TraceWindowModel
    {
        public Trace Trace { get; set; }

        public TraceWindowModel(Trace trace)
        {
            Trace = trace;
        }
    }
}