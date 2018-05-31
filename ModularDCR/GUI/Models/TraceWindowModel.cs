using DataLogic.Trace;

namespace GUI.Models
{
    public sealed class TraceWindowModel : SuperModel
    {
        public Trace Trace { get; set; }

        public TraceWindowModel(Trace trace)
        {
            Trace = trace;
            OnPropertyChanged(nameof(Trace));
        }
    }
}