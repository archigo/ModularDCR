using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Windows.Media;
using DataLogic.Annotations;

namespace DataLogic.Trace
{
    public class Trace : INotifyPropertyChanged
    {
        private bool _status = true;
        public List<string> ActivitySequence { get; set; } = new List<string>();
        public DateTime Recorded { get; set; } = DateTime.Now;
        public string Name { get; set; }
        public bool TrackAccepting { get; set; }

        public bool Status
        {
            get => _status;
            set
            {
                _status = value;
                OnPropertyChanged(nameof(Status));
                OnPropertyChanged(nameof(BackgroundColor));
            }
        }

        public List<string> AcceptingTrace { get; set; } = new List<string>();
        public Context Context { get; set; }
        public bool Positive { get; set; }

        public string PositiveString => Positive ? "P" : "N";

        public Color BackgroundColor => Status ? Colors.Green : Colors.Red;

        public Trace(string name, ContextType type, bool strict, bool positive, bool trackAccepting)
        {
            Name = string.IsNullOrEmpty(name) ? DateTime.Now.ToString("g") : name;
            Context = new Context(strict, type);
            Positive = positive;
            TrackAccepting = trackAccepting;
        }

        public void RecordActivityExecution(string activity)
        {
            ActivitySequence.Add(activity);
            Context.AddToContext(activity);
        }

        public void AddToContext(string activity)
        {
            Context.AddToContext(activity);
        }

        public bool CheckTrace(DcrGraph.DcrGraph dcrGraph)
        {
            Status = false;
            var pass = new TraceChecker().CheckTrace(this, dcrGraph);
            if (TrackAccepting)
            {
                if ((Positive && pass) || (!Positive && !pass))
                {
                    Status = true;
                }
            }
            else
            {
                if ((Positive && pass) || (!Positive && !pass))
                {
                    Status = true;
                }
            }
            

            return Status;
        }

        public event PropertyChangedEventHandler PropertyChanged;

        [NotifyPropertyChangedInvocator]
        protected virtual void OnPropertyChanged([CallerMemberName] string propertyName = null)
        {
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }
    }
    
}