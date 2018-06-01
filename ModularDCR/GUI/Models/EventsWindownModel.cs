using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DataLogic.DcrGraph;

namespace GUI.Models
{
    public class EventsWindownModel : SuperModel
    {
        private List<Activity> _activities;
        private List<string> _traces;

        public List<Activity> Activities
        {
            get => _activities;
            set
            {
                _activities = value;
                OnPropertyChanged(nameof(Activities));
            }
        }

        public List<string> Traces
        {
            get => _traces;
            set
            {
                _traces = value;
                OnPropertyChanged();
            }
        }

        public EventsWindownModel(List<Activity> activities, List<string> traces)
        {
            Activities = activities;
            Traces = traces;
        }
    }
}
