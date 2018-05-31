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

        public List<Activity> Activities
        {
            get { return _activities; }
            set
            {
                _activities = value;
                OnPropertyChanged(nameof(Activities));
            }
        }

        public EventsWindownModel(List<Activity> activities)
        {
            Activities = activities;
        }
    }
}
