using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Media;
using DataLogic.Trace;
using GUI.Annotations;

namespace GUI.Models
{
    public class MainWindowModel : SuperModel
    {
        private DrawingImage _dcrImage;
        private string _dcrText = "\"Some Activity\" -->* \"Some Other Activity\"";
        private ObservableCollection<string> _events = new ObservableCollection<string> { "Some Activity", "Some Other Activity"};
        private ObservableCollection<Trace> _traces = new ObservableCollection<Trace>(){ new Trace(){ActivitySequence = new List<string>(), Name = "Test", Recorded = DateTime.Now, Status = true}};

        public ObservableCollection<Trace> Traces
        {
            get => _traces;
            set
            {
                _traces = value;
                OnPropertyChanged(nameof(Traces));
            }
        }

        public DrawingImage DcrImage
        {
            get => _dcrImage;
            set
            {
                _dcrImage = value;
                OnPropertyChanged(nameof(DcrImage));
            }
        }

        public string DcrText
        {
            get => _dcrText;
            set
            {
                _dcrText = value;
                OnPropertyChanged(nameof(DcrText));
            }
        }

        public ObservableCollection<string> Events
        {
            get => _events;
            set
            {
                _events = value;
                OnPropertyChanged(nameof(Events));
            }
        }

        public string DcrExplanation => "Include -->+ : Exclude -->% : Response *--> : Condition -->* : Milestone --<>";
    }
}
