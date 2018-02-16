using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Media;
using GUI.Annotations;

namespace GUI.Models
{
    public class MainWindowModel : SuperModel
    {
        private DrawingImage _dcrImage;
        private string _dcrText;
        private ObservableCollection<string> _events = new ObservableCollection<string> { "ASD", "qwe"};

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
    }
}
