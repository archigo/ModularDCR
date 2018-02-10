using System;
using System.Collections.Generic;
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
    }
}
