using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;

namespace GUI.Views
{
    /// <summary>
    /// Interaction logic for selectTrace.xaml
    /// </summary>
    public partial class selectTrace : Window
    {
        public event Action<string> TraceSelected;
        public List<string> Traces { get; set; }
        public selectTrace(List<string> traces)
        {
            InitializeComponent();
            cbTrace.ItemsSource = traces;
            Traces = traces;
        }

        private void ButtonBase_OnClick(object sender, RoutedEventArgs e)
        {
            TraceSelected?.Invoke(Traces[cbTrace.SelectedIndex]);
            this.Close();
        }


    }
}
