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
using DataLogic.Trace;
using GUI.Models;

namespace GUI.Views
{
    /// <summary>
    /// Interaction logic for TraceWindow.xaml
    /// </summary>
    public partial class TraceWindow : Window
    {
        public TraceWindowModel Model { get; set; }
        public TraceWindow(Trace trace)
        {
            InitializeComponent();
            Model = new TraceWindowModel(trace);
            this.DataContext = Model;

        }
    }
}
