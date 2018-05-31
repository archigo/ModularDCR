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
using DataLogic.DcrGraph;
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
        public DcrGraph DcrGraph { get; set; }
        public TraceWindow(Trace trace, DcrGraph dcrGraph)
        {
            InitializeComponent();
            DcrGraph = dcrGraph;
            Model = new TraceWindowModel(trace);
            this.DataContext = Model;
            this.Title = Model.Trace.Name;
            var res = "";
            foreach (var activity in Model.Trace.ActivitySequence)
            {
                res += "\"" + activity + "\" ";
            }
            this.TraceSequenceText.Text = res;
            ContextStrictness.IsChecked = Model.Trace.Context.Strict;
        }

        private void TestTrace_Click(object sender, RoutedEventArgs e)
        {
            TestResult.Visibility = Visibility.Hidden;
            
            if (Model.Trace.CheckTrace(DcrGraph))
            {
                TestResult.Foreground = new SolidColorBrush(Colors.Green);
                var res = "";
                foreach (var activity in Model.Trace.AcceptingTrace)
                {
                    res += "\"" + activity + "\" ";
                }
                TestResult.Text = res;
            }
            else
            {
                TestResult.Foreground = new SolidColorBrush(Colors.Red);
                TestResult.Text = "Trace failed";
            }
            TestResult.Visibility = Visibility.Visible;
        }
    }
}
