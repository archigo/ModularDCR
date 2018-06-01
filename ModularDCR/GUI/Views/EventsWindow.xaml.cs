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
using GUI.Models;
using Button = System.Windows.Controls.Button;

namespace GUI.Views
{
    /// <summary>
    /// Interaction logic for EventsWindow.xaml
    /// </summary>
    public partial class EventsWindow : Window
    {
        public event Action<string, string> AddEventToContext; 

        public EventsWindownModel Model { get; set; }
        public string SelectedActivity { get; set; }
        public EventsWindow(List<Activity> activities, List<string> traces)
        {
            
            Model = new EventsWindownModel(activities, traces);
            this.DataContext = Model;
            InitializeComponent();
        }

        private void AddEventToContext_Click(object sender, RoutedEventArgs e)
        {
            var w = new selectTrace(Model.Traces);
            w.TraceSelected += OnTraceSelected;
            var button = sender as Button;
            var datacontext = button.DataContext as Activity;
            SelectedActivity = datacontext.Id;
            w.ShowDialog();
        }

        private void OnTraceSelected(string s)
        {
            AddEventToContext?.Invoke(SelectedActivity,s);
        }
    }
}
