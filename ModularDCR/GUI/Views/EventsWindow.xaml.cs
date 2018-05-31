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

namespace GUI.Views
{
    /// <summary>
    /// Interaction logic for EventsWindow.xaml
    /// </summary>
    public partial class EventsWindow : Window
    {

        public EventsWindownModel Model { get; set; }
        public EventsWindow(List<Activity> activities)
        {
            InitializeComponent();
            Model = new EventsWindownModel(activities);
            this.DataContext = Model;
        }
    }
}
