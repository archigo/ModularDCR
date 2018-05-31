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

namespace GUI.Views
{
    /// <summary>
    /// Interaction logic for PreservationResultsWindow.xaml
    /// </summary>
    public partial class PreservationResultsWindow : Window
    {
        public PreservationResultsWindow(List<Tuple<Activity, Relation>> preservationRes)
        {
            InitializeComponent();

            BuildTextPreservation(preservationRes);
        }

        private void BuildTextPreservation(List<Tuple<Activity, Relation>> preservationRes)
        {
            ResultTest.Text = "";
            foreach (var res in preservationRes)
            {
                ResultTest.Text += "\r\n";
                ResultTest.Text += res.Item1?.Id + "---- From:" + res.Item2?.From?.Id +  " " + res.Item2?.Type.ToString() + " " + res.Item2?.To?.Id;
            }
        }

        public PreservationResultsWindow(Freedom freedom)
        {
            InitializeComponent();

            BuildFreedomResults(freedom);
        }

        private void BuildFreedomResults(Freedom freedom)
        {
            ResultTest.Text = "Deadlocks:\r\n";
            foreach (var freedomDeadlock in freedom.Deadlocks)
            {
                ResultTest.Text += "Graph: " + freedomDeadlock.Item1.Name + " ----- ";
                if (freedomDeadlock.Item2 != null)
                {
                    foreach (var activity in freedomDeadlock.Item2)
                    {
                        ResultTest.Text += activity.Id + " -- ";
                    }
                }

                ResultTest.Text += "\r\n";
            }

            ResultTest.Text = "Livelocks:\r\n";
            foreach (var freedomLivelock in freedom.Livelokcs)
            {
                ResultTest.Text += "Graph: " + freedomLivelock.Item1.Name + " ----- ";
                if (freedomLivelock.Item2 != null)
                {
                    foreach (var activity in freedomLivelock.Item2)
                    {
                        ResultTest.Text += activity.Id + " -- ";
                    }
                }

                ResultTest.Text += "\r\n";
            }
        }
    }
}
