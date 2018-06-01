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
    /// Interaction logic for FreedomWindow.xaml
    /// </summary>
    public partial class FreedomWindow : Window
    {

        public List<Tuple<string,string>> Deads { get; set; }
        public List<Tuple<string, string>> Lives { get; set; }
        public FreedomWindow(DataLogic.DcrGraph.Freedom freedom)
        {
            InitializeComponent();
            

            var deads = Deads =  new List<Tuple<string, string>>();
            var lives = Lives = new List<Tuple<string, string>>();

            foreach (var freedomDeadlock in freedom.Deadlocks)
            {
                var seq = "";
                foreach (var activity in freedomDeadlock.Item2)
                {
                    if (seq.Equals(""))
                    {
                        seq += activity.Id;
                        continue;
                    }

                    seq += " - " + activity.Id;
                }
                deads.Add(new Tuple<string, string>(freedomDeadlock.Item1.Name, seq));
            }

            foreach (var freedomLivelocks in freedom.Livelokcs)
            {
                var seq = "";
                foreach (var activity in freedomLivelocks.Item2)
                {
                    if (seq.Equals(""))
                    {
                        seq += activity.Id;
                        continue;
                    }

                    seq += " - " + activity.Id;
                }
                lives.Add(new Tuple<string, string>(freedomLivelocks.Item1.Name, seq));
            }


            DataGridDead.ItemsSource = deads;
            DataGridLive.ItemsSource = lives;
            
        }
    }
}
