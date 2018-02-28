using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Windows.UI.Xaml.Controls;

namespace Calculator1
{
    class PaperTrail
    {
        string args;

        private TextBlock PaperBox;

        public PaperTrail()
        {
            //PaperBox.IsEnabled = false;
        }

        public PaperTrail(TextBlock paperBox)
        {
            PaperBox = paperBox;
        }

        public void AddArguments(string a)
        {
            args = a;
        }

        public void AddResult(string r)
        {
            PaperBox.Text += String.Format("{0}={1}{2}", args, r, Environment.NewLine);
        }

        public void Clear()
        {
            PaperBox.Text = args = string.Empty;
        }

    }
}
