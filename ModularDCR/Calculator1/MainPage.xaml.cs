using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Windows;
using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;

namespace Calculator1
{
    public partial class MainPage : Page
    {
        public MainPage()
        {
            this.InitializeComponent();

            PaperBox = new TextBlock();
            Paper = new PaperTrail(PaperBox);

            ProcessKey('0');
            EraseDisplay = true;
        }

        private TextBlock PaperBox;
        private PaperTrail Paper;

        private enum Operation
        {
            None,
            Devide,
            Multiply,
            Subtract,
            Add,
            Sqrt,
            OneX,
            Negate
        }
        private Operation LastOper;
        private string _display = "";
        private string _last_val;
        private bool _erasediplay;

        private bool EraseDisplay
        {
            get
            {
                return _erasediplay;

            }
            set
            {
                _erasediplay = value;
            }
        }
        private string LastValue
        {
            get
            {
                if (_last_val == string.Empty)
                    return "0";
                return _last_val;

            }
            set
            {
                _last_val = value;
            }
        }
        private string Display
        {
            get
            {
                return _display;
            }
            set
            {
                _display = value;
            }
        }

        private void DigitBtn_Click(object sender, RoutedEventArgs e)
        {
            string s = ((Button)sender).Content.ToString();

            //char[] ids = ((Button)sender).ID.ToCharArray();
            char[] ids = s.ToCharArray();
            ProcessKey(ids[0]);

        }
        private void ProcessKey(char c)
        {
            if (EraseDisplay)
            {
                Display = string.Empty;
                EraseDisplay = false;
            }
            AddToDisplay(c);
        }
        private void ProcessOperation(string s)
        {
            Double d = 0.0;
            if (s == "BPM")
            {
                LastOper = Operation.Negate;
                LastValue = Display;
                CalcResults();
                LastValue = Display;
                EraseDisplay = true;
                LastOper = Operation.None;
            }
            else if (s == "BDevide")
            {
                if (EraseDisplay)
                {
                    LastOper = Operation.Devide;
                    return;
                }
                CalcResults();
                LastOper = Operation.Devide;
                LastValue = Display;
                EraseDisplay = true;
            }
            else if (s == "BMultiply")
            {
                if (EraseDisplay)
                {
                    LastOper = Operation.Multiply;
                    return;
                }
                CalcResults();
                LastOper = Operation.Multiply;
                LastValue = Display;
                EraseDisplay = true;
            }
            else if (s == "BMinus")
            {
                if (EraseDisplay)
                {
                    LastOper = Operation.Subtract;
                    return;
                }
                CalcResults();
                LastOper = Operation.Subtract;
                LastValue = Display;
                EraseDisplay = true;
            }
            else if (s == "BPlus")
            {
                if (EraseDisplay)
                {
                    LastOper = Operation.Add;
                    return;
                }
                CalcResults();
                LastOper = Operation.Add;
                LastValue = Display;
                EraseDisplay = true;
            }
            else if (s == "BEqual")
            {
                if (EraseDisplay)
                    return;
                CalcResults();
                EraseDisplay = true;
                LastOper = Operation.None;
                LastValue = Display;
                //val = Display;
            }
            else if (s == "BSqrt")
            {
                LastOper = Operation.Sqrt;
                LastValue = Display;
                CalcResults();
                LastValue = Display;
                EraseDisplay = true;
                LastOper = Operation.None;
            }
            else if (s == "BOneOver")
            {
                LastOper = Operation.OneX;
                LastValue = Display;
                CalcResults();
                LastValue = Display;
                EraseDisplay = true;
                LastOper = Operation.None;
            }
            else if (s == "BC") //clear All
            {
                LastOper = Operation.None;
                Display = LastValue = string.Empty;
                Paper.Clear();
                UpdateDisplay();
            }
            else if (s == "BCE") //clear entry
            {
                LastOper = Operation.None;
                Display = LastValue;
                UpdateDisplay();
            }
        }

        private void OperBtn_Click(object sender, RoutedEventArgs e)
        {
            ProcessOperation(((Button)sender).Name.ToString());
        }


        private double Calc(Operation LastOper)
        {
            double d = 0.0;

            try
            {
                switch (LastOper)
                {
                    case Operation.Devide:
                        Paper.AddArguments(String.Format("{0}·{1}⁻¹", LastValue, Display));
                        d = (Convert.ToDouble(LastValue) / Convert.ToDouble(Display));
                        CheckResult(d);
                        Paper.AddResult(d.ToString());
                        break;
                    case Operation.Add:
                        Paper.AddArguments(String.Format("{0}+{1}", LastValue, Display));
                        d = Convert.ToDouble(LastValue) + Convert.ToDouble(Display);
                        CheckResult(d);
                        Paper.AddResult(d.ToString());
                        break;
                    case Operation.Multiply:
                        Paper.AddArguments(String.Format("{0}*{1}", LastValue, Display));
                        d = Convert.ToDouble(LastValue) * Convert.ToDouble(Display);
                        CheckResult(d);
                        Paper.AddResult(d.ToString());
                        break;
                    case Operation.Subtract:
                        Paper.AddArguments(String.Format("{0}-{1}", LastValue, Display));
                        d = Convert.ToDouble(LastValue) - Convert.ToDouble(Display);
                        CheckResult(d);
                        Paper.AddResult(d.ToString());
                        break;
                    case Operation.Sqrt:
                        Paper.AddArguments(String.Format("√{0}", LastValue));
                        d = Math.Sqrt(Convert.ToDouble(LastValue));
                        CheckResult(d);
                        Paper.AddResult(d.ToString());
                        break;
                    case Operation.OneX:
                        Paper.AddArguments(String.Format("1/{0}", LastValue));
                        d = 1.0F / Convert.ToDouble(LastValue);
                        CheckResult(d);
                        Paper.AddResult(d.ToString());
                        break;
                    case Operation.Negate:
                        d = Convert.ToDouble(LastValue) * (-1.0F);
                        break;
                }
            }
            catch (Exception e)
            {
                d = 0;
                //Paper.AddResult("Error");
                //MessageBox.Show("Operation cannot be perfomed: " + e.ToString());
                DisplayBox.Text = "Error";
            }

            return d;
        }
        private void CheckResult(double d)
        {
            if (Double.IsNaN(d)) // || Double.IsNegativeInfinity(d) || Double.IsPositiveInfinity(d))
                throw new Exception("Illegal value");
        }

        private void CalcResults()
        {
            double d;
            if (LastOper == Operation.None)
                return;

            d = Calc(LastOper);
            Display = d.ToString();


            UpdateDisplay();
        }

        private void UpdateDisplay()
        {
            if (String.IsNullOrEmpty(Display))
            {
                DisplayBox.Text = "0";
            }
            else
            {
                DisplayBox.Text = Display;
            }
        }
        private void AddToDisplay(char c)
        {
            if (c == '.')
            {
                if (Display.IndexOf('.', 0) >= 0)  //already exists
                    return;
                Display = Display + c;
            }
            else
            {
                if (c >= '0' && c <= '9')
                {
                    Display = Display + c;
                }
                else
                    if (c == '\b')  //backspace ?
                {
                    if (Display.Length <= 1)
                        Display = String.Empty;
                    else
                    {
                        int i = Display.Length;
                        Display = Display.Remove(i - 1, 1);  //remove last char 
                    }
                }

            }

            UpdateDisplay();

        }
    }
}
