using System;
using System.Collections.Generic;
using System.IO;
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
using System.Windows.Navigation;
using System.Windows.Shapes;
using GUI.Utils;
using GUI.Models;
using SharpVectors.Converters;
using SharpVectors.Renderers.Wpf;
using Svg;

namespace GUI.Views
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindowModel Model { get; set; }

        private Point _clickPoint;

        private Transform qwe;

        public MainWindow()
        {
            InitializeComponent();
            Model = new MainWindowModel();
            this.DataContext = Model;

            Loaded += Updateimage;
        }

        private async void Updateimage(object sender, RoutedEventArgs e)
        {
            try
            {
                var svg = await DcrToSvg.GetSvgFromDcr(DcrText.Text);
                

                var svgDoc = SvgDocument.FromSvg<SvgDocument>(svg);

                Model.Events.Clear();
                foreach (var activity in svgDoc.Children[0].Children.Where(x => x.ID != null && x.ID.Contains("__e")))
                {
                    Model.Events.Add(activity.ID.Replace("__e", ""));
                }

                WpfDrawingSettings settings = new WpfDrawingSettings();
                settings.IncludeRuntime = true;
                settings.TextAsGeometry = true;
                settings.OptimizePath = true;

                FileSvgReader converter = new FileSvgReader(settings);

                var xamlFile = converter.Read(new MemoryStream(Encoding.UTF8.GetBytes(svg)));

                qwe = ((xamlFile.Children[0] as DrawingGroup).Children[0] as DrawingGroup).Transform;
                

                Model.DcrImage = new DrawingImage(xamlFile);
            }
            catch
            {
            }
        }

        private void DcrText_TextChanged(object sender, TextChangedEventArgs e)
        {
            // todo validate input

            Updateimage(null, null);

        }

        private void DcrGraphImage_OnMouseDown(object sender, MouseButtonEventArgs e)
        {
            _clickPoint = e.GetPosition(DcrGraphImage);
        }

        private void DcrGraphImage_OnMouseUp(object sender, MouseButtonEventArgs e)
        {
            var p = qwe.Inverse.Transform(_clickPoint);
            p.X += -14.4;
            p.Y += -14.4;
            Console.WriteLine(p.ToString());
            //if (_clickPoint.Equals(e.GetPosition(DcrGraphImage)))
            //{
            //    foreach (var elm in _currentSvgDocument.Children[0].Children.Where(x => x.ID != null && x.ID.Contains("__e")))
            //    {
            //        //var bounds = elm.
            //        //if()
            //    }
            //}
        }

        private void DcrEvent_OnMouseLeftButtonUp(object sender, MouseButtonEventArgs e)
        {
            var stop = 1;
        }
    }
}
