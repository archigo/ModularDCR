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
using SharpVectors.Converters;
using SharpVectors.Renderers.Wpf;
using GUI.Models;
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
        private SvgDocument _currentSvgDocument;

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

                _currentSvgDocument = SvgDocument.FromSvg<SvgDocument>(svg);

                WpfDrawingSettings settings = new WpfDrawingSettings();
                settings.IncludeRuntime = true;
                settings.TextAsGeometry = true;

                FileSvgReader converter = new FileSvgReader(settings);

                var xamlFile = converter.Read(new MemoryStream(Encoding.UTF8.GetBytes(svg)));

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
            Console.WriteLine(_clickPoint.ToString());
            if (_clickPoint.Equals(e.GetPosition(DcrGraphImage)))
            {
                foreach (var elm in _currentSvgDocument.Children[0].Children.Where(x => x.ID != null && x.ID.Contains("__e")))
                {
                    //var bounds = elm.
                    //if()
                }
            }
        }
    }
}
