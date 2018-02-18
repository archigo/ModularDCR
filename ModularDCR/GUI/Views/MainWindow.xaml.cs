using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using DataLogic.DcrGraph;
using GUI.Utils;
using GUI.Models;
using SharpVectors.Converters;
using SharpVectors.Renderers.Wpf;

namespace GUI.Views
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindowModel Model { get; set; }

        private DcrGraph _dcrGraph;

        public MainWindow()
        {
            InitializeComponent();
            Model = new MainWindowModel();
            this.DataContext = Model;

            Loaded += OnLoaded;
        }

        private void OnLoaded(object sender, RoutedEventArgs e)
        {
            DcrText.Text = Model.DcrText;
            //RebuildDcr(Model.DcrText);
        }

        private async void RebuildDcr(string rawDcr, bool buildInMemoryDcr = true)
        {
            var dcrGraph = _dcrGraph;
            try
            {
                var svg = await DcrToSvg.GetSvgFromDcr(rawDcr);

                var settings = new WpfDrawingSettings
                {
                    IncludeRuntime = true,
                    TextAsGeometry = true,
                    OptimizePath = true
                };

                var converter = new FileSvgReader(settings);

                var drawingGroup = converter.Read(new MemoryStream(Encoding.UTF8.GetBytes(svg)));

                Model.DcrImage = new DrawingImage(drawingGroup);

                if (buildInMemoryDcr)
                {
                    dcrGraph = new DcrGraph(rawDcr);
                }
            }
            catch
            {
                return;
            }
            _dcrGraph = dcrGraph;


            Model.Events.Clear();
            foreach (var activity in _dcrGraph.GetExecutableActivityNames())
            {
                Model.Events.Add(activity);
            }
        }

        private void DcrText_TextChanged(object sender, TextChangedEventArgs e)
        {
            RebuildDcr(DcrText.Text);
        }

        private void DcrEvent_OnMouseLeftButtonUp(object sender, MouseButtonEventArgs e)
        {
            var activity = sender?.GetType().GetProperty("DataContext")?.GetValue(sender, null) as string;
            if (_dcrGraph.ExecuteActivity(activity))
                RebuildDcr(_dcrGraph.ExportRawDcrString());
        }

        private void DcrGraphImage_OnMouseRightButtonUp(object sender, MouseButtonEventArgs e)
        {
            RebuildDcr(DcrText.Text, true);
        }
    }
}
