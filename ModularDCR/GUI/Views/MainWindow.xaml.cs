using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Input;
using System.Windows.Media;
using DataLogic.DcrGraph;
using DataLogic.Trace;
using GUI.Utils;
using GUI.Models;
using MaterialDesignThemes.Wpf;
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
        private bool _traceRecording = false;
        private Trace _currentlyRecordingTrace;

        public MainWindow()
        {
            InitializeComponent();
            Model = new MainWindowModel();
            Model.Traces.Clear();
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
            {
                RebuildDcr(_dcrGraph.ExportRawDcrString());
                if (_traceRecording)
                {
                    _currentlyRecordingTrace.RecordActivityExecution(activity);
                }
            }
        }

        private void DcrGraphImage_OnMouseRightButtonUp(object sender, MouseButtonEventArgs e)
        {
            RebuildDcr(DcrText.Text, true);
        }

        private void RecordTrace_Click(object sender, RoutedEventArgs e)
        {
            if (_traceRecording)
            {
                _traceRecording = false;
                DcrText.IsReadOnly = false;
                TraceSpinner.Spin = false;
                TraceSpinner.Visibility = Visibility.Hidden;
                RecordTrace.Content = "Trace";
                TraceList.IsEnabled = true;

                // No trace to record
                if (_currentlyRecordingTrace.ActivitySequence.Count == 0)
                {
                    _currentlyRecordingTrace = null;
                    return;
                }

                Model.Traces.Add(_currentlyRecordingTrace);
                _currentlyRecordingTrace = null;
            }
            else
            {
                TraceOverlay.Visibility = Visibility.Visible;
                DcrText.IsReadOnly = true;
                TraceList.IsEnabled = false;
                TraceSpinner.Spin = true;
                TraceSpinner.Visibility = Visibility.Visible;
                RecordTrace.Content = "Finish";
            }
        }

        private void ButtonTraceOverlayDone_Click(object sender, RoutedEventArgs e)
        {

            _traceRecording = true;
            _currentlyRecordingTrace = new Trace(TraceOverlayTraceName.Text, (ContextType)TraceOverlayTraceContextType.SelectedIndex);

            TraceOverlay.Visibility = Visibility.Hidden;
        }

        private void DcrEvent_OnMouseRightButtonUp(object sender, MouseButtonEventArgs e)
        {
            if (_traceRecording)
            {
                var activity = sender?.GetType().GetProperty("DataContext")?.GetValue(sender, null) as string;

                if (_currentlyRecordingTrace.ActivitySequence.Contains(activity) ||
                    _currentlyRecordingTrace.Context.ContextActivities.Contains(activity))
                    return;

                _currentlyRecordingTrace.AddToContext(activity);
            }
        }

        private void Trace_Click(object sender, MouseButtonEventArgs e)
        {
            var traceWindow = new TraceWindow(Model.Traces.First());
            traceWindow.ShowDialog();
        }

        private void TraceOverlayTraceNameOnKeyUp(object sender, KeyEventArgs keyEventArgs)
        {
            Console.WriteLine(Model.Traces.Count);
            if (Model.Traces.Count > 0)
                Console.WriteLine(Model.Traces.Select(x => x.Name).Aggregate((a, v) => a + Environment.NewLine + v));
            var text = (sender as TextBox).Text;
            if (string.IsNullOrWhiteSpace(text) || Model.Traces.Any(x => x.Name.Equals(text)))
                TraceOverlayDoneButton.IsEnabled = false;
            else
                TraceOverlayDoneButton.IsEnabled = true;
        }

    }
}
