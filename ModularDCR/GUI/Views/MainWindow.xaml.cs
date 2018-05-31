using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
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

            string name = PromptDialog.Prompt("Name your first dcrgraph", "Confirm");

            Model = new MainWindowModel();
            Model.Traces.Clear();
            Model.CurrentGraphName = name;
            Model.StoredDcrGraphs.Add(new DcrGraphContainer() { Name = Model.CurrentGraphName });
            this.DataContext = Model;

            Loaded += OnLoaded;

        }

        private void OnLoaded(object sender, RoutedEventArgs e)
        {
            DcrText.Text = Model.DcrText;
            //RebuildDcr(Model.DcrText);
        }

        private async void RebuildDcr(string rawDcr, string editWindowString, bool buildInMemoryDcr = true)
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
                    dcrGraph = new DcrGraph(rawDcr, dcrGraph?.StrictActivities, editWindowString, Model.CurrentGraphName);
                }
            }
            catch (Exception e)
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
            RebuildDcr(DcrText.Text, DcrText.Text);
            TestAll();
        }

        private void TestAll()
        {
            if (Model?.Traces is null) return;
            foreach (var trace in Model.Traces)
            {
                RebuildDcr(DcrText.Text, DcrText.Text);
                trace.CheckTrace(_dcrGraph);
            }
        }

        private void DcrEvent_OnMouseLeftButtonUp(object sender, MouseButtonEventArgs e)
        {
            var activity = sender?.GetType().GetProperty("DataContext")?.GetValue(sender, null) as string;
            if (_dcrGraph.ExecuteActivity(activity))
            {
                RebuildDcr(_dcrGraph.ExportRawDcrString(), DcrText.Text);
                if (_traceRecording)
                {
                    _currentlyRecordingTrace.RecordActivityExecution(activity);
                }
            }
        }

        private void DcrGraphImage_OnMouseRightButtonUp(object sender, MouseButtonEventArgs e)
        {
            RebuildDcr(DcrText.Text, DcrText.Text, true);
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
            RebuildDcr(DcrText.Text, DcrText.Text);
            _traceRecording = true;
            _currentlyRecordingTrace = new Trace(TraceOverlayTraceName.Text, (ContextType)TraceOverlayTraceContextType.SelectedIndex, TraceOverlayTraceStrictness.IsChecked != null && (bool)TraceOverlayTraceStrictness.IsChecked, TraceOverlayTracePositive.IsChecked != null && (bool)TraceOverlayTracePositive.IsChecked);

            TraceOverlay.Visibility = Visibility.Hidden;
        }

        private void DcrEvent_OnMouseRightButtonUp(object sender, MouseButtonEventArgs e)
        {
            if (_traceRecording)
            {
                var activity = sender?.GetType().GetProperty("DataContext")?.GetValue(sender, null) as string;

                //if (_currentlyRecordingTrace.ActivitySequence.Contains(activity) ||
                //    _currentlyRecordingTrace.Context.ContextActivities.Contains(activity))
                //    return;

                _currentlyRecordingTrace.AddToContext(activity);
            }
        }

        private void Trace_Click(object sender, MouseButtonEventArgs e)
        {
            RebuildDcr(DcrText.Text, DcrText.Text);
            var traceWindow = new TraceWindow(Model.Traces.First(x => x.Name.Equals(((Label)sender).Content)), _dcrGraph);
            traceWindow.ShowDialog();
        }

        private void TraceOverlayTraceNameOnKeyUp(object sender, KeyEventArgs keyEventArgs)
        {
            Console.WriteLine(Model.Traces.Count);
            if (Model.Traces.Count > 0)
                Console.WriteLine(Model.Traces.Select(x => x.Name).Aggregate((a, v) => a + Environment.NewLine + v));
            var text = (sender as TextBox).Text;

            if (string.IsNullOrWhiteSpace(text) || Model.Traces.Any(x => x.Name.Equals(text)) || Model.StoredDcrGraphs.Any(x => x.DcrGraph != null && x.DcrGraph.StoredTraces.Any(y => y.Name.Equals(text))))
                TraceOverlayDoneButton.IsEnabled = false;
            else
                TraceOverlayDoneButton.IsEnabled = true;
        }

        private void ShowEvents_Click(object sender, RoutedEventArgs e)
        {
            var eventsWindow = new EventsWindow(_dcrGraph.Activities);
            eventsWindow.ShowDialog();
        }

        private void SelectDcrGraph_Click(object sender, RoutedEventArgs e)
        {
            Button button = sender as Button;
            DcrGraphContainer dcrGraphContainer = button.DataContext as DcrGraphContainer;

            LoadNewGraph(dcrGraphContainer.Name);
        }

        private void StoreCurrentGraph()
        {
            var dcrGraph = new DcrGraph(DcrText.Text, _dcrGraph.StrictActivities, DcrText.Text, Model.CurrentGraphName);
            dcrGraph.StoredTraces = Model.Traces.ToList();
            Model.StoredDcrGraphs[Model.StoredDcrGraphs.IndexOf(Model.StoredDcrGraphs.First(x => x.Name.Equals(Model.CurrentGraphName)))].DcrGraph = dcrGraph;
        }

        private void LoadNewGraph(string dcrGraphName)
        {
            StoreCurrentGraph();
            var dcrGraph = Model.StoredDcrGraphs.FirstOrDefault(x => x.Name.Equals(dcrGraphName))?.DcrGraph ??
                           _dcrGraph;
            Model.CurrentGraphName = dcrGraph.Name;
            Model.Traces.Clear();
            Model.Traces = new ObservableCollection<Trace>(dcrGraph.StoredTraces);
            DcrText.Text = dcrGraph.EditWindowString;
            _dcrGraph = dcrGraph;
            RebuildDcr(DcrText.Text, DcrText.Text);
            TestAll();
        }

        private void AddGraph_Click(object sender, RoutedEventArgs e)
        {
            string name;
            do
            {
                name = PromptDialog.Prompt("Name your new dcrgraph", "Confirm");
            } while (Model.StoredDcrGraphs.Any(x => x.Name.Equals(name)));

            var dcrGraph = new DcrGraph("\"A new graph\"", null, "\"A new graph\"", name);
            Model.StoredDcrGraphs.Add(new DcrGraphContainer() { DcrGraph = dcrGraph, Name = name });

            LoadNewGraph(dcrGraph.Name);
        }

        private void Merge_Click(object sender, RoutedEventArgs e)
        {
            MergeGraph();
        }

        private void MergeGraph()
        {
            try
            {
                StoreCurrentGraph();
                var mergeWindow = new GraphMergeWindow(Model.StoredDcrGraphs.Select(x => x.DcrGraph).ToList());
                mergeWindow.ShowDialog();
            }
            catch (Exception e)
            {
                Console.WriteLine(e);
            }
        }
    }
}
