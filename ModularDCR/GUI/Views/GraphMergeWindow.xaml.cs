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
using System.Windows.Shapes;
using DataLogic.DcrGraph;
using DataLogic.Trace;
using GUI.Models;
using GUI.Utils;
using SharpVectors.Converters;
using SharpVectors.Renderers.Wpf;
using ThicknessConverter = Xceed.Wpf.DataGrid.Converters.ThicknessConverter;

namespace GUI.Views
{
    /// <summary>
    /// Interaction logic for GraphMergeWindow.xaml
    /// </summary>
    public partial class GraphMergeWindow : Window
    {
        private DcrGraph _dcrGraph;
        public GraphMergeWindowModel Model { get; set; }

        public GraphMergeWindow(List<DcrGraph> dcrGraphs)
        {
            InitializeComponent();
            Model = new GraphMergeWindowModel(dcrGraphs);
            this.DataContext = Model;
            RebuildDcr(Model.GraphMerge.DcrGraphFullMerge.EditWindowString, Model.GraphMerge.DcrGraphFullMerge.EditWindowString, Model.GraphMerge.DcrGraphFullMerge.Name);
            Model.GraphMerge.TestAllTraces();
            Model.SimulateOnPropertyChanged();
            
            this.DataContext = null;
            this.DataContext = Model;
        }

        private async void RebuildDcr(string rawDcr, string editWindowString, string name = "Full merge graph", bool buildInMemoryDcr = true)
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
                    dcrGraph = new DcrGraph(rawDcr, dcrGraph?.StrictActivities, editWindowString, name);
                }
            }
            catch (Exception e)
            {
                return;
            }
            _dcrGraph = dcrGraph;

            //Used if need to be able to execute
            //Model.Events.Clear();
            //foreach (var activity in _dcrGraph.GetExecutableActivityNames())
            //{
            //    Model.Events.Add(activity);
            //}
        }

        private void DcrGraphImage_OnMouseRightButtonUp(object sender, MouseButtonEventArgs e)
        {
            RebuildDcr(Model.GraphMerge.DcrGraphFullMerge.EditWindowString, Model.GraphMerge.DcrGraphFullMerge.EditWindowString, Model.GraphMerge.DcrGraphFullMerge.Name);
        }

        private void Trace_Click(object sender, MouseButtonEventArgs e)
        {
            var lbl = sender as Label;
            string name = (string) lbl?.Content;
            RebuildDcr(_dcrGraph.EditWindowString, _dcrGraph.EditWindowString);
            var trace = Model.GraphMerge.GraphTraces.First(x => x.Any(y => y.Name.Equals(name)))
                .First(x => x.Name.Equals(name));
            DcrGraph dcrGraph;

            if (trace.Context.ContextType == ContextType.Global) dcrGraph = _dcrGraph;
            else if (trace.Context.ContextType == ContextType.Local)
                dcrGraph = Model.GraphMerge.OriginalDcrGraphs.First(x =>
                    x.StoredTraces.Any(y => y.Name.Equals(trace.Name)));
            else dcrGraph = Model.GraphMerge.DcrGraphEventLocalMerges.First(x =>
                    x.StoredTraces.Any(y => y.Name.Equals(trace.Name)));
            var traceWindow = new TraceWindow(trace, dcrGraph);
            traceWindow.ShowDialog();
        }

        private void TraceOverlayTraceNameOnKeyUp(object sender, KeyEventArgs e)
        {
            throw new NotImplementedException();
        }

        private void ButtonTraceOverlayDone_Click(object sender, RoutedEventArgs e)
        {
            throw new NotImplementedException();
        }

        private void ShowPsrtialMerge_Click(object sender, RoutedEventArgs e)
        {
            RebuildDcr(Model.GraphMerge.Freedom.RelatedDcrGraph.EditWindowString, Model.GraphMerge.Freedom.RelatedDcrGraph.EditWindowString, "Partial Merge");
        }

        private void ShowPreservationResults_Click(object sender, RoutedEventArgs e)
        {
            var window = new PreservationResultsWindow(Model.GraphMerge.PreserveTests);
            window.ShowDialog();
        }

        private void ShowFreedom_Click(object sender, RoutedEventArgs e)
        {
            var window = new PreservationResultsWindow(Model.GraphMerge.Freedom);
            window.ShowDialog();
        }
    }
}
