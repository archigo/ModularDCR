using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DataLogic.Trace;

namespace DataLogic.DcrGraph
{
    public class GraphMerge
    {
        public List<DcrGraph> DcrGraphsToMerge { get; set; }

        public DcrGraph DcrGraphFullMerge { get; set; }
        public List<DcrGraph> DcrGraphEventLocalMerges { get; set; } = new List<DcrGraph>();
        public List<DcrGraph> OriginalDcrGraphs { get; set; }
        public Freedom Freedom { get; set; }
        public List<Tuple<Activity, Relation>> PreserveTests { get; set; } = new List<Tuple<Activity, Relation>>();
        /// <summary>
        /// Indexed same order as graphs in DcrGraphEventLocalMerges
        /// </summary>
        public List<List<Trace.Trace>> GraphTraces { get; set; }

        public List<Trace.Trace> TracesLocal
        {
            get
            {
                var traces = new List<Trace.Trace>();
                foreach (var graphMergeGraphTrace in GraphTraces)
                {
                    foreach (var trace in graphMergeGraphTrace)
                    {
                        if (trace.Context.ContextType == ContextType.Local) traces.Add(trace);
                    }
                }

                return traces;
            }
        }

        public List<Trace.Trace> TracesEventLocal
        {
            get
            {
                var traces = new List<Trace.Trace>();
                foreach (var graphMergeGraphTrace in GraphTraces)
                {
                    foreach (var trace in graphMergeGraphTrace)
                    {
                        if (trace.Context.ContextType == ContextType.EventLocal) traces.Add(trace);
                    }
                }

                return traces;
            }
        }

        public List<Trace.Trace> TracesGlobal
        {
            get
            {
                var traces = new List<Trace.Trace>();
                foreach (var graphMergeGraphTrace in GraphTraces)
                {
                    foreach (var trace in graphMergeGraphTrace)
                    {
                        if (trace.Context.ContextType == ContextType.Global) traces.Add(trace);
                    }
                }

                return traces;
            }
        }

        public GraphMerge(List<DcrGraph> dcrGraphsToMerge)
        {
            DcrGraphsToMerge = dcrGraphsToMerge;
            OriginalDcrGraphs = dcrGraphsToMerge;
            GraphTraces = new List<List<Trace.Trace>>();
            foreach (var dcrGraph in DcrGraphsToMerge)
            {
                GraphTraces.Add(dcrGraph.StoredTraces);
            }

            if (DcrGraphsToMerge == null || DcrGraphsToMerge.Count == 0)
            {
                return;
            }

            foreach (var dcrGraph in dcrGraphsToMerge)
            {
                dcrGraph.ResetGraphState();
            }

            CreateFullMerge();
            CreateEventLocalMerges();
            FreedomCheck();
            PreserveTestCheck();
        }


        private void FreedomCheck()
        {
            Freedom = new Freedom();
            var allGraph = new List<DcrGraph>();
            allGraph.AddRange(OriginalDcrGraphs);
            allGraph.Add(DcrGraphFullMerge);
            Freedom.CheckDeadlockLivelockFreedom(OriginalDcrGraphs);
        }
        private void PreserveTestCheck()
        {
            var safeGraph = OriginalDcrGraphs[0];
            for (int i = 0; i < OriginalDcrGraphs.Count; i++)
            {
                if (i + 1 >= OriginalDcrGraphs.Count) break;
                
                var next = OriginalDcrGraphs[i + 1];

                var tc = safeGraph.IsTransparent(next);
                var tn = next.IsTransparent(safeGraph);

                if (tc.Item1 == null && tc.Item2 == null && tn.Item1 == null && tn.Item2 == null)
                {
                    var rawDcr = safeGraph.EditWindowString + " \r\n " + next.EditWindowString;
                    safeGraph = new DcrGraph(rawDcr, new List<string>(), rawDcr, "MergedTestPreserved" );
                }
                else
                {
                    if(tc.Item1 != null || tc.Item2 != null) PreserveTests.Add(tc);
                    if(tn.Item1 != null || tn.Item2 != null) PreserveTests.Add(tn);
                }
            }
        }

        private void CreateFullMerge()
        {
            var fullMergeRawDcr = "";
            var allStrictActivities = new List<string>();
            foreach (var dcrGraph in DcrGraphsToMerge)
            {
                fullMergeRawDcr += dcrGraph.EditWindowString + " \r\n\r\n ";
                allStrictActivities.AddRange(dcrGraph.StrictActivities);
            }
            
            DcrGraphFullMerge = new DcrGraph(fullMergeRawDcr, allStrictActivities, fullMergeRawDcr, "Full merge graph");
        }

        private void CreateEventLocalMerges()
        {
            for (int i = 0; i < DcrGraphsToMerge.Count; i++)
            {
                PerformEventLocelMergeFromGraph(i);
            }
        }

        private void PerformEventLocelMergeFromGraph(int index)
        {
            var startGraph = new DcrGraph(DcrGraphsToMerge[index].EditWindowString, DcrGraphsToMerge[index].StrictActivities, DcrGraphsToMerge[index].EditWindowString, DcrGraphsToMerge[index].Name);
            startGraph.StoredTraces = DcrGraphsToMerge[index].StoredTraces;

            for (int i = 0; i < DcrGraphsToMerge.Count; i++)
            {
                if (i == index) continue;
                var graphToMerge = DcrGraphsToMerge[i];

                startGraph.TakeEventLocalActivities(graphToMerge);
            }

            DcrGraphEventLocalMerges.Add(startGraph);
        }

        public void TestAllTraces()
        {
            foreach (var trace in TracesGlobal)
            {
                trace.CheckTrace(DcrGraphFullMerge);
            }

            foreach (var trace in TracesEventLocal)
            {
                trace.CheckTrace(DcrGraphEventLocalMerges.First(x => x.StoredTraces.Any(y => y.Name.Equals(trace.Name))));
            }

            foreach (var trace in TracesLocal)
            {
                trace.CheckTrace(OriginalDcrGraphs.First(x => x.StoredTraces.Any(y => y.Name.Equals(trace.Name))));
            }
        }
    }
}
