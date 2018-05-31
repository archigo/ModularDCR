using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DataLogic.DcrGraph;

namespace DataLogic.Trace
{
    public class TraceChecker
    {
        public static List<Activity> TraceExecutedSequence = new List<Activity>();
        public static List<string> ForbiddenActivities;
        public static List<Activity> CircleActivities;

        public Trace Trace { get; set; }
        public DcrGraph.DcrGraph DcrGraph { get; set; }
        public bool CheckTrace(Trace trace, DcrGraph.DcrGraph dcrGraph)
        {
            TraceExecutedSequence = new List<Activity>();
            CircleActivities = new List<Activity>();

            Trace = trace;
            Trace.AcceptingTrace = new List<string>();
            DcrGraph = dcrGraph;
            DcrGraph.ResetGraphState();

            ForbiddenActivities = new List<string>();

             if (Trace.Context.Strict)
            {
                foreach (var activityName in DcrGraph.GetActivityNames())
                {
                    ForbiddenActivities.Add(activityName);
                }
            }
            else
            {
                foreach (var contextContextActivity in Trace.Context.ContextActivities)
                {
                    ForbiddenActivities.Add(contextContextActivity.Name);
                }

                foreach (var dcrGraphStrictActivity in DcrGraph.StrictActivities)
                {
                    ForbiddenActivities.Add(dcrGraphStrictActivity);
                }
            }

            foreach (var activity in Trace.ActivitySequence)
            {
                var success = TryExecuteActivity(activity);
                if (!success) return false;
            }

            foreach (var activity in TraceExecutedSequence)
            {
                Trace.AcceptingTrace.Add(activity.Id);
            }

            return true;
        }

        private bool TryExecuteActivity(string stringActivity)
        {
            var activity = DcrGraph.GetActivityByName(stringActivity);
            if (activity == null) return false;
            return activity.TryExecute(true);
        }
    }
}
