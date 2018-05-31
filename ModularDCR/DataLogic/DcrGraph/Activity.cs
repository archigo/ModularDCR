using System;
using System.CodeDom;
using System.Collections.Generic;
using System.Data.Entity.Core.Metadata.Edm;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DataLogic.Trace;

namespace DataLogic.DcrGraph
{
    public class Activity
    {
        public string Id { get; set; }
        public bool Executed { get; set; }
        public bool Pending { get; set; }
        public bool Excluded { get; set; }
        internal List<Relation> _relationsIncoming;
        internal List<Relation> _relationsOutgoing;
        public bool Strict { get; set; } = false;

        public int ExecutionCount { get; set; }

        public Activity(int executed, int included, int pending, string id)
        {
            if(!CheckZeroOrOne(executed) || !CheckZeroOrOne(included) || !CheckZeroOrOne(pending))
                throw new Exception($"An activity was created with invalid parameters for one or more of: Executed, Excluded and Pending. {executed}, {included}, {pending}. Must all be either 0 or 1.");

            Executed = executed != 0;
            Excluded = included == 0; // Inverted because it is more normal to be included than excluded (imho)
            Pending = pending != 0;

            ExecutionCount = Executed ? 1 : 0;

            Id = id;
            
        }

        public void ResetStates(ActivityStateHolder activityStateHolder)
        {
            Executed = activityStateHolder.Executed;
            Pending = activityStateHolder.Pending;
            Excluded = activityStateHolder.Excluded;
            ExecutionCount = 0;
        }

        /// <summary>
        /// Must only be called once, as part of the graph creation.
        /// </summary>
        /// <param name="relationsIncoming"></param>
        /// <param name="relationsOutgoing"></param>
        public void SetRelations(List<Relation> relationsIncoming, List<Relation> relationsOutgoing)
        {
            _relationsIncoming = relationsIncoming;
            _relationsOutgoing = relationsOutgoing;
        }

        public bool IsExecuteable()
        {
            if (Excluded) return false;

            foreach (var relation in _relationsIncoming)
            {
                if (relation.From.IsBlockingState(relation.Type)) return false;
            }

            return true;
        }

        public bool Execute()
        {
            if (!IsExecuteable()) return false;
            TraceChecker.TraceExecutedSequence.Add(this);
            Executed = true;
            ExecutionCount++;

            Pending = false;

            foreach (var relation in _relationsOutgoing)
            {
                relation.To.ChangeState(relation.Type);
            }

            return Executed;
        }

        public bool TryExecute(bool outside = false)
        {
            if (!outside)
            {
                var forbidden = TraceChecker.ForbiddenActivities.Exists(x => x.Equals(this.Id));
                if (forbidden) return false;
            }
            if (Execute()) return true;
            TryMakeExecuteable();
            return Execute();
        }

        public string ExportRawDcrString()
        {
            var rawDcr = "";
            rawDcr += (Executed ? ":" : "") + "\"" + Id + "\"";
            rawDcr += (Excluded ? "%" : "") +  "\"" + Id + "\"";
            rawDcr += (Pending ? "!" : "") + "\"" + Id + "\"";

            return rawDcr;
        }

        private void TryMakeExecuteable()
        {
            if (TraceChecker.CircleActivities.Exists(x => x.Id.Equals(this.Id))) return;
            var remIndex = TraceChecker.CircleActivities.Count;
            try
            {
            TraceChecker.CircleActivities.Add(this);
             
            if (Excluded)
            {
                TryInclude();
                if (!Excluded) return;
            }

            if (IsExecuteable()) return;

            foreach (var relation in _relationsIncoming.Where(x => x.Type.Equals(Relation.RelationType.Condition) || x.Type.Equals(Relation.RelationType.Milestone)))
            {
                relation.From.TryDischarge(relation.Type);
            }
            }
            finally
            {
                TraceChecker.CircleActivities.RemoveAt(remIndex);
            }
        }

        private void TryDischarge(Relation.RelationType type)
        {
            if (!IsBlockingState(type)) return;
            TryExecute();
            if (!IsBlockingState(type)) return;

            TryExclude();
            if (!IsBlockingState(type)) return;

            TryMakeExecuteable();
            TryExecute();
        }

        public void TryExclude()
        {
            if (Excluded) return;

            foreach (var excludeRelation in _relationsIncoming.Where(x => x.Type.Equals(Relation.RelationType.Exclude)))
            {
                excludeRelation.From.TryExecute();
                if (!Excluded) return;
            }
        }
        private void TryInclude()
        {
            if (!Excluded) return;

            foreach (var includeRelation in _relationsIncoming.Where(x => x.Type.Equals(Relation.RelationType.Include)))
            {
                includeRelation.From.TryExecute();
                if (!Excluded) return;
            }
        }

        private void ChangeState(Relation.RelationType type)
        {
            switch (type)
            {
                case Relation.RelationType.Include:
                    Excluded = false;
                    break;
                case Relation.RelationType.Exclude:
                    Excluded = true;
                    break;
                case Relation.RelationType.Response:
                    Pending = true;
                    break;
            }
        }

        private bool IsBlockingState(Relation.RelationType type)
        {
            if (type == Relation.RelationType.Condition)
                return !Excluded && !Executed;
            if (type == Relation.RelationType.Milestone)
                return !Excluded && Pending;
            
            return false; // Others can't block execution
        }

        private static bool CheckZeroOrOne(int checkValue)
        {
            return checkValue >= 0 && checkValue <= 1;
        }


        public bool IsSafe(DcrGraph safeForDcrGraph)
        {
            //acyclic
            var dependencyGraph = this.BuildDependencyGraph();
            var cycleChecker = new CycleChecker();
            var cycle = cycleChecker.ExistsCycleConditionMilestone(dependencyGraph);
            var cycleResponse = cycleChecker.ExistsCycleResponse(dependencyGraph);
            if (cycle != null || cycleResponse != null) return false;

            // no include, exclude or response to any activity
            if (dependencyGraph.Any(x => x._relationsOutgoing.Any(y =>
                y.Type == Relation.RelationType.Include || y.Type == Relation.RelationType.Exclude ||
                y.Type == Relation.RelationType.Response))) return false;

            // np-hard? Check any outgoing condition/milestone reachable
            if (dependencyGraph.Any(x => x._relationsOutgoing.Any(y =>
                (y.Type == Relation.RelationType.Condition || y.Type == Relation.RelationType.Milestone) &&
                !ReachableFromTo(y.From, y.To, safeForDcrGraph)))) return false;

            return true;

        }

        private bool ReachableFromTo(Activity from, Activity to, DcrGraph dcrGraph)
        {
            var testGraph = new DcrGraph(dcrGraph.EditWindowString, new List<string>(), dcrGraph.EditWindowString, dcrGraph.Name);
            var testFrom = testGraph.Activities.First(x => x.Id.Equals(from.Id));
            var testTo = testGraph.Activities.First(x => x.Id.Equals(to.Id));

            var executed = testFrom.TryExecute();
            if (!executed) return false;
            executed = testTo.TryExecute();
            return executed;
        }

        public List<Activity> BuildDependencyGraph(List<Activity> superDenpendencyGraph = null)
        {
            if(superDenpendencyGraph == null) superDenpendencyGraph = new List<Activity>(){this};

            foreach (var inRelation in this._relationsIncoming)
            {
                if ((inRelation.Type == Relation.RelationType.Condition ||
                     inRelation.Type == Relation.RelationType.Milestone || inRelation.Type == Relation.RelationType.Response) &&
                    !superDenpendencyGraph.Any(x => x.Id.Equals(inRelation.From.Id)))
                {
                    superDenpendencyGraph.Add(inRelation.From);
                    inRelation.From.BuildDependencyGraph(superDenpendencyGraph);
                }

            }

            return superDenpendencyGraph;
        }
    }

}
