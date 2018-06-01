using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Threading.Tasks;

namespace DataLogic.DcrGraph
{
    public class Freedom
    {
        public List<DcrGraph> DcrGraphs { get; set; }

        public DcrGraph RelatedDcrGraph { get; set; }
        public List<Tuple<DcrGraph, List<Activity>>> Deadlocks { get; set; } = new List<Tuple<DcrGraph, List<Activity>>>();
        public List<Tuple<DcrGraph, List<Activity>>> Livelokcs { get; set; } = new List<Tuple<DcrGraph, List<Activity>>>();

        public bool CheckDeadlockLivelockFreedom(List<DcrGraph> dcrGraphs)
        {
            DcrGraphs = dcrGraphs;

            CreateRelatedGraph();

            FindDeadlocks();

            FindLivelocks();

            return !Deadlocks.Any() && !Livelokcs.Any();
        }

        private void FindDeadlocks()
        {
            var cycleChecker = new CycleChecker();
            foreach (var dcrGraph in DcrGraphs)
            {
                var cycles = cycleChecker.GetAllCycleConditionMilestone(dcrGraph.Activities);
                if (cycles != null)
                {
                    var breakAble = false;
                    // check any event included and no incoming except include
                    breakAble = dcrGraph.Activities.Any(x =>
                        !x.Excluded && x._relationsIncoming.All(y => y.Type == Relation.RelationType.Include));
                    if (breakAble) return;

                    // check exists safe event which can not be excluded
                    foreach (var activity in dcrGraph.Activities)
                    {
                        if (activity.IsSafe(dcrGraph) &&
                            activity._relationsIncoming.All(x => x.Type != Relation.RelationType.Exclude))
                            breakAble = true;
                    }

                    if (breakAble) return;

                    var cyclesBreakAble = new List<bool>();

                    foreach (var cycle in cycles)
                    {
                        breakAble = cycle.Any(x =>
                            x._relationsIncoming.All(y => y.Type != Relation.RelationType.Include) &&
                            x._relationsIncoming.All(y =>
                                y.Type == Relation.RelationType.Exclude && y.From.IsSafe(dcrGraph)));
                        cyclesBreakAble.Add(breakAble);
                    }

                    for (var index = 0; index < cyclesBreakAble.Count; index++)
                    {
                        var breakable = cyclesBreakAble[index];
                        if (breakable) continue;
                        var cycle = cycles[index];
                        if (cycle.All(x =>
                            !x.Pending && x._relationsIncoming.All(y => y.Type != Relation.RelationType.Response)))
                            cyclesBreakAble[index] = true;
                    }

                    for (var index = 0; index < cyclesBreakAble.Count; index++)
                    {
                        var b = cyclesBreakAble[index];
                        var cycle = cycles[index];
                        if(!b) Deadlocks.Add(new Tuple<DcrGraph, List<Activity>>(dcrGraph, cycle));
                    }
                }
            }
        }

        private void FindLivelocks()
        {
            var cycleChecker = new CycleChecker();
            foreach (var dcrGraph in DcrGraphs)
            {
                var deadCycles = cycleChecker.GetAllCycleConditionMilestone(dcrGraph.Activities);
                var liveCycles = cycleChecker.GetAllCycleResponse(dcrGraph.Activities);
                if (liveCycles == null && deadCycles == null) return;

                var noDead = new List<bool>();

                if (deadCycles != null)
                {
                    foreach (var deadCycle in deadCycles)
                    {
                        if (deadCycle.All(x =>
                            !x.Pending && x._relationsIncoming.All(y => y.Type != Relation.RelationType.Response)))
                        {
                            if (deadCycle.Any(x => x._relationsOutgoing.Any(y =>
                                (y.Type == Relation.RelationType.Condition ||
                                y.Type == Relation.RelationType.Milestone) && !y.To.Id.Equals(y.From.Id))))
                            {
                                noDead.Add(false);
                                continue;
                            }
                            noDead.Add(true);
                            continue;
                        }
                            
                        var possiblyPending = deadCycle.Where(x =>
                            x.Pending || x._relationsIncoming.Any(y => y.Type == Relation.RelationType.Response)).ToList();
                        if (possiblyPending.Any())
                        {
                            foreach (var activity in possiblyPending)
                            {
                                var graphWithNoRelationsToActivityBeingTested = new DcrGraph(dcrGraph.EditWindowString, new List<string>(), dcrGraph.EditWindowString, "graphWithNoRelationsToActivityBeingTested");
                                var perhapsSafeActivity =
                                    graphWithNoRelationsToActivityBeingTested.Activities.Where(x =>
                                        x._relationsOutgoing.Any(y => y.To.Id.Equals(activity.Id) && y.Type == Relation.RelationType.Exclude)).ToList();

                                foreach (var activity1 in perhapsSafeActivity)
                                {
                                    activity1._relationsOutgoing.Remove(activity1._relationsOutgoing.First(x =>
                                        x.Type == Relation.RelationType.Exclude && x.To.Id.Equals(activity.Id)));
                                }

                                var anySafeExlude = perhapsSafeActivity.Any(x =>
                                    x.IsSafe(graphWithNoRelationsToActivityBeingTested) && x._relationsIncoming.All(y => y.Type != Relation.RelationType.Exclude));

                                if (!(activity.Excluded || anySafeExlude))
                                {
                                    noDead.Add(false);
                                    break;
                                }
                            }
                            //noDead.Add(true);
                        }
                        else noDead.Add(true);
                    }

                    for (int i = 0; i < deadCycles.Count; i++)
                    {
                        var cycle = deadCycles[i];
                        bool live = noDead[i];
                        if(!live) Livelokcs.Add(new Tuple<DcrGraph, List<Activity>>(dcrGraph, cycle));
                    }
                }

                var noLive = new List<bool>();

                if (liveCycles != null)
                {
                    foreach (var liveCycle in liveCycles)
                    {
                        if (liveCycle.All(x => x.IsSafe(dcrGraph)))
                        {
                            noLive.Add(true);
                            continue;
                        }

                        if (liveCycle.Any(x =>
                            x.Excluded ||
                            x._relationsIncoming.Any(y =>
                                y.Type == Relation.RelationType.Exclude && y.From.IsSafe(dcrGraph) &&
                                y.From._relationsIncoming.All(z => z.Type != Relation.RelationType.Exclude)) &&
                            x._relationsIncoming.All(c => c.Type != Relation.RelationType.Include)))
                        {
                            noLive.Add(true);
                            continue;
                        }

                        noLive.Add(false);
                    }

                    for (int i = 0; i < liveCycles.Count; i++)
                    {
                        var cycle = liveCycles[i];
                        bool live = noLive[i];
                        if (!live) Livelokcs.Add(new Tuple<DcrGraph, List<Activity>>(dcrGraph, cycle));
                    }
                }
            }
        }

        private void CreateRelatedGraph()
        {
            // find all directly shared activities
            var sharedActivities = new List<Activity>();

            foreach (var dcrGraph in DcrGraphs)
            {
                foreach (var activity in dcrGraph.Activities)
                {
                    if (sharedActivities.Any(x => x.Id.Equals(activity.Id))) continue;
                    foreach (var x in DcrGraphs)
                    {
                        var q = !x.Name.Equals(dcrGraph.Name);
                        var w = x.Activities.Any(y => y.Id.Equals(activity.Id));
                        if (q && w)
                        {
                            sharedActivities.Add(activity);
                            break;
                        }
                    }
                }
            }

            //Build the fully merged graph
            var mergeDcrString = "";

            foreach (var dcrGraph in DcrGraphs)
            {
                mergeDcrString += " \r\n " + dcrGraph.EditWindowString;
            }

            var mergedGraph = new DcrGraph(mergeDcrString, new List<string>(), mergeDcrString, "relatedGraph");

            // get the shared activities now that they have the merged relations
            var sharedActivitiesWithProperRelations = new List<Activity>();

            foreach (var sharedActivity in sharedActivities)
            {
                sharedActivitiesWithProperRelations.Add(mergedGraph.Activities.First(x => x.Id.Equals(sharedActivity.Id)));
            }

            // Add in the dependency graph
            for (var i = 0; i < sharedActivitiesWithProperRelations.Count; i++)
            {
                var activity = sharedActivitiesWithProperRelations[i];
                var dependencyGraph = activity.BuildDependencyGraph();
                foreach (var activity1 in dependencyGraph)
                {
                    if (sharedActivitiesWithProperRelations.Any(x => x.Id.Equals(activity1.Id))) continue;
                    sharedActivitiesWithProperRelations.Add(activity1);
                }
            }

            // remove irrelevant activities/relations
            foreach (var activity in sharedActivitiesWithProperRelations)
            {
                var toBeRemovedIn = new List<Relation>();
                
                foreach (var relation in activity._relationsIncoming)
                {
                    if(sharedActivitiesWithProperRelations.Any(x => !x.Id.Equals(relation.From.Id))) toBeRemovedIn.Add(relation);
                }

                var toBeRemovedOut = new List<Relation>();

                foreach (var relation in activity._relationsOutgoing)
                {
                    if (sharedActivitiesWithProperRelations.Any(x => !x.Id.Equals(relation.To.Id))) toBeRemovedOut.Add(relation);
                }

                foreach (var relation in toBeRemovedIn)
                {
                    if(!relation.To._relationsIncoming.Remove(relation)) Console.WriteLine(relation.ToString() + " could not be removed in Freedom.cs");
                }

                foreach (var relation in toBeRemovedOut)
                {
                    if(!relation.From._relationsOutgoing.Remove(relation)) Console.WriteLine(relation.ToString() + " could not be removed in Freedom.cs");
                }
            }

            for (var i = 0; i < mergedGraph.Activities.Count; i++)
            {
                var activity = mergedGraph.Activities[i];
                if (sharedActivitiesWithProperRelations.Any(x => x.Id.Equals(activity.Id))) continue;
                mergedGraph.Activities.Remove(activity);
                i--;
            }

            var rawDcr = mergedGraph.ExportRawDcrString();

            RelatedDcrGraph = new DcrGraph(rawDcr, new List<string>(), rawDcr, "RelatedGraph" );
        }
    }
}
