using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace DataLogic.DcrGraph
{
    public class DcrGraph
    {
        private readonly List<Activity> _activities = new List<Activity>();
        private readonly List<Relation> _relations = new List<Relation>();
        public string Name { get; set; }
        public string EditWindowString { get; set; }
        public List<Trace.Trace> StoredTraces { get; set; } = new List<Trace.Trace>();
        private List<ActivityStateHolder> InitialActivityStates { get; set; } = new List<ActivityStateHolder>();

        public List<string> StrictActivities
        {
            get { return _activities.Where(x => x.Strict).Select(x => x.Id).ToList(); }
            
        }

        public List<Activity> Activities => _activities;  

        public DcrGraph(string rawDcr, List<string> strictActivities, string editWindowString, string name)
        {
            Name = name;
            EditWindowString = editWindowString;
            ParseRawDcr(rawDcr);
            if (strictActivities != null)
            {
                foreach (var strictActivity in strictActivities)
                {
                    var activity = _activities.FirstOrDefault(x => x.Id.Equals(strictActivity));
                    if (activity != null) activity.Strict = true;
                }
            }

            foreach (var activity in Activities)
            {
                InitialActivityStates.Add(new ActivityStateHolder(activity));
            }
        }

        #region Public Methods

        public void ResetGraphState()
        {
            foreach (var initialActivityState in InitialActivityStates)
            {
                var activity = Activities.First(x => x.Id.Equals(initialActivityState.Id));

            }
        }

        public string[] GetActivityNames() => _activities.Count == 0 ? default(string[]) : _activities.Select(x => x.Id).ToArray();

        public string[] GetExecutableActivityNames() => _activities.Count == 0 ? new string[0] : _activities.Where(a => a.IsExecuteable()).Select(a => a.Id).ToArray();

        public bool ExecuteActivity(string id)
        {
            var activity = _activities.Find(a => a.Id.Equals(id));

            if(activity == null)
                throw new Exception($"Attempt to execute activity which does not exist. Attempted activity id: {id}.\nExisting activities:\n{(_activities.Count != 0 ? _activities.Select(a => a.Id).Aggregate((acc, val) => acc + "\n" + val) : "" )}");

            return activity.Execute();
        }

        public string ExportRawDcrString()
        {
            var rawDcr = "";

            foreach (var activity in _activities)
            {
                rawDcr += activity.ExportRawDcrString();
            }

            foreach (var relation in _relations)
            {
                rawDcr += relation.ExportRawDcrString();
            }

            return rawDcr;
        }

        public Activity GetActivityByName(string name)
        {
            return _activities.FirstOrDefault(x => x.Id.Equals(name));
        }

        #endregion

        #region Private Methods

        private void ParseRawDcr(string rawDcr)
        {
            var easyReadFormat = Convert.convert(rawDcr);

            var lineSplit = easyReadFormat.Replace(Environment.NewLine, ",").Split(',');

            var eventCount = int.Parse(lineSplit[0]);

            
            if(eventCount == 0) return; // Empty graph
            

            for (int i = 1; i <= eventCount; i++)
            {
                CreateActivities(lineSplit[i]);
            }

            for (int i = eventCount + 1; i < lineSplit.Length; i++)
            {
                CreateRelation(lineSplit[i]);
            }

            foreach (var activity in _activities)
            {
                activity.SetRelations(_relations.Where(r => r.To.Id.Equals(activity.Id)).ToList(), _relations.Where(r => r.From.Id.Equals(activity.Id)).ToList());
            }
        }

        private void CreateActivities(string activityDefinition)
        {
            var regex = new Regex("^[0-1] [0-1] [0-1]");

            var rawBools = regex.Match(activityDefinition).Value;

            var id = activityDefinition.Replace(rawBools, "").TrimStart(' ');

            var bools = rawBools.Split(' ');

            if(bools.Length != 3 || id.Equals(""))
                throw new Exception("Activity definition was invalid. " + activityDefinition);

            var activity = new Activity(int.Parse(bools[0]), int.Parse(bools[1]), int.Parse(bools[2]), id);

            _activities.Add(activity);
        }

        private void CreateRelation(string relationDefinition)
        {
            var rawVals = relationDefinition.Split(' ');

            if(rawVals.Length != 3)
                throw new Exception("Relation definition was invalid. " + relationDefinition);

            _relations.Add(new Relation(_activities[int.Parse(rawVals[0])], _activities[int.Parse(rawVals[1])], int.Parse(rawVals[2])));
        }

        #endregion

        public void TakeEventLocalActivities(DcrGraph graphToMerge)
        {
            var sharedActivities = graphToMerge.Activities.Where(x => Activities.Exists(y => y.Id.Equals(x.Id))).ToList();

            foreach (var sharedActivity in sharedActivities)
            {
                var localActivity = Activities.First(x => x.Id.Equals(sharedActivity.Id));

                localActivity.Excluded = sharedActivity.Excluded || localActivity.Excluded;
                localActivity.Pending = sharedActivity.Pending || localActivity.Pending;
                localActivity.Executed = sharedActivity.Executed || localActivity.Executed;

                foreach (var relation in sharedActivity._relationsIncoming)
                {
                    // from activity must exist in the local graph. The relation must not already exist.
                    if (Activities.Exists(x => x.Id.Equals(relation.From.Id)) && !localActivity._relationsIncoming.Any(x => x.Type.Equals(relation.Type) && x.From.Id.Equals(relation.From.Id)))
                    {
                        var localFrom = Activities.First(x => x.Id.Equals(relation.From.Id));
                        localActivity._relationsIncoming.Add(new Relation(localFrom, localActivity, (int) relation.Type));
                    }
                }

                foreach (var relation in sharedActivity._relationsOutgoing)
                {
                    // "to" activity must exist in the local graph. The relation must not already exist.
                    if (Activities.Exists(x => x.Id.Equals(relation.To.Id)) && !localActivity._relationsOutgoing.Exists(x => x.Type.Equals(relation.Type) && x.From.Id.Equals(relation.From.Id)))
                    {
                        var localTo = Activities.First(x => x.Id.Equals(relation.To.Id));
                        localActivity._relationsOutgoing.Add(new Relation(localActivity, localTo, (int)relation.Type));
                    }
                }
            }

        }

        /// <summary>
        /// Null if transparent, otherwise return first relation that breaks.
        /// </summary>
        /// <param name="dcrGraph"></param>
        /// <returns></returns>
        public Tuple<Activity,Relation> IsTransparent(DcrGraph dcrGraph)
        {
            var dcrstring = EditWindowString + "\r\n" + dcrGraph.EditWindowString;
            var mergedGraph = new DcrGraph(dcrstring, new List<string>(), dcrstring, "tempMerge");
            var newRelations = new List<Relation>();

            foreach (var newRelation in dcrGraph._relations)
            {
                if(RelationExists(newRelation)) continue;
                newRelations.Add(newRelation);
            }

            foreach (var mergedActivity in mergedGraph.Activities)
            {
                var oldActivity = Activities.FirstOrDefault(x => x.Id.Equals(mergedActivity.Id));
                //preserve marking
                if (oldActivity != null && oldActivity.Excluded != mergedActivity.Excluded && oldActivity.Pending != mergedActivity.Pending && oldActivity.Executed != mergedActivity.Executed)
                {
                    return new Tuple<Activity, Relation>(mergedActivity, null);
                }
                // no new pending
                if (mergedActivity.Pending) return new Tuple<Activity, Relation>(mergedActivity, null);
            }

            foreach (var newRelation in newRelations)
            {
                switch (newRelation.Type)
                {
                    case Relation.RelationType.Include:
                        // not between old
                        if(Activities.Any(x => x.Id.Equals(newRelation.From.Id)) && Activities.Any(x => x.Id.Equals(newRelation.To.Id))) return new Tuple<Activity, Relation>(newRelation.From, newRelation);

                        //no old to new
                        if(Activities.Any(x => x.Id.Equals(newRelation.From.Id)) && !Activities.Any(x => x.Id.Equals(newRelation.To.Id))) return new Tuple<Activity, Relation>(newRelation.From, newRelation);

                        // Includes to old must exists beforehand
                        if (Activities.Any(x => x.Id.Equals(newRelation.To.Id)) && !_relations.Any(x => x.Type == Relation.RelationType.Include && x.From.Id.Equals(newRelation.From.Id) && x.To.Id.Equals(newRelation.To.Id))) return new Tuple<Activity, Relation>(newRelation.From, newRelation);
                        break;
                    case Relation.RelationType.Exclude:
                        // not between old
                        if (Activities.Any(x => x.Id.Equals(newRelation.From.Id)) && Activities.Any(x => x.Id.Equals(newRelation.To.Id))) return new Tuple<Activity, Relation>(newRelation.From, newRelation);
                        // Excludes to old must exists beforehand
                        if(Activities.Any(x => x.Id.Equals(newRelation.To.Id)) && !_relations.Any(x => x.Type == Relation.RelationType.Exclude && x.From.Id.Equals(newRelation.From.Id) && x.To.Id.Equals(newRelation.To.Id))) return new Tuple<Activity, Relation>(newRelation.From, newRelation);
                        break;
                    case Relation.RelationType.Response:
                        // not between old
                        if (Activities.Any(x => x.Id.Equals(newRelation.From.Id)) && Activities.Any(x => x.Id.Equals(newRelation.To.Id))) return new Tuple<Activity, Relation>(newRelation.From, newRelation);
                        //no old to new
                        if (Activities.Any(x => x.Id.Equals(newRelation.From.Id)) && !Activities.Any(x => x.Id.Equals(newRelation.To.Id))) return new Tuple<Activity, Relation>(newRelation.From, newRelation);
                        break;
                    case Relation.RelationType.Condition:
                    case Relation.RelationType.Milestone:
                        //must not add from old activities
                        if(Activities.Any(x => x.Id.Equals(newRelation.From.Id))) return new Tuple<Activity, Relation>(newRelation.From, newRelation);
                        // Can only add from new if new is safe
                        if(Activities.Any(x => x.Id.Equals(newRelation.To.Id) && !mergedGraph.Activities.First(y => y.Id.Equals(newRelation.From.Id)).IsSafe(mergedGraph))) return new Tuple<Activity, Relation>(newRelation.From, newRelation);
                        break;
                }
            }

            return new Tuple<Activity, Relation>(null, null);
        }

        private bool RelationExists(Relation relation) => this._relations.Any(x =>
                x.From.Id.Equals(relation.From.Id) && x.Type.Equals(relation.Type) && x.To.Id.Equals(relation.To.Id));

        public bool IsAccepting()
        {
            return !Activities.Any(x => x.Pending);
        }
    }
}
