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

        public DcrGraph(string rawDcr)
        {
            ParseRawDcr(rawDcr);
        }

        #region Public Methods

        public string[] GetActivityNames() => _activities.Count == 0 ? default(string[]) : _activities.Select(x => x.Id).ToArray();

        public string[] GetExecutableActivityNames() => _activities.Count == 0 ? default(string[]) : _activities.Where(a => a.IsExecuteable()).Select(a => a.Id).ToArray();

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
                activity.SetRelations(_relations.Where(r => r.To.Id.Equals(activity.Id)).ToArray(), _relations.Where(r => r.From.Id.Equals(activity.Id)).ToArray());
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
    }
}
