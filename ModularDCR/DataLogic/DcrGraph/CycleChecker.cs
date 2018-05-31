using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DataLogic.DcrGraph
{
    public class CycleChecker
    {
        public List<Activity> ExistsCycleConditionMilestone(List<Activity> activities)
        {
            foreach (var activity in activities)
            {
                var cycle = ExploreConditionMilestoneForExisting(activity, new List<Activity>() {activity});
                if (cycle != null) return cycle;
            }

            return null;
        }

        private List<Activity> ExploreConditionMilestoneForExisting(Activity activity, List<Activity> checkedActivities)
        {
            if (checkedActivities.Any(x => x.Id.Equals(activity.Id))) return checkedActivities;
            checkedActivities.Add(activity);
            foreach (var relation in activity._relationsIncoming)
            {
                if (relation.Type != Relation.RelationType.Condition ||
                    relation.Type != Relation.RelationType.Milestone) continue;
                var cycle = ExploreConditionMilestoneForExisting(relation.From, checkedActivities);
                if (cycle != null) return cycle;
            }

            return null;
        }

        public List<Activity> ExistsCycleResponse(List<Activity> activities)
        {
            foreach (var activity in activities)
            {
                var cycle = ExploreResponseForExisting(activity, new List<Activity>() { activity });
                if (cycle != null) return cycle;
            }

            return null;
        }

        private List<Activity> ExploreResponseForExisting(Activity activity, List<Activity> checkedActivities)
        {
            if (checkedActivities.Any(x => x.Id.Equals(activity.Id))) return checkedActivities;
            checkedActivities.Add(activity);
            foreach (var relation in activity._relationsIncoming)
            {
                if (relation.Type != Relation.RelationType.Response) continue;
                var cycle = ExploreResponseForExisting(relation.From, checkedActivities);
                if (cycle != null) return cycle;
            }

            return null;
        }

        public List<Activity> ExistsCycle(List<Activity> activities)
        {
            foreach (var activity in activities)
            {
                var cycle = ExploreForExisting(activity, new List<Activity>() { activity });
                if (cycle != null) return cycle;
            }

            return null;
        }

        private List<Activity> ExploreForExisting(Activity activity, List<Activity> checkedActivities)
        {
            if (checkedActivities.Any(x => x.Id.Equals(activity.Id))) return checkedActivities;
            checkedActivities.Add(activity);
            foreach (var relation in activity._relationsIncoming)
            {
                var cycle = ExploreForExisting(relation.From, checkedActivities);
                if (cycle != null) return cycle;
            }

            return null;
        }

        public List<List<Activity>> GetAllCycleConditionMilestone(List<Activity> activities)
        {
            var cycles = new List<List<Activity>>();
            foreach (var activity in activities)
            {
                var cycle = ExploreAllConditionMilestoneForExisting(activity, new List<Activity>() { activity }, new List<List<Activity>>());
                if (cycle != null) cycles.AddRange(cycle);
            }

            return null;
        }

        private List<List<Activity>> ExploreAllConditionMilestoneForExisting(Activity activity, List<Activity> checkedActivities, List<List<Activity>> cycles)
        {
            if (checkedActivities.Any(x => x.Id.Equals(activity.Id)))
            {
                cycles.Add(checkedActivities);
                return cycles;
            }

            checkedActivities.Add(activity);

            foreach (var relation in activity._relationsIncoming)
            {
                if (relation.Type != Relation.RelationType.Condition ||
                    relation.Type != Relation.RelationType.Milestone) continue;
                var cycle = ExploreAllConditionMilestoneForExisting(relation.From, checkedActivities, cycles);
                if (cycle != null)
                {
                    cycles.AddRange(cycle);
                    return cycles;
                }
            }

            return null;
        }

        public List<List<Activity>> GetAllCycleResponse(List<Activity> activities)
        {
            var cycles = new List<List<Activity>>();
            foreach (var activity in activities)
            {
                var cycle = ExploreAllResponseForExisting(activity, new List<Activity>() { activity }, new List<List<Activity>>());
                if (cycle != null) cycles.AddRange(cycle);
            }

            return null;
        }

        private List<List<Activity>> ExploreAllResponseForExisting(Activity activity, List<Activity> checkedActivities, List<List<Activity>> cycles)
        {
            if (checkedActivities.Any(x => x.Id.Equals(activity.Id)))
            {
                cycles.Add(checkedActivities);
                return cycles;
            }

            checkedActivities.Add(activity);

            foreach (var relation in activity._relationsIncoming)
            {
                if (relation.Type != Relation.RelationType.Response) continue;
                var cycle = ExploreAllResponseForExisting(relation.From, checkedActivities, cycles);
                if (cycle != null)
                {
                    cycles.AddRange(cycle);
                    return cycles;
                }
            }

            return null;
        }
    }
}
