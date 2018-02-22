using System;
using System.Collections.Generic;
using System.Data.Entity.Core.Metadata.Edm;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DataLogic.DcrGraph
{
    public class Activity
    {
        public string Id { get; set; }
        public bool Executed { get; set; }
        public bool Pending { get; set; }
        public bool Excluded { get; set; }
        private Relation[] _relationsIncoming;
        private Relation[] _relationsOutgoing;

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

        /// <summary>
        /// Must only be called once, as part of the graph creation.
        /// </summary>
        /// <param name="relationsIncoming"></param>
        /// <param name="relationsOutgoing"></param>
        public void SetRelations(Relation[] relationsIncoming, Relation[] relationsOutgoing)
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

            Executed = true;
            ExecutionCount++;

            Pending = false;

            foreach (var relation in _relationsOutgoing)
            {
                relation.To.ChangeState(relation.Type);
            }

            return Executed;
        }

        public string ExportRawDcrString()
        {
            var rawDcr = "";
            rawDcr += (Executed ? ":" : "") + "\"" + Id + "\"";
            rawDcr += (Excluded ? "%" : "") +  "\"" + Id + "\"";
            rawDcr += (Pending ? "!" : "") + "\"" + Id + "\"";

            return rawDcr;
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
    }
}
