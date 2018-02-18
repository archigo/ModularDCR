using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DataLogic.DcrGraph
{
    internal class Relation
    {
        public enum RelationType
        {
            Include,
            Exclude,
            Condition,
            Response,
            Milestone
        }

        public Activity From { get; }
        public Activity To { get; }
        public RelationType Type { get;}

        public Relation(Activity from, Activity to, int type)
        {
            From = from;
            To = to;

            switch (type)
            {
                case 0:
                    Type = RelationType.Include;
                    break;
                case 1:
                    Type = RelationType.Exclude;
                    break;
                case 2:
                    Type = RelationType.Response;
                    break;
                case 3:
                    Type = RelationType.Condition;
                    break;
                case 4:
                    Type = RelationType.Milestone;
                    break;
                default:
                    throw new Exception($"A relation was created with an invalid type: {type}. Valid is from 0 to 4. Relation from {From} to {To}.");
            }
        }

        public string ExportRawDcrString()
        {
            switch (Type)
            {
                case RelationType.Include:
                    return $"\"{From.Id}\" -->+ \"{To.Id}\"";
                case RelationType.Exclude:
                    return $"\"{From.Id}\" -->% \"{To.Id}\"";
                case RelationType.Response:
                    return $"\"{From.Id}\" *--> \"{To.Id}\"";
                case RelationType.Condition:
                    return $"\"{From.Id}\" -->* \"{To.Id}\"";
                case RelationType.Milestone:
                    return $"\"{From.Id}\" --<> \"{To.Id}\"";
                default:
                    throw new Exception("Export relation of unkown type to raw dcr. This should not ever happen ?!?");
            }
        }
    }
}

