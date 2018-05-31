using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DataLogic.DcrGraph
{
    public class ActivityStateHolder
    {
        public string Id { get; set; }
        public bool Executed { get; set; }
        public bool Pending { get; set; }
        public bool Excluded { get; set; }

        public ActivityStateHolder(Activity activity)
        {
            Id = activity.Id;
            Executed = activity.Executed;
            Pending = activity.Pending;
            Excluded = activity.Excluded;
        }
    }
}
