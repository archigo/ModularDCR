using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace PreviewOnWinRT
{
    class Operation
    {
        // constructor logic 
        public Operation(string name)
        {
            this.Name = name;
        }
        // Name
        private string _name;
        public string Name
        {
            get { return _name; }
            set { _name = value; }
        }
    }
}
