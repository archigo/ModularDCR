using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace PreviewOnWinRT
{
    class Operations
    {
        private static Operation _BPlus = new Operation("BPlus");
        public static Operation BPlus()
        {
            return _BPlus;
        }
    }
}
