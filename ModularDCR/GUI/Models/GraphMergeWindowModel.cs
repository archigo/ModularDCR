using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Media;
using DataLogic.DcrGraph;
using DataLogic.Trace;

namespace GUI.Models
{
    public class GraphMergeWindowModel : SuperModel
    {
        private GraphMerge _graphMerge;
        private DrawingImage _dcrImage;
        public DrawingImage DcrImage
        {
            get => _dcrImage;
            set
            {
                _dcrImage = value;
                OnPropertyChanged(nameof(DcrImage));
            }
        }

        

        public GraphMerge GraphMerge
        {
            get => _graphMerge;
            set
            {
                _graphMerge = value;
                OnPropertyChanged(nameof(GraphMerge));
            }
        }

        public GraphMergeWindowModel(List<DcrGraph> dcrGraphs)
        {
            GraphMerge = new GraphMerge(dcrGraphs);
        }

        public void SimulateOnPropertyChanged()
        {
            OnPropertyChanged();
        }
    }
}
