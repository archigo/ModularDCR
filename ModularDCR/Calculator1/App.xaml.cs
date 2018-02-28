using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;

namespace Calculator1
{
    public sealed partial class App : Application
    {
        public App()
        {
            this.InitializeComponent();

            var mainPage = new MainPage();
            Window.Current.Content = mainPage;
        }
    }
}
