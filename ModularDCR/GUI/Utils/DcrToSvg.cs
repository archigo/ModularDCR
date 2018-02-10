using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Text;
using System.Threading.Tasks;

namespace GUI.Utils
{
    public class DcrToSvg
    {
        public static async Task<string> GetSvgFromDcr(string dcr)
        {
            var body = "src=" + dcr;

            var noplus = body.Replace("+", "%2B");

            //var encoded = Uri.EscapeDataString(body);


            //body = Regex.Replace(body, @"[^\w\s<>/""=]", "");

            using (WebClient wc = new WebClient())
            {
                wc.Headers[HttpRequestHeader.ContentType] = "application/x-www-form-urlencoded";

                var result = await wc.UploadStringTaskAsync("http://dcr.itu.dk:8023/trace/dcr", noplus);

                return result;
            }
        }
    }
}
