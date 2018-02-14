using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Text;
using System.Threading.Tasks;
using System.Web;

namespace GUI.Utils
{
    public class DcrToSvg
    {
        public static async Task<string> GetSvgFromDcr(string dcr)
        {
            //dcr = dcr.Replace(":", "__e");
            var body = "src=" + System.Net.WebUtility.UrlEncode(dcr);
            //var qwe = System.Net.WebUtility.UrlEncode(body);
            //var noplus = body.Replace("+", "%2B");

            //var encoded = Uri.EscapeDataString(body);


            //body = Regex.Replace(body, @"[^\w\s<>/""=]", "");

            using (WebClient wc = new WebClient())
            {
                wc.Headers[HttpRequestHeader.ContentType] = "application/x-www-form-urlencoded";

                var result = await wc.UploadStringTaskAsync("http://dcr.itu.dk:8023/trace/dcr", body);

                result = result.Replace("âœ“", "&#10003;");

                return result;
            }
        }
    }
}
