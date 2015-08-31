using Suave;
using Nancy;
using Nancy.Owin;

namespace Suave.NancyFx
{
    public class SampleModule : NancyModule
    {
        public SampleModule()
        {
            Get["/"] = _ => "Hello World!";
        }
    }
    
    public class Program
    {
        public static int Main(string[] args)
        {
            var opts = new NancyOptions();
            var app = Suave.Owin.OwinAppModule.OfMidFunc(NancyMiddleware.UseNancy(opts));
            Web.startWebServer(Web.defaultConfig, app);
            return 0;
        }
    }
}