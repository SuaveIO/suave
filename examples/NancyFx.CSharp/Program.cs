using Suave;
using Owin;
using Nancy;
using Nancy.Owin;
using System;
using Microsoft.FSharp.Core;
using Microsoft.FSharp.Control;

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
            var app = Owin.OwinAppModule.OfMidFunc("/", NancyMiddleware.UseNancy(opts));
            Func<Http.HttpContext, FSharpAsync<FSharpOption<Http.HttpContext>>> webPart = x => Owin.op_EqualsGreaterEquals(app, RequestErrors.NOT_FOUND("File not found"), x);
            var webPartFunc = Microsoft.FSharp.Core.FSharpFunc<Http.HttpContext, FSharpAsync<FSharpOption<Http.HttpContext>>>.FromConverter(new Converter< Http.HttpContext, FSharpAsync < FSharpOption < Http.HttpContext >> >(webPart));
            Web.startWebServer(Web.defaultConfig, webPartFunc);
            return 0;
        }
    }
}