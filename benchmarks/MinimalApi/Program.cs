var builder = WebApplication.CreateBuilder(args);
builder.WebHost.UseUrls("http://127.0.0.1:3001");
builder.WebHost.ConfigureKestrel(options => options.AddServerHeader = false);
builder.Logging.ClearProviders();

var app = builder.Build();
Task EmptyResponse(HttpContext context)
{
	context.Response.ContentType = "text/html";
	context.Response.ContentLength = 0;
	return Task.CompletedTask;
}

app.MapGet("/", EmptyResponse);
app.MapGet("/user/{id}", (string id) => Results.Bytes(System.Text.Encoding.UTF8.GetBytes(id), "text/html"));
app.MapPost("/user", EmptyResponse);
app.Run();