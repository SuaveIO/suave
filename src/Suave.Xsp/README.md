# Suave.Xsp

This module allows to run ASP.NET applications in Suave.

```fsharp
startWebServer defaultConfig (Xsp.createApplication "/mnt/myapp/" |> Xsp.run)
```


# Gotchas

.NET and Mono's implementation of the HttpRuntime class differs in small and subtles ways that need to be accounted for when running ASP.NET apps in Mono.

## Common Linux error

System.UnauthorizedAccessException
Access to the path "/etc/mono/registry" is denied.

http://stackoverflow.com/questions/24872394/access-to-the-path-etc-mono-registry-is-denied


simply creating the folder (/etc/mono/registry) and setting the right permissions (chmod uog+rw /etc/mono/registry) did the trick. 

