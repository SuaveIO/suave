---
layout: default
---

Deploying Suave to Azure App Service
------------------------------------

Suave Web Sites in Azure App Service can be deployed either as an F# script with a suitable host e.g. FAKE, or as a standalone executable. This tutorial will focus on: -

* Hosting a Suave console application in Azure App Service
* Deploying and building your Suave application from source control directly in the Azure App Service  
* Instructing the Azure App Service IIS host to redirect all traffic to Suave

### 1. Create a ``web.config`` file.
This file instructs IIS to act as a passthrough and to redirect all traffic through to Suave.

{% highlight xml %}
<?xml version="1.0" encoding="UTF-8"?>
<configuration>
  <system.webServer>
    <handlers>
      <remove name="httpplatformhandler" />
      <add name="httpplatformhandler" path="*" verb="*" modules="httpPlatformHandler" resourceType="Unspecified"/>
    </handlers>
    <httpPlatform stdoutLogEnabled="true" stdoutLogFile=".\suave.log" startupTimeLimit="20" processPath="%HOME%\site\wwwroot\SuaveHost.exe" arguments="%HTTP_PLATFORM_PORT%"/>
  </system.webServer>
</configuration>
{% endhighlight %}

* Notice the use of the ``processPath`` attribute which tells IIS which executable to host. Change the content from SuaveHost.exe to the name of your application.
* Also note the use of ``%HTTP_PLATFORM_PORT%`` as an argument to Suave. This tells Suave which port it should listen on for inbound HTTP traffic.
* This file does *not* replace your ``app.config`` file, which should remain.

### 2. Create a ``.deployment`` file.
At this point, you can elect to manually FTP your Suave application into the Azure App Service into ``site\wwwroot`` - it should just work. However, if you want an automated deployment process from source control, continue with the following steps.

This file tells Azure App Service what to do after downloading your application code i.e. your build script e.g.

{% highlight ini %}
[config]
command = build.cmd
{% endhighlight %}

In this case, it will run ``build.cmd``. Of course, this can do anything you want. The output should be that your application is copied into the ``site\wwwroot`` folder.
 
 * Run Paket
 * Download Nuget dependencies
 * Kick off msbuild (installed by default)
 
Recommended practice is to use something like FAKE to orchestrate your build. FAKE also contains helpers for the Azure App Service "Kudu" feature, which performs diff copies between the "current" version of the application and the latest version.

### 3. Bind Azure App Service to repository.
Create a Web App in the [Azure App Service](https://azure.microsoft.com/en-us/services/app-service/) from the Azure portal. Then, [connect it to your source control repository of choice](https://azure.microsoft.com/en-us/documentation/articles/web-sites-publish-source-control/) e.g. GitHub, BitBucket etc. 

Azure will immediately download the source code and kick off the build script you specified in step 2. Every commit to the branch in future will kick off the same process.

Once hosted, you can use the Settings and Tools in the service to e.g. check deployments, view logs, scale out etc. etc.
