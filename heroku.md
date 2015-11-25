---
layout: default
---

Deploying Suave to Heroku
----------------------------

Suave web sites can be as simple as a single F# script which starts a web server, or a full project.

Your application needs to be either a single script ``app.fsx`` (plus a Heroku ``Procfile`` and ``dummy.sln`` file) OR 
a directory with a ``.sln`` solution (plus a Heroku ``Procfile``)
	
Optionally, you can have a ``paket.dependencies`` OR ``packages.config`` files

Either way, your application must start a web server that binds to 0.0.0.0:$PORT.

Your ``Procfile`` must specify how the application starts.

If you don't have an app.fsx already that implements your website, then clone an example, putting it in a new directory (replace myproj by a unique project name)

You can one-click-deploy the Suave sample to Heroku by clicking the button below.

[![Deploy](https://www.herokucdn.com/deploy/button.png)](https://heroku.com/deploy?template=https://github.com/SuaveIO/heroku-getting-started)

Or deploy the sample from the CLI.


1. [Install the Heroku Toolbelt](https://toolbelt.heroku.com/) and login to Heroku using the command-line tools:

       heroku login

2. Clone the sample:

       git clone https://github.com/SuaveIO/heroku-getting-started.git myproj
       cd myproj

3. Create a new heroku web app and register "heroku" as a remote you can push to:

       heroku create myproj --buildpack https://github.com/SuaveIO/mono-script-buildpack.git 

4. Push!

        git push heroku master

When pushing, use an empty user name. You may need to use ``git auth:token`` to get a app token to use as a password here.

You can change the buildpack being used at a later date (e.g. to update to a later version of Mono) using 

    heroku buildpack:set https://github.com/your-build-pack-repo

If using github or bitbucket, find your app on Heroku and enable automatic deploy so you don't need to push explciitly.

You can look at logs from your web server script using ``heroku logs``.