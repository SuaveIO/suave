dotnet restore build.proj
dotnet tool install fake-cli --version 5.20.4
dotnet fake build %*
