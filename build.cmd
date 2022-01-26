dotnet restore build.proj
dotnet tool install fake-cli
dotnet fake build %*
