def get_dependencies nuspec
  nuspec.metadata.dependencies.values.inject("") { |dependencies, d| dependencies << "\s\s\s\s#{d.id} >= #{d.version}\n" }
end

def get_files nuspec, proj_path
  nuspec.files.inject("") { |files, f| files << "    #{proj_path}\#{f.src} ==> #{f.target}" }
end

def create_nuspec proj, knowns
  Albacore::NugetModel::Package.from_xxproj proj, 
    symbols:        false,
    verify_files:   true,
    dotnet_version: 'net40', #this is the default used by albacore nugets_pack
    known_projects: knowns,
    version:        ENV['NUGET_VERSION'],
    configuration:  Configuration,
    project_dependencies: true,
    nuget_dependencies: true 
end
