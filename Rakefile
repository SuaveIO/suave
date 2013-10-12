require 'bundler/setup'

require 'albacore'
require 'albacore/tasks/versionizer'
require 'albacore/ext/teamcity'

Albacore::Tasks::Versionizer.new :versioning

nugets_restore :restore do |p|
  p.out = 'packages'
  p.exe = 'buildsupport/NuGet.exe'
end

desc "Perform full build"
build :build => [:versioning, :restore] do |b|
  b.sln = 'suave.sln'
end

directory 'build/pkg'

desc "package nugets"
nugets_pack :create_nugets => ['build/pkg', :versioning, :build] do |p|
  p.files   = FileList['**/*.{fsproj}'].
    exclude(/Tests/).
    exclude(/Spec/).
    exclude(/Example/).
    exclude(/packages/)
  p.out     = 'build/pkg'
  p.exe     = 'buildsupport/NuGet.exe'
  
  p.with_metadata do |m|
    m.version       = ENV['NUGET_VERSION']
    m.authors       = 'Ademar Gonzalez'
    m.description   = 'Suave is a simple web development F# library providing a lightweight web server and a set of combinators to manipulate route flow and task composition.'
    m.language      = 'en-GB'
    m.copyright     = 'Ademar Gonzalez'
    m.release_notes = "Full version: #{ENV['BUILD_VERSION']}."
  end
end

task :default => :create_nugets
