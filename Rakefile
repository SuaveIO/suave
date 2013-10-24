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
  b.file = 'suave.sln'
  b.prop 'Configuration', 'Release'
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
  p.configuration = 'Release'
  
  p.with_metadata do |m|
    m.version       = ENV['version']
    m.authors       = 'Ademar Gonzalez'
    m.description   = 'Suave is a simple web development F# library providing a lightweight web server and a set of combinators to manipulate route flow and task composition.'
    m.language      = 'en-GB'
    m.copyright     = 'Ademar Gonzalez'
    m.release_notes = "Full version: #{ENV['BUILD_VERSION']}."
    m.license_url       = "https://github.com/ademar/suave/blob/master/COPYING"
    m.project_url    = "https://github.com/ademar/suave"
  end
end

task :default => :create_nugets
