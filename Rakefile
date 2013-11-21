require 'bundler/setup'

require 'albacore'
require 'albacore/nuget_model'
require 'albacore/tasks/versionizer'
require 'albacore/task_types/nugets_pack'
require 'albacore/task_types/asmver'
require 'albacore/ext/teamcity'

Albacore::Tasks::Versionizer.new :versioning

include ::Albacore::NugetsPack

nugets_restore :restore do |p|
  p.out = 'packages'
  p.exe = 'buildsupport/NuGet.exe'
end

desc "Perform full build"
task :build => [:versioning, :restore, :build_quick]

build :build_quick do |b|
  b.file = 'suave.sln'
  b.prop 'Configuration', 'Release'
end

desc "Create a nuget for Suave"
task :create_nuget => ['build/pkg', :versioning, :build] do |p|
  p = Albacore::NugetModel::Package.new.with_metadata do |m|
    m.id            = "suave"
    m.version       = ENV['NUGET_VERSION']
    m.authors       = 'Ademar Gonzalez'
    m.description   = 'Suave is a simple web development F# library providing a lightweight web server and a set of combinators to manipulate route flow and task composition.'
    m.language      = 'en-GB'
    m.copyright     = 'Ademar Gonzalez'
    m.release_notes = "Full version: #{ENV['BUILD_VERSION']}."
    m.license_url       = "https://github.com/ademar/suave/blob/master/COPYING"
    m.project_url    = "http://suave.io"
  end
  p.add_file  "Suave/bin/Release/suave.dll", "lib"
  p.add_file  "Suave/bin/Release/suave.xml", "lib"
  p.add_file  "libs/ManagedOpenSsl.dll", "lib"
  p.add_file  "libs/ManagedOpenSsl.xml", "lib"
  p.add_file  "libs/ManagedOpenSsl.dll.config", "build/native"
  p.add_file  "libs/libeay32.dll", "build/native"
  p.add_file  "libs/ssleay32.dll", "build/native"
  p.add_file  "libs/libcrypto.so.1.0.0", "build/native"
  p.add_file  "libs/libssl.so.1.0.0", "build/native"
  p.add_file  "libs/libcrypto.1.0.0.dylib", "build/native"
  p.add_file  "libs/libssl.1.0.0.dylib", "build/native"
  p.add_file  "buildsupport/suave.targets", "build"
  nuspec_path = 'suave.nuspec'
  File.write(nuspec_path,p.to_xml)
  cmd = Albacore::NugetsPack::Cmd.new "buildsupport/NuGet.exe", out: "build/pkg"
  pkg, spkg = cmd.execute nuspec_path
end

desc "Create the assembly info file"
task :assembly_info do 
  x = Albacore::Asmver::Config.new()

  x.attributes assembly_version: SemVer.find.to_s,
    assembly_product: "Suave.IO",
    assembly_title: "Suave.IO Framework",
    assembly_description: "Suave is an F# library providing a lightweight web server and a set of combinators to manipulate route flow and task composition.",
    assembly_copyright: "(c) 2013 by Ademar Gonzalez",
    auto_open: "Suave.Utils"

  x.file_path = "Suave/AssemblyInfo.fs"
  x.namespace = "Suave"

  @task = ::Albacore::Asmver::Task.new(x.opts)
  @task.execute
end

task :increase_version_number do
  # inc patch version in .semver
  s = SemVer.find
  s.patch += 1
  s.save
end

desc 'release the next version'
task :release_next => [ :increase_version_number, :assembly_info , :create_nuget ] do
  s = SemVer.find
  # commit and tag
  system %w[git add .semver]
  system %w[git add Suave/AssemblyInfo.fs]
  system %W[git commit -m "released v#{s.to_s}"]
#  Rake::Tasks['build'].invoke
  system %w[buildsupport/NuGet.exe push #{ENV['NUGET_KEY']} build/pkg/suave.#{s.to_s}.nupkg]
end

task :default => :create_nuget
