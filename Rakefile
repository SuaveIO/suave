require 'bundler/setup'

require 'albacore'
require 'albacore/nuget_model'
require 'albacore/tasks/versionizer'
require 'albacore/task_types/nugets_pack'
require 'albacore/task_types/asmver'
require 'albacore/ext/teamcity'

require 'semver'

Albacore::Tasks::Versionizer.new :versioning

include ::Albacore::NugetsPack

nugets_restore :restore do |p|
  p.out = 'packages'
  p.exe = 'buildsupport/NuGet.exe'
end

desc 'Perform full build'
task :build => [:versioning, :restore, :build_quick]

build :clean do |b|
  b.file = 'suave.sln'
  b.prop 'Configuration', 'Release'
  b.target = 'Clean'
end

build :build_quick do |b|
  b.file = 'suave.sln'
  b.prop 'Configuration', 'Release'
end

task :tests_quick do
  system 'Tests/bin/Release/Tests.exe', clr_command: true
end

task :tests => [:build, :tests_quick]

directory 'build/pkg'

desc 'Create a nuget for Suave'
task :create_nuget => [ 'build/pkg', :versioning, :build] do |p|
  p = Albacore::NugetModel::Package.new.with_metadata do |m|
    m.id            = "Suave"
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
  Albacore.publish :artifact, OpenStruct.new(
    :nuspec   => nuspec_path,
    :nupkg    => pkg,
    :location => pkg
  )
end

desc 'Create the assembly info file'
task :assembly_info do 
  x = Albacore::Asmver::Config.new()

  x.attributes assembly_version: SemVer.find.to_s,
    assembly_product: "Suave.IO",
    assembly_title: "Suave.IO Framework",
    assembly_description: "Suave is an F# library providing a lightweight web server and a set of combinators to manipulate route flow and task composition.",
    assembly_copyright: "(c) 2013 by Ademar Gonzalez, Henrik Feldt",
    auto_open: "Suave.Utils"

  x.file_path = "Suave/AssemblyInfo.fs"
  x.namespace = "Suave"

  @task = ::Albacore::Asmver::Task.new(x.opts)
  @task.execute
end

task :increase_version_number do
  # inc patch version in .semver
  s = SemVer.find
  s.minor += 1
  s.save
  ENV['NUGET_VERSION'] = s.format("%M.%m.%p%s")
end

desc 'release the next version'
task :release_next => [ :increase_version_number, :assembly_info , :create_nuget ] do
  s = SemVer.find.format("%M.%m.%p%s")
  # commit and tag
  system %q[git add .semver]
  system %q[git add Suave/AssemblyInfo.fs]
  system "git commit -m \"released v#{s.to_s}\""
#  Rake::Tasks['build'].invoke
  system "buildsupport/NuGet.exe setApiKey #{ENV['NUGET_KEY']}", clr_command: true
  system "buildsupport/NuGet.exe push build/pkg/suave.#{s.to_s}.nupkg", clr_command: true
end

namespace :docs do
  desc 'clean generated documentation'
  task :clean do
    FileUtils.rm_rf 'gh-pages' if Dir.exists? 'gh-pages'
  end

  desc 'build documentation'
  task :build => :clean do
    system 'git clone https://github.com/SuaveIO/suave.git -b gh-pages gh-pages' unless Dir.exists? 'gh-pages'
    Dir.chdir 'gh-pages' do
      Bundler.with_clean_env do
        system 'bundle'
        system 'bundle exec jekyll build'
      end
    end
  end

  desc 'build and push docs'
  task :push => :'docs:build' do
    system "sshpass -p #{ENV['SUAVE_SERVER_PASS']} scp -P #{ENV['SUAVE_SERVER_PORT']} -r _site/* suave@northpole.cloudapp.net:/home/suave/site",
      work_dir: 'gh-pages'
  end
end

namespace :tests do
  desc 'run a stress test'
  task :stress do
    system 'Pong/bin/Release/Pong.exe', clr_command: true
  end

  desc 'run the unit-tests'
  task :unit do
    system 'Tests/bin/Release/Tests.exe', clr_command: true
  end
end

task :default => [:build, :'tests:unit', :create_nuget]

