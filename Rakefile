require 'bundler/setup'

require 'albacore'
require 'albacore/nuget_model'
require 'albacore/tasks/versionizer'
require 'albacore/tasks/release'
require 'albacore/task_types/nugets_pack'
require 'albacore/task_types/asmver'
require 'albacore/ext/teamcity'

require 'semver'

Albacore::Tasks::Versionizer.new :versioning

include ::Albacore::NugetsPack

suave_description = 'Suave is a simple web development F# library providing a lightweight web server and a set of combinators to manipulate route flow and task composition.'

desc "Restore paket.exe"
task :restore_paket do
  system 'tools/paket.bootstrapper.exe', clr_command: true
end

desc "Restore all packages"
task :restore => [:restore_paket] do
  system 'tools/paket.exe', 'restore', clr_command: true
end

desc 'create assembly infos'
asmver_files :asmver => :versioning do |a|
  a.files = FileList['{Suave,Tests,Pong,WebMachine,Example,Experimental,Load,Suave.*,suave.*}/*proj']
  a.attributes assembly_description: suave_description,
               assembly_configuration: 'Release',
               assembly_company: 'Suave.IO',
               assembly_copyright: "(c) #{Time.now.year} by Ademar Gonzalez, Henrik Feldt",
               assembly_version: ENV['LONG_VERSION'],
               assembly_file_version: ENV['LONG_VERSION'],
               assembly_informational_version: ENV['BUILD_VERSION']
end

desc 'Perform full build'
task :build => [:versioning, :restore, :asmver, :build_quick]

desc 'clean the project'
build :clean do |b|
  b.file = 'suave.sln'
  b.prop 'Configuration', 'Release'
  b.target = 'Clean'
end

build :build_quick do |b|
  b.file = 'suave.sln'
  b.prop 'Configuration', 'Release'
end

namespace :tests do
  desc 'run a stress test'
  task :stress do
    system 'Pong/bin/Release/Pong.exe', clr_command: true
  end

  task :unit do
    system 'Tests/bin/Release/Tests.exe', clr_command: true
  end
end

directory 'build/pkg'

task :create_nuget_quick => ['build/pkg', :versioning] do
  p = Albacore::NugetModel::Package.new.with_metadata do |m|
    m.id            = "Suave"
    m.version       = ENV['NUGET_VERSION']
    m.authors       = 'Ademar Gonzalez, Henrik Feldt'
    m.description   = suave_description
    m.language      = 'en-GB'
    m.copyright     = 'Ademar Gonzalez'
    m.release_notes = "Full version: #{ENV['BUILD_VERSION']}."
    m.license_url   = "https://github.com/ademar/suave/blob/master/COPYING"
    m.project_url   = "http://suave.io"
  end
  p.add_file  "Suave/bin/Release/suave.dll", "lib"
  p.add_file  "Suave/bin/Release/suave.xml", "lib"
  nuspec_path = 'suave.nuspec'
  File.write(nuspec_path,p.to_xml)
  cmd = Albacore::NugetsPack::Cmd.new "packages/NuGet.CommandLine/tools/NuGet.exe", out: "build/pkg"
  pkg, spkg = cmd.execute nuspec_path
  Albacore.publish :artifact, OpenStruct.new(
    :nuspec   => nuspec_path,
    :nupkg    => pkg,
    :location => pkg
  )
end

desc 'create suave nuget'
task :create_nuget => ['build/pkg', :versioning, :build, :create_nuget_quick]

desc 'build, gen versions, test and create nuget'
task :default => [:build, :'tests:unit']

task :increase_version_number do
  # inc patch version in .semver
  s = SemVer.find
  s.minor += 1
  s.save
  ENV['NUGET_VERSION'] = s.format("%M.%m.%p%s")
end

Albacore::Tasks::Release.new :release,
                             pkg_dir: 'build/pkg',
                             depend_on: :create_nuget,
                             nuget_exe: 'packages/NuGet.CommandLine/tools/NuGet.exe',
                             api_key: ENV['NUGET_KEY']

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
      work_dir: 'gh-pages',
      silent: true
  end
end
