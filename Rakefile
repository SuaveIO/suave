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

Configuration = ENV['CONFIGURATION'] || 'Release'

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
               assembly_configuration: Configuration,
               assembly_company: 'Suave.IO',
               assembly_copyright: "(c) #{Time.now.year} by Ademar Gonzalez, Henrik Feldt",
               assembly_version: ENV['LONG_VERSION'],
               assembly_file_version: ENV['LONG_VERSION'],
               assembly_informational_version: ENV['BUILD_VERSION']
end

desc 'Perform full build'
task :compile => [:versioning, :restore, :asmver, :compile_quick]

desc 'clean the project'
build :clean do |b|
  b.file = 'suave.sln'
  b.prop 'Configuration', Configuration
  b.target = 'Clean'
end

build :compile_quick do |b|
  b.file = 'suave.sln'
  b.prop 'Configuration', Configuration
end

namespace :tests do
  task :stress_quick do
    system "Pong/bin/#{Configuration}/Pong.exe", clr_command: true
  end

  desc 'run a stress test'
  task :stress => [:compile, :stress_quick]

  task :unit_quick do
    system "Tests/bin/#{Configuration}/Tests.exe", clr_command: true
  end

  desc 'run unit tests'
  task :unit => [:compile, :unit_quick]
end

desc 'run all tests (stress, unit)'
task :tests => [:'tests:stress', :'tests:unit']

directory 'build/pkg'

task :create_nuget_quick => ['build/pkg', :versioning] do
  p = Albacore::NugetModel::Package.new.with_metadata do |m|
    m.id            = "Suave"
    m.version       = ENV['NUGET_VERSION']
    m.authors       = 'Ademar Gonzalez, Henrik Feldt'
    m.description   = suave_description
    m.language      = 'en-GB'
    m.copyright     = 'Ademar Gonzalez, Henrik Feldt'
    m.release_notes = "Full version: #{ENV['BUILD_VERSION']}."
    m.license_url   = "https://github.com/ademar/suave/blob/master/COPYING"
    m.project_url   = "http://suave.io"
    m.add_dependency 'FsPickler', '1.0.3'
  end
  p.add_file  "Suave/bin/#{Configuration}/suave.dll", "lib"
  p.add_file  "Suave/bin/#{Configuration}/suave.xml", "lib"
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

nugets_pack :create_nuget_testing_quick do |p|
  p.configuration = Configuration
  p.files   = FileList['src/Suave.Testing/Suave.Testing.fsproj'].
    exclude(/Tests/)
  p.out     = 'build/pkg'
  p.exe     = 'packages/NuGet.CommandLine/tools/NuGet.exe'
  p.with_metadata do |m|
    m.id            = "Suave.Testing"
    m.version       = ENV['NUGET_VERSION']
    m.authors       = 'Ademar Gonzalez, Henrik Feldt'
    m.description   = suave_description
    m.language      = 'en-GB'
    m.copyright     = 'Ademar Gonzalez, Henrik Feldt'
    m.release_notes = "Full version: #{ENV['BUILD_VERSION']}."
    m.license_url   = "https://github.com/ademar/suave/blob/master/COPYING"
    m.project_url   = "http://suave.io"
    m.add_dependency 'Fuchu-suave', '0.5.0'
  end
end

desc 'create suave nuget'
task :create_nuget => ['build/pkg', :versioning, :compile, :create_nuget_quick, :create_nuget_testing_quick]

desc 'compile, gen versions, test and create nuget'
task :default => [:compile, :'tests:unit']

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
  task :compile => :clean do
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
