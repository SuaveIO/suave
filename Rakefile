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
Platform = ENV['MSBUILD_PLATFORM'] || 'Any CPU'
Prefer32Bits = ENV['PREFER_32_BITS'] || 'false'

desc "Restore paket.exe"
task :restore_paket do
  system 'tools/paket.bootstrapper.exe', clr_command: true unless
    File.exists? 'tools/paket.exe'
end

desc "Restore all packages"
task :restore => [:restore_paket] do
  system 'tools/paket.exe', 'restore', clr_command: true
end

desc 'create assembly infos'
asmver_files :asmver => :versioning do |a|
  a.files = FileList[
    'examples/**/*.fsproj',
    'src/{Suave,Suave.*,Experimental}/*proj'
  ]
  a.attributes assembly_description: suave_description,
               assembly_configuration: Configuration,
               assembly_company: 'Suave.IO',
               assembly_copyright: "(c) #{Time.now.year} by Ademar Gonzalez, Henrik Feldt",
               assembly_version: ENV['LONG_VERSION'],
               assembly_file_version: ENV['LONG_VERSION'],
               assembly_informational_version: ENV['BUILD_VERSION']
  a.handle_config do |proj, conf|
    conf.namespace = proj.namespace + "Asm"
    conf
  end
end

task :libs do
  unless Albacore.windows?
    system "pkg-config --cflags libuv" do |ok, res|
      if !ok
        raise %{
  You seem to be missing `libuv`, which needs to be installed.

  On OS X:
    brew install libuv --universal
    and then `export LD_LIBRARY_PATH=/usr/local/lib:/usr/lib:/lib`

  On Windows:
    @powershell -NoProfile -ExecutionPolicy Bypass -Command "Start-FileDownload 'https://github.com/libuv/libuv/archive/v1.7.5.zip'"
    7z x v1.7.5.zip & cd libuv-1.7.5 & vcbuild.bat x86 shared debug
    mkdir src\\Suave.Tests\\bin\\Release\\ & cp libuv-1.7.5\\Debug\\libuv.dll src\\Suave.Tests\\bin\\Release\\libuv.dll

  On Linux:
    ...
  }
      end
    end
  end
end

desc 'Perform full build'
task :compile => [:libs, :versioning, :restore, :asmver, :compile_quick]

desc 'clean the project'
build :clean do |b|
  b.file = 'src/Suave.sln'
  b.prop 'Configuration', Configuration
  b.target = 'Clean'
end

build :compile_quick do |b|
  b.file = 'src/Suave.sln'
  b.prop 'Configuration', Configuration
  b.prop 'Platform', Platform
  b.prop 'Prefer32Bits', Prefer32Bits
end

namespace :tests do
  task :stress_quick do
    system "examples/Pong/bin/#{Configuration}/Pong.exe", clr_command: true
  end

  desc 'run a stress test'
  task :stress => [:compile, :stress_quick]

  task :unit_quick do
    system "src/Suave.Tests/bin/#{Configuration}/Suave.Tests.exe", clr_command: true
  end

  desc 'run unit tests'
  task :unit => [:compile, :unit_quick]
end

desc 'run all tests (stress, unit)'
task :tests => [:'tests:stress', :'tests:unit']

directory 'build/pkg'

nugets_pack :create_nuget_quick => [:versioning, 'build/pkg'] do |p|
  p.configuration = Configuration
  p.files   = FileList['src/**/*.fsproj'].exclude(/Tests/)
  p.out     = 'build/pkg'
  p.exe     = 'packages/NuGet.CommandLine/tools/NuGet.exe'
  p.with_metadata do |m|
    m.version       = ENV['NUGET_VERSION']
    m.authors       = 'Ademar Gonzalez, Henrik Feldt'
    m.description   = suave_description
    m.language      = 'en-GB'
    m.copyright     = 'Ademar Gonzalez, Henrik Feldt'
    m.license_url   = "https://github.com/SuaveIO/Suave/blob/master/COPYING"
    m.project_url   = "http://suave.io"
    m.icon_url      = 'https://raw.githubusercontent.com/SuaveIO/resources/master/images/head_trans.png'
    # m.add_dependency 'Fuchu-suave', '0.5.0'
  end
end

desc 'create suave nuget'
task :nugets => ['build/pkg', :versioning, :compile, :create_nuget_quick]

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
                             depend_on: [:compile, :nugets],
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
