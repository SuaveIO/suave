# Encoding: utf-8

require 'bundler/setup'

require 'albacore'
require 'albacore/nuget_model'
require 'albacore/project'
require 'albacore/tools'
require 'albacore/tasks/versionizer'
require 'albacore/tasks/release'
require 'albacore/task_types/nugets_pack'
require 'albacore/task_types/asmver'
require 'albacore/ext/teamcity'
require './tools/paket_pack'
require 'semver'

Albacore::Tasks::Versionizer.new :versioning

include ::Albacore::NugetsPack

suave_description = 'Suave is a simple web development F# library providing a lightweight web server and a set of combinators to manipulate route flow and task composition.'

Configuration = ENV['CONFIGURATION'] || 'Release'
Platform = ENV['MSBUILD_PLATFORM'] || 'Any CPU'

task :yolo do
  system %{ruby -pi.bak -e "gsub(/module internal YoLo/, 'module internal Suave.Utils.YoLo')" paket-files/haf/YoLo/YoLo.fs} unless Albacore.windows?
  system %{ruby -pi.bak -e "gsub(/module internal YoLo/, 'module internal Suave.Utils.YoLo')" paket-files/examples/haf/YoLo/YoLo.fs} unless Albacore.windows?
end

desc "Restore paket.exe"
task :restore_paket do
  system 'tools/paket.bootstrapper.exe', clr_command: true unless
    File.exists? 'tools/paket.exe'
end

desc "Restore all packages"
task :restore => [:restore_paket, :yolo] do
  system 'tools/paket.exe', 'restore', clr_command: true
  system 'tools/paket.exe', %w|restore group Build|, clr_command: true
end

desc 'create assembly infos'
asmver_files :asmver => :versioning do |a|
  a.files = FileList[
    'examples/**/*.fsproj',
    'src/{Suave,Suave.*,Experimental}/*proj'
  ]
  a.attributes assembly_description: suave_description,
               assembly_configuration: Configuration,
               assembly_company: 'Suave.io',
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

  On Linux Ubuntu/Debian:
    sudo apt-get install automake libtool
    curl -sSL https://github.com/libuv/libuv/archive/v1.7.5.tar.gz | sudo tar zxfv - -C /usr/local/src
    cd /usr/local/src/libuv-1.7.5
    sudo sh autogen.sh
    sudo ./configure
    sudo make 
    sudo make install
    sudo rm -rf /usr/local/src/libuv-1.7.5 && cd ~/
    sudo ldconfig
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
end

namespace :dotnetcli do
  task :restore do
    system "dotnet restore"
  end

  # build Suave and test project
  task :build do
    Dir.chdir("src/Suave.DotnetCLI.Tests") do
      system "dotnet --verbose build"
    end
  end

  # create Suave nugets packages
  task :pack do
    Dir.chdir("src/Suave") do
      system "dotnet --verbose pack --configuration #{Configuration}"
    end
  end
 
  # merge standard and dotnetcli nupkgs
  task :merge do
    Dir.chdir("src/Suave") do
      #version = ENV['NUGET_VERSION']
      version = SemVer.find.format("%M.%m.%p%s")
      sourcenupkg = "../../build/pkg/Suave.#{version}.nupkg"
      clinupkg = "bin/#{Configuration}/Suave.#{version}-dotnetcli.nupkg"
      system %Q[dotnet mergenupkg --source "#{sourcenupkg}" --other "#{clinupkg}" --framework netstandard1.5]
    end
  end

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

task :create_nuget_quick => [:versioning, 'build/pkg'] do
  projects = FileList['src/**/*.fsproj'].exclude(/Tests/)
  knowns = Set.new(projects.map { |f| Albacore::Project.new f }.map { |p| p.id })
  authors = "Ademar Gonzalez, Henrik Feldt"
  projects.each do |f|
    p = Albacore::Project.new f
    n = create_nuspec p, knowns
    d = get_dependencies n
    fi = get_files n, p.proj_path_base
    m = %{type file
id #{p.id}
version #{ENV['NUGET_VERSION']}
title #{p.id}
authors #{authors}
owners #{authors}
description #{suave_description}
language en-GB
copyright #{authors}
licenseUrl https://github.com/SuaveIO/Suave/blob/master/COPYING
projectUrl http://suave.io
iconUrl https://raw.githubusercontent.com/SuaveIO/resources/master/images/head_trans.png
files
  #{p.proj_path_base}/bin/#{Configuration}/#{p.id}.* ==\> lib/net40
releaseNotes
  #{n.metadata.release_notes}
dependencies
  #{d}
}
    begin
      File.open("paket.template", "w") do |template|
        template.write m
      end
      system "tools/paket.exe", %w|pack output build/pkg|, clr_command: true
    ensure
      File.delete "paket.template"
    end
  end
end

desc 'create suave nuget'
task :nugets => ['build/pkg', :versioning, :compile, :create_nuget_quick]

desc 'compile, gen versions, test and create nuget'
task :appveyor => [:compile, :'tests:unit', :nugets]

desc 'compile, gen versions, test'
task :default => [:compile, :'tests:unit', :'docs:build']

task :increase_version_number do
  # inc patch version in .semver
  s = SemVer.find
  s.minor += 1
  s.save
  ENV['NUGET_VERSION'] = s.format("%M.%m.%p%s")
end

namespace :docs do
  desc 'clean generated documentation'
  task :clean do
    FileUtils.rm_rf 'docs/_site' if Dir.exists? 'docs/_site'
  end

  task :reference => :restore_paket do
    system 'packages/docs/FsLibTool/tools/FsLibTool.exe', %W|src docs/_site|, clr_command: true
  end

  task :jekyll do
    Dir.chdir 'docs' do
      Bundler.with_clean_env do
        system 'bundle'
        system 'bundle exec jekyll build'
      end
    end
  end

  desc 'build documentation'
  task :build => [:clean, :restore_paket, :jekyll, :reference]

  desc 'deploy the suave.io site'
  task :deploy => :build do
    # In ~/.ssh/config:
    # Host suave.io
    #    User suaveio
    #    IdentityFile ~/.ssh/suaveio_deployer
    system %{rsync -crvz --delete-after --exclude server --delete-excluded docs/_site/ suaveio@suave.io:}
  end
end

task :docs => :'docs:build'

Albacore::Tasks::Release.new :release,
                             pkg_dir: 'build/pkg',
                             depend_on: [:compile, :nugets, :'docs:deploy'],
                             nuget_exe: 'packages/build/NuGet.CommandLine/tools/NuGet.exe',
                             api_key: ENV['NUGET_KEY']
