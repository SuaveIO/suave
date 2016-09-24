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
require './tools/paket_pack'
require 'semver'

Albacore::Tasks::Versionizer.new :versioning

include ::Albacore::NugetsPack

suave_description = 'Suave is a simple web development F# library providing a lightweight web server and a set of combinators to manipulate route flow and task composition.'

Configuration = ENV['CONFIGURATION'] || 'Release'
Platform = ENV['MSBUILD_PLATFORM'] || 'Any CPU'

task :paket_files do
  sh %{ruby -pi.bak -e "gsub(/module internal YoLo/, 'module internal Suave.Utils.YoLo')" paket-files/haf/YoLo/YoLo.fs}
  sh %{ruby -pi.bak -e "gsub(/module internal YoLo/, 'module internal Suave.Utils.YoLo')" paket-files/examples/haf/YoLo/YoLo.fs}
  sh %{ruby -pi.bak -e "gsub(/namespace Logary.Facade/, 'namespace Suave.Logging')" paket-files/logary/logary/src/Logary.Facade/Facade.fs}
end

desc "Restore paket.exe"
task :restore_paket do
  system 'tools/paket.bootstrapper.exe', clr_command: true unless
    File.exists? 'tools/paket.exe'
end

task :paket_restore do
  system 'tools/paket.exe', 'restore', clr_command: true
  system 'tools/paket.exe', %w|restore group Build|, clr_command: true
end

desc 'Restore all packages'
task :restore => [:restore_paket, :paket_restore, :paket_files]

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
  You seem to be missing `libuv`, which needs to be installed. See https://github.com/SuaveIO/suave#libuv-installation
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
  directory 'tools/coreclr'

  dotnet_exe_path = "dotnet" #from PATH

  def get_installed_dotnet_version
    begin
      installed_dotnet_version = `dotnet --version`
      return "" unless $?.success?
      if installed_dotnet_version.nil? then "" else installed_dotnet_version.strip end
    rescue
      ""
    end
  end

  task :coreclr_binaries => 'tools/coreclr' do
    dotnet_version = '1.0.0-preview3-003686'
    dotnet_installed_version = get_installed_dotnet_version
    # check if required version of .net core sdk is already installed, otherwise download and install it
    if dotnet_installed_version == dotnet_version then
      puts ".NET Core SDK #{dotnet_version} already installed. Skip install"
      dotnet_exe_path = "dotnet"
      next
    elsif dotnet_installed_version == "" then
      puts ".NET Core SDK #{dotnet_version} not found. Downloading and installing in ./tools/coreclr"
    else
      puts "Found .NET Core SDK #{dotnet_installed_version} but require #{dotnet_version}. Downloading and installing in ./tools/coreclr"
    end

    case RUBY_PLATFORM
    when /darwin/
      filename = "dotnet-dev-osx-x64.#{dotnet_version}.tar.gz"
      system 'curl',
        %W|-o tools/#{filename}
           -L https://dotnetcli.blob.core.windows.net/dotnet/preview/Binaries/#{dotnet_version}/#{filename}| \
        unless File.exists? "tools/#{filename}"

      system 'tar',
        %W|xf tools/#{filename}
           --directory tools/coreclr|
    when /linux/
      filename = "dotnet-dev-ubuntu.14.04-x64.#{dotnet_version}.tar.gz"
      system 'curl',
        %W|-o tools/#{filename}
           -L https://dotnetcli.blob.core.windows.net/dotnet/preview/Binaries/#{dotnet_version}/#{filename}| \
        unless File.exists? "tools/#{filename}"

      system 'tar',
        %W|xf tools/#{filename}
           --directory tools/coreclr|
    end
    if Gem.win_platform?
      system 'powershell',
        %W|Invoke-WebRequest "https://raw.githubusercontent.com/dotnet/cli/rel/1.0.0-preview2/scripts/obtain/dotnet-install.ps1" -OutFile "dotnet_cli_install.ps1"|
      system 'powershell',
        %W|-ExecutionPolicy Unrestricted ./dotnet_cli_install.ps1 -InstallDir "tools/coreclr" -Channel "beta" -version "#{dotnet_version}"|
    end

    dotnet_exe_path = "#{Dir.pwd}/tools/coreclr/dotnet"
  end

  desc 'Restore the CoreCLR binaries'
  task :restore => :coreclr_binaries do
    Dir.chdir "src" do
      system dotnet_exe_path, "restore"
    end
  end

  task :build_lib => :coreclr_binaries do
    Dir.chdir "src/Suave" do
      system dotnet_exe_path, %W|--verbose build --configuration #{Configuration} -f netstandard1.6|
    end
  end

  desc 'Build Suave and test project'
  task :build => [:build_lib]

  desc 'Create Suave nugets packages'
  task :pack => :coreclr_binaries do
    Dir.chdir "src/Suave"  do
      system dotnet_exe_path, %W|--verbose pack --configuration #{Configuration} --no-build|
    end
  end

  task :do_netcorepackage => [ :restore, :build, :pack ]

  desc 'Merge standard and dotnetcli nupkgs; note the need to run :nugets before'
  task :merge => :coreclr_binaries do
    Dir.chdir("src/Suave") do
      version = SemVer.find.format("%M.%m.%p%s")
      sourcenupkg = "../../build/pkg/Suave.#{version}.nupkg"
      clinupkg = "bin/#{Configuration}/Suave.#{version}-dotnetcli.nupkg"
      system dotnet_exe_path, %W|mergenupkg --source "#{sourcenupkg}" --other "#{clinupkg}" --framework netstandard1.6|
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
  #{n.metadata.release_notes.each_line.reject{|x| x.strip == ""}.join}
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

desc 'create suave nuget with .NET Core'
task :nugets_with_netcore => [:nugets, 'dotnetcli:do_netcorepackage', 'dotnetcli:merge']

desc 'compile, gen versions, test and create nuget'
task :appveyor => [:compile, :'tests:unit', :nugets_with_netcore]

desc 'compile, gen versions, test'
task :default => [:compile, :'tests:unit', :'docs:build']

task :increase_version_number do
  # inc patch version in .semver
  s = SemVer.find
  s.minor += 1
  s.save
  version = s.format("%M.%m.%p%s")
  ENV['NUGET_VERSION'] = version
  projectjson = 'src/Suave/project.json'
  contents = File.read(projectjson).gsub(/"version": ".*-dotnetcli"/, %{"version": "#{version}-dotnetcli"})
  File.open(projectjson, 'w') do |out|
    out << contents
  end
end

namespace :docs do
  desc 'clean generated documentation'
  task :clean do
    FileUtils.rm_rf 'docs/_site' if Dir.exists? 'docs/_site'
  end

  task :reference => :restore_paket do
    system 'packages/docs/FsLibTool/tools/FsLibTool.exe', %W|src docs/_site|, clr_command: true
    puts "Reference docs generated successfully."
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
                             depend_on: [:compile, :nugets_with_netcore, :'docs:deploy'],
                             nuget_exe: 'packages/build/NuGet.CommandLine/tools/NuGet.exe',
                             api_key: ENV['NUGET_KEY']
