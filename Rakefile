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
require 'semver'

Albacore::Tasks::Versionizer.new :versioning

include ::Albacore::NugetsPack

suave_description = 'Suave is a simple web development F# library providing a lightweight web server and a set of combinators to manipulate route flow and task composition.'

Configuration = ENV['CONFIGURATION'] || 'Release'
Platform = ENV['MSBUILD_PLATFORM'] || 'Any CPU'

task :paket_files do
  sh %{ruby -pi.bak -e "gsub(/module YoLo/, 'module internal Suave.Utils.YoLo')" paket-files/haf/YoLo/YoLo.fs}
  sh %{ruby -pi.bak -e "gsub(/module YoLo/, 'module internal Suave.Utils.YoLo')" paket-files/examples/haf/YoLo/YoLo.fs}
  sh %{ruby -pi.bak -e "gsub(/module YoLo/, 'module internal Suave.Utils.YoLo')" paket-files/docs/haf/YoLo/YoLo.fs}
  sh %{ruby -pi.bak -e "gsub(/namespace Logary.Facade/, 'namespace Suave.Logging')" paket-files/logary/logary/src/Logary.Facade/Facade.fs}
end

desc "Restore paket.exe"
task :restore_paket do
  system '.paket/paket.bootstrapper.exe', clr_command: true unless
    File.exists? '.paket/paket.exe'
end

task :paket_restore do
  system '.paket/paket.exe', 'restore', clr_command: true
  system '.paket/paket.exe', %w|restore group Build|, clr_command: true
end

desc 'Restore all packages'
task :restore => [:restore_paket, :paket_restore, :paket_files]

desc 'create assembly infos'
asmver_files :asmver => :versioning do |a|
  a.files = FileList[
    'examples/**/*.fsproj',
    'src/{Suave,Suave.*,Experimental}/*proj'
  ].exclude(/.netcore.fsproj/)
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

desc 'Perform full build'
task :compile => [:versioning, :restore, :asmver, :compile_quick]

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
    dotnet_version = '2.0.3'
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

    coreclr_bin_dir = File.expand_path "tools/coreclr"

    dotnet_exe_path = "dotnet"

    case RUBY_PLATFORM
    when /darwin/ , /linux/
      system 'curl',
        %W|-o tools/dotnet-install.sh
           -L https://dot.net/v1/dotnet-install.sh| \
        unless File.exists? "tools/dotnet-install.sh"

      system 'bash',
        %W|tools/dotnet-install.sh
           --install-dir tools/coreclr
           --channel stable
           --version #{dotnet_version}|

      ENV['PATH'] = "#{coreclr_bin_dir}:#{ENV['PATH']}"
    else
      system 'powershell',
        %W|Invoke-WebRequest "https://dot.net/v1/dotnet-install.ps1" -OutFile "tools/dotnet-install.ps1"| \
        unless File.exists? "tools/dotnet-install.ps1"

      system 'powershell',
        %W|-ExecutionPolicy Unrestricted ./tools/dotnet-install.ps1
           -InstallDir "#{coreclr_bin_dir}"
           -Channel "stable"
           -Version "#{dotnet_version}"|

      ENV['PATH'] = "#{coreclr_bin_dir};#{ENV['PATH']}"
    end

    system 'dotnet',
      %W|--info|

  end

  desc 'Restore the CoreCLR binaries'
  task :restore => :coreclr_binaries do
    system dotnet_exe_path, %W|restore src/Suave.netcore.sln -v n|
  end

  task :build_lib => :coreclr_binaries do
    system dotnet_exe_path, %W|build src/Suave.netcore.sln -c #{Configuration} -v n|
  end

  desc 'Build Suave and test project'
  task :build => [:build_lib]

  directory 'build/pkg-netcore'

  desc 'Create Suave nugets packages'
  task :pack  => [:versioning, 'build/pkg-netcore', :coreclr_binaries] do
    out_dir = File.expand_path "build/pkg-netcore"
    [ "src/Suave/Suave.netcore.fsproj", "src/Suave.Testing/Suave.Testing.netcore.fsproj", "src/Experimental/Suave.Experimental.netcore.fsproj", "src/Suave.DotLiquid/Suave.DotLiquid.netcore.fsproj", "src/Suave.LibUv/Suave.LibUv.netcore.fsproj" ].each do |item|
        system dotnet_exe_path, %W|pack #{item} --configuration #{Configuration} --output "#{out_dir}" --no-build -v n /p:Version=#{ENV['NUGET_VERSION']}|
    end
  end

  task :do_netcorepackage => [ :restore, :build, :pack ]

  def merge_nugets(dotnet_exe_path, item, version, framework)
    sourcenupkg = "../build/pkg/#{item}.#{version}.nupkg"
    netcorenupkg = "../build/pkg-netcore/#{item}.#{version}.nupkg"
    system dotnet_exe_path, %W|mergenupkg --source "#{sourcenupkg}" --other "#{netcorenupkg}" --framework #{framework}|
  end

  desc 'Merge standard and dotnetcli nupkgs; note the need to run :nugets before'
  task :merge => :coreclr_binaries do    
    system dotnet_exe_path, %W|restore tools/tools.proj -v n|
    version = SemVer.find.format("%M.%m.%p%s")

    Dir.chdir "tools" do
      [ "Suave", "Suave.Testing", "Suave.Experimental", "Suave.DotLiquid", "Suave.LibUv" ].each do |item|
          merge_nugets(dotnet_exe_path, item, version, "netstandard2.0")
      end
    end
  end

  task :stress_quick do
    system dotnet_exe_path, %W|run --project examples/Pong/Pong.netcore.fsproj|
  end

  desc 'run a stress test'
  task :stress => [:build_lib, :stress_quick]

  task :unit_quick do
    system dotnet_exe_path, %W|run --project src/Suave.Tests/Suave.Tests.netcore.fsproj -- --sequenced|
  end

  desc 'run unit tests on .NET Core 2.0'
  task :unit => [:build_lib, :unit_quick]

end

namespace :tests do
  task :stress_quick do
    system "examples/Pong/bin/#{Configuration}/Pong.exe", clr_command: true
  end

  desc 'run a stress test'
  task :stress => [:compile, :stress_quick]

  task :unit_quick do
    system "src/Suave.Tests/bin/#{Configuration}/Suave.Tests.exe", %w|--sequenced|, clr_command: true
  end

  desc 'run unit tests'
  task :unit => [:compile, :unit_quick]
end

desc 'run all tests (stress, unit)'
task :tests => [:'tests:stress', :'tests:unit']

directory 'build/pkg'

task :nugets_quick => [:versioning, 'build/pkg'] do
  FileList['src/**/*.fsproj'].exclude(/.netcore.fsproj/).exclude(/Tests/).each do |proj|
    taskName = "nugets_quick:" + proj
    nugets_pack taskName do |x|
      x.configuration = Configuration
      x.exe = '.paket/paket.exe'
      x.files = FileList[proj]
      x.output = 'build/pkg'
      x.authors = "Ademar Gonzalez, Henrik Feldt"
      x.version = ENV['NUGET_VERSION']
      x.description = suave_description
      x.language = 'en-GB'
      x.license_url = 'https://github.com/SuaveIO/Suave/blob/master/COPYING'
      x.project_url = 'https://suave.io'
      x.icon_url = 'https://raw.githubusercontent.com/SuaveIO/resources/master/images/head_trans.png'
    end
    Rake::Task[taskName].invoke
  end
end

desc 'create suave nuget'
task :nugets => ['build/pkg', :versioning, :compile, :nugets_quick]

desc 'create suave nuget with .NET Core'
task :nugets_with_netcore => [:nugets, 'dotnetcli:do_netcorepackage', 'dotnetcli:merge']

desc 'compile, gen versions, test and generate nuget'
task :ci => [:compile, :'tests:unit', :'dotnetcli:unit', :nugets_with_netcore]

desc 'compile, gen versions, test'
task :default => [:compile, :'tests:unit', :'docs:build']

task :increase_version_number do
  # inc patch version in .semver
  s = SemVer.find
  s.minor += 1
  s.save
  version = s.format("%M.%m.%p%s")
  ENV['NUGET_VERSION'] = version
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

  task :server do
    Dir.chdir 'docs' do
      system 'xbuild', %w|/p:Configuration=Release server/server.fsproj|
    end
  end

  desc 'build documentation'
  task :build => [:clean, :restore_paket, :jekyll, :server, :reference]

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
                             nuget_exe: '.paket/paket.exe',
                             api_key: ENV['NUGET_KEY']
