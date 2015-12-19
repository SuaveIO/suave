require 'bundler/setup'
require 'albacore'

desc 'build documentation'
task :build do
  system 'bundle exec jekyll build'
end

desc 'build and push docs'
task :push => :build do
  FileList['_site/{Gemfile,Gemfile.lock,Rakefile,.gitignore}'].each { |f| FileUtils.rm f }
  system "/usr/bin/sshpass -p #{ENV['SUAVE_SERVER_PASS']} scp -P #{ENV['SUAVE_SERVER_PORT']} -r _site/* suave@northpole.cloudapp.net:/home/suave/site", silent: true
end

task :default => :build

