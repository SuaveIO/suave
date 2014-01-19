require 'bundler/setup'
require 'albacore'

desc 'build documentation'
task :build do
  system 'bundle exec jekyll build'
end

desc 'build and push docs'
task :push do
  system '/usr/bin/sshpass', %W[-p #{ENV['SUAVE_SERVER_PASS']} scp -P #{ENV['SUAVE_SERVER_PORT']} -r _site/* suave@northpole.cloudapp.net:/home/suave/site],
    work_dir: 'gh-pages'
end

task :default => :build

