FROM fsharp/fsharp

RUN apt-get update && \
    apt-get install -y rake bundler

ADD . /suave-build
WORKDIR /suave-build
RUN bundle
RUN bundle exec rake dotnetcli:restore dotnetcli:build
