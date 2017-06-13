FROM eu.gcr.io/$PROJECT_ID/suaveio-docs-builder:latest

ADD . /workspace
WORKDIR /workspace
RUN bundle
RUN bundle exec rake build:docs
