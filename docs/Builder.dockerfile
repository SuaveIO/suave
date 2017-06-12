FROM fsharp:4.1.18

ARG DOCKER_VERSION=17.03.0~ce-0~ubuntu-trusty
RUN apt-get -y update && \
    apt-get -y install apt-transport-https ca-certificates curl wget \
        software-properties-common python-software-properties \
        gcc python2.7 python-dev && \

    add-apt-repository ppa:git-core/ppa && \
    apt-get -y update && \
    apt-get -y install git && \

    curl -fsSL https://download.docker.com/linux/ubuntu/gpg | apt-key add - && \
    apt-key fingerprint 0EBFCD88D && \
    add-apt-repository \
        "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
        $(lsb_release -cs) \
        stable" && \
    apt-get -y update && \
    apt-get -y install docker-ce=$DOCKER_VERSION && \

    # Setup Google Cloud SDK (latest)
    mkdir -p /builder && \
    wget -qO- https://dl.google.com/dl/cloudsdk/release/google-cloud-sdk.tar.gz | tar zx -C /builder && \
    CLOUDSDK_PYTHON="python2.7" /builder/google-cloud-sdk/install.sh \
        --usage-reporting=false \
        --bash-completion=false \
        --disable-installation-options && \

    # Install components
    /builder/google-cloud-sdk/bin/gcloud -q components install beta && \
    /builder/google-cloud-sdk/bin/gcloud -q components update && \
    /builder/google-cloud-sdk/bin/gcloud -q components update kubectl && \

    # Clean up
    apt-get -y remove gcc python-dev python-setuptools && \
    rm -rf /var/lib/apt/lists/* && \
    rm -rf ~/.config/gcloud

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-add-repository ppa:brightbox/ruby-ng && \
    apt-get update -y && \
    apt-get install -q -y ruby2.2 ruby2.2-dev ruby-switch g++ build-essential curl git libssl-dev
RUN ruby-switch --set ruby2.2 && gem install bundler fpm --no-ri --no-rdoc
RUN gem install albacore fpm --no-rdoc --no-ri
RUN node -v && npm -v && ruby -v

ENV MONO_THREADS_PER_CPU 50 \
    MONO_TLS_PROVIDER btls \
    PATH=/builder/google-cloud-sdk/bin/:$PATH

ENTRYPOINT ["/bin/sh", "-c"]