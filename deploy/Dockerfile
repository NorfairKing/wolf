FROM fpco/ubuntu-with-libgmp:14.04

ARG PORT=80

run apt-get -y update
run apt-get -y install software-properties-common python-software-properties
run add-apt-repository -y ppa:git-core/ppa
run apt-get -y update
run apt-get -y install git

RUN mkdir -p /www/bin /www/wolf-data
WORKDIR /www
ADD dist/wolf-web-server /www/bin/wolf-web-server
RUN chmod +x /www/bin/wolf-web-server

EXPOSE ${PORT}

ENV PORT ${PORT}
ENV SHARED_DATA_DIR /www/wolf-data

CMD /www/bin/wolf-web-server serve
