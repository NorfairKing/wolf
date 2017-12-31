FROM fpco/ubuntu-with-libgmp:14.04

ARG PORT=8000
ARG API_PORT=8001

run apt-get -y update
run apt-get -y install software-properties-common python-software-properties
run add-apt-repository -y ppa:git-core/ppa
run apt-get -y update
run apt-get -y install git

RUN mkdir -p /www/bin /www/wolf-data
WORKDIR /www

EXPOSE ${PORT}
ENV PORT ${PORT}
ENV API_PORT ${API_PORT}
ENV DATA_DIR /www/wolf-data

ADD dist/wolf-web-server /www/bin/wolf-web-server
RUN chmod +x /www/bin/wolf-web-server

CMD /www/bin/wolf-web-server serve
