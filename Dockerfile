FROM erlang:25.3

RUN apt-get update && apt-get install -y \
   openssl \
   libssl-dev \
   apt-utils \
   net-tools

# RUN /etc/init.d/postgresql restart

RUN mkdir -p /webserver

COPY src /webserver/src

COPY rebar.config /webserver

COPY Makefile /webserver

COPY config /webserver/config

COPY sftp-config.json /webserver

COPY docker-entry.sh /webserver


RUN cd /webserver && make && cd /

EXPOSE 5432

CMD  ["webserver/docker-entry.sh"]