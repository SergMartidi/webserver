FROM erlang:25.3


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