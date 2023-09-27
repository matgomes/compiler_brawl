FROM erlang:26

RUN mkdir /app
WORKDIR /app

COPY rebar.config rebar.config
COPY src src

RUN rebar3 escriptize

CMD ["./_build/default/bin/compiler_brawl", "/var/rinha/source.rinha.json"]
