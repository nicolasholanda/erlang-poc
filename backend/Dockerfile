FROM debian:bookworm-slim

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
	   curl ca-certificates git build-essential \
	&& rm -rf /var/lib/apt/lists/*

RUN curl -fSL -o /usr/local/bin/rebar3 https://github.com/erlang/rebar3/releases/download/3.21.0/rebar3 \
	&& chmod +x /usr/local/bin/rebar3

RUN apt-get update && apt-get install -y --no-install-recommends erlang-nox && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy project files and build with rebar3 inside the image
COPY rebar.config ./
COPY src ./src
RUN rebar3 update && rebar3 compile

EXPOSE 5555

CMD erl -noshell -pa _build/default/lib/*/ebin -eval "application:ensure_all_started(chat_poc), io:format(\"Cowboy WebSocket listener started on port 5555~n\"), timer:sleep(infinity)."
