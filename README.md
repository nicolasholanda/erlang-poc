Docker Compose for Erlang backend and Vite frontend

Quick steps to build and run both services using Docker Compose.

Requirements:
- Docker and Docker Compose (or Docker Desktop) installed.

Start (build and run):

Run the following in a shell:

    docker compose up --build

This will build the frontend image (Node builds Vite output, served by nginx on port 3000)
and the backend image (builds the Erlang app inside the image using `rebar3` and runs the Erlang VM exposing port 5555).

Access the frontend at http://localhost:3000 and the websocket endpoint at ws://localhost:5555/ws
