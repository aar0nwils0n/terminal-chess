FROM ubuntu:18.04

ENV CHESS_DIR=/opt/chess
RUN apt-get update && apt-get install curl -y
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN PATH=/usr/local/bin/stack:$PATH
RUN mkdir -p ${CHESS_DIR}
COPY . ${CHESS_DIR}/
WORKDIR ${CHESS_DIR}/
RUN stack setup
RUN stack build

EXPOSE  9160
ENTRYPOINT ["stack", "exec", "chess-exe"]