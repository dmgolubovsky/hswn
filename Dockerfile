from ubuntu:20.04

run apt -y update && apt -y upgrade && apt -y autoremove

run apt -y install sqlite3 libsqlite3-dev

run mkdir /wn

volume /wn

add scripts /usr/bin

