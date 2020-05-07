from ubuntu:20.04 as base-ubuntu

run apt -y update && apt -y upgrade && apt -y autoremove

run apt -y install sqlite3 libsqlite3-dev

run mkdir /wn

volume /wn

add scripts /usr/bin

# Build espeak-ng

from base-ubuntu as espeak

run apt install -y autoconf automake libtool make libsonic-dev

workdir /src
add espeak-ng espeak-ng
workdir espeak-ng
run ./autogen.sh
run ./configure --prefix=/espeak
run make
run make install


from base-ubuntu as hswn

run echo "APT::Get::Install-Recommends \"false\";" >> /etc/apt/apt.conf
run echo "APT::Get::Install-Suggests \"false\";" >> /etc/apt/apt.conf
run echo "APT::Install-Recommends \"false\";" >> /etc/apt/apt.conf
run echo "APT::Install-Suggests \"false\";" >> /etc/apt/apt.conf


run apt install -y sox libsonic0 strace locales

copy --from=espeak /espeak /espeak

run /usr/sbin/locale-gen en_US.UTF-8

run apt clean

# Flatten the image

from scratch

copy --from=hswn / /
env PATH=/bin:/usr/bin:/usr/local/bin:/espeak/bin
env LANG=en_US.UTF-8
