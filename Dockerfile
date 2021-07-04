from ubuntu:20.04 as base-ubuntu

run apt -y update && apt -y upgrade && apt -y autoremove

run apt -y install --no-install-recommends sqlite3 libsqlite3-dev wordnet wordnet-grind

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

# Install Haskell Stack

from base-ubuntu as stack

run apt install -y wget
run wget -qO- https://get.haskellstack.org/ | sh
run stack update
run stack upgrade

# Compile the importer

from stack as wnimport

run stack new wnimport new-template -p "author-email:golubovsky@gmail.com" \
                                    -p "author-name:Dmitry Golubovsky" \
                                    -p "category:other" -p "copyright:none" \
                                    -p "github-username:dmgolubovsky"

workdir /wnimport

run stack setup

add wnimport/package.yaml .

add wnimport/stack.yaml .

run stack build --only-dependencies

add wnimport/Main.hs app
add wnimport/ImpWordIndex.hs app
add wnimport/ImpExcTable.hs app
add wnimport/IpaMap.hs app
add wnimport/IPA.hs app

run stack build

run stack install

# Build the classifier

from stack as wnclass

run stack new wnclass  new-template -p "author-email:golubovsky@gmail.com" \
                                    -p "author-name:Dmitry Golubovsky" \
                                    -p "category:other" -p "copyright:none" \
                                    -p "github-username:dmgolubovsky"

workdir /wnclass

run stack setup

add wnclass/package.yaml .

add wnclass/stack.yaml .

run stack build --only-dependencies

add wnclass/Main.hs app

run stack build

run stack install


# Build the rhymer

from stack as wnrhymer

run stack new wnrhymer  new-template -p "author-email:golubovsky@gmail.com" \
                                     -p "author-name:Dmitry Golubovsky" \
                                     -p "category:other" -p "copyright:none" \
                                     -p "github-username:dmgolubovsky"

workdir /wnrhymer

run stack setup

add wnrhymer/package.yaml .

add wnrhymer/stack.yaml .

run stack build --only-dependencies

add wnrhymer/Main.hs app

run stack build

run stack install


from base-ubuntu as hswn

run echo "APT::Get::Install-Recommends \"false\";" >> /etc/apt/apt.conf
run echo "APT::Get::Install-Suggests \"false\";" >> /etc/apt/apt.conf
run echo "APT::Install-Recommends \"false\";" >> /etc/apt/apt.conf
run echo "APT::Install-Suggests \"false\";" >> /etc/apt/apt.conf

run apt install -y sox libsonic0 strace locales

copy --from=espeak /espeak /espeak
copy --from=wnimport /root/.local/bin /root/.local/bin
copy --from=wnclass  /root/.local/bin /root/.local/bin
copy --from=wnrhymer /root/.local/bin /root/.local/bin


run /usr/sbin/locale-gen en_US.UTF-8

run apt clean

add scripts /usr/bin

# run /usr/bin/impwn

# Flatten the image

from scratch

copy --from=hswn / /
env PATH=/bin:/usr/bin:/usr/local/bin:/espeak/bin:/root/.local/bin
env LANG=en_US.UTF-8


