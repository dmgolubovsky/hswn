from ubuntu:20.04 as base-ubuntu

run apt -y update && apt -y upgrade && apt -y autoremove

run apt -y install --no-install-recommends apt-utils sqlite3 libsqlite3-dev wordnet wordnet-grind wordnet-sense-index

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
run stack setup ghc-8.8.3

# Compile the wordnet test program

from stack as wntest

run stack new wntest new-template   -p "author-email:golubovsky@gmail.com" \
                                    -p "author-name:Dmitry Golubovsky" \
                                    -p "category:other" -p "copyright:none" \
                                    -p "github-username:dmgolubovsky"

workdir /wntest

add wntest/package.yaml .

add wntest/stack.yaml .

add wntest/WordNet-1.1.0 WordNet-1.1.0

run stack build --only-dependencies

add wntest/Main.hs app

run stack build

run stack install

# Compile the WN importer

from stack as wnimport

run stack new wnimport new-template -p "author-email:golubovsky@gmail.com" \
                                    -p "author-name:Dmitry Golubovsky" \
                                    -p "category:other" -p "copyright:none" \
                                    -p "github-username:dmgolubovsky"

workdir /wnimport

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

# Compile the lyrics importer

from stack as lyrimport

run stack new lyrimport new-template -p "author-email:golubovsky@gmail.com" \
                                     -p "author-name:Dmitry Golubovsky" \
                                     -p "category:other" -p "copyright:none" \
                                     -p "github-username:dmgolubovsky"

workdir /lyrimport

add lyrimport/package.yaml .

add lyrimport/stack.yaml .

run stack build --only-dependencies

add lyrimport/Main.hs app
add lyrimport/ImpLyrics.hs app
add lyrimport/IpaMap.hs app
add lyrimport/IPA.hs app

run stack build

run stack install

# Build the classifier

from stack as wnclass

run stack new wnclass  new-template -p "author-email:golubovsky@gmail.com" \
                                    -p "author-name:Dmitry Golubovsky" \
                                    -p "category:other" -p "copyright:none" \
                                    -p "github-username:dmgolubovsky"

workdir /wnclass

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

add wnrhymer/package.yaml .

add wnrhymer/stack.yaml .

run stack build --only-dependencies

add wnrhymer/Main.hs app

run stack build

run stack install

from base-ubuntu as lyrics

run apt install -y git

workdir /

run git clone https://github.com/Lyrics/lyrics.git

from base-ubuntu as hswn

run echo "APT::Get::Install-Recommends \"false\";" >> /etc/apt/apt.conf
run echo "APT::Get::Install-Suggests \"false\";" >> /etc/apt/apt.conf
run echo "APT::Install-Recommends \"false\";" >> /etc/apt/apt.conf
run echo "APT::Install-Suggests \"false\";" >> /etc/apt/apt.conf

run apt install -y sox libsonic0 strace locales less

copy --from=espeak /espeak /espeak
copy --from=lyrimport /root/.local/bin /root/.local/bin
copy --from=wnimport /root/.local/bin /root/.local/bin
copy --from=wnclass  /root/.local/bin /root/.local/bin
copy --from=wnrhymer /root/.local/bin /root/.local/bin
copy --from=wntest /root/.local/bin /root/.local/bin
copy --from=lyrics /lyrics/database /database


run /usr/sbin/locale-gen en_US.UTF-8

run apt clean

add scripts /usr/bin

# Temporarily suspend database import

# run /usr/bin/impwn

# Flatten the image

from scratch

copy --from=hswn / /
run find  /database -type f | xargs -d '\n' cat | grep -v '^$' | sort -u | wc -l
env PATH=/bin:/usr/bin:/usr/local/bin:/espeak/bin:/root/.local/bin
env LANG=en_US.UTF-8


