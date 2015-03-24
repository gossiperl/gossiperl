#!/bin/bash

SCRIPT_DIRECTORY=$(dirname "${BASH_SOURCE[0]}")

ERL_V=17.4
REBAR_V=2.0
OPENSSL_V=1.0.1m

ERLANG_DIR=/etc/gossiperl/erlang

mkdir -p $ERLANG_DIR

if [ ! -f $ERLANG_DIR/apt ]; then
  echo "Changing APT settings..."
  sudo rm -Rf /etc/apt/sources.list
  echo "deb mirror://mirrors.ubuntu.com/mirrors.txt precise main restricted universe multiverse" >> /etc/apt/sources.list
  echo "deb mirror://mirrors.ubuntu.com/mirrors.txt precise-updates main restricted universe multiverse" >> /etc/apt/sources.list
  echo "deb mirror://mirrors.ubuntu.com/mirrors.txt precise-backports main restricted universe multiverse" >> /etc/apt/sources.list
  echo "deb mirror://mirrors.ubuntu.com/mirrors.txt precise-security main restricted universe multiverse" >> /etc/apt/sources.list
  echo "APT settings changed."
  apt-get -y update
  apt-get install -y libncurses5-dev libssl-dev curl git-core build-essential
  echo $(date) >> /etc/gossiperl/apt
else
  echo "APT settings already applied"
fi

if [ ! -f /etc/gossiperl/openssl ]; then
  echo "Installing OpenSSL $OPENSSL_V"
  curl https://www.openssl.org/source/openssl-$OPENSSL_V.tar.gz | tar xz && cd openssl-$OPENSSL_V && sudo ./config && sudo make && sudo make install
  sudo ln -sf /usr/local/ssl/bin/openssl `which openssl`
  cd ..
  echo $(date) >> /etc/gossiperl/openssl
else
  echo "OpenSSL $OPENSSL_V already installed"
fi

if [ ! -f /etc/gossiperl/multicast ]; then
  echo "Enabling multicast"
  ifconfig eth1 multicast
  route add -net 224.0.0.0 netmask 224.0.0.0 eth1
  echo $(date) >> /etc/gossiperl/multicast
else
  echo "Multicast already enabled"
fi

if [ ! -f $ERLANG_DIR/kerl ]; then
  echo "Installing Kerl"
  cd $ERLANG_DIR
  curl -O https://raw.githubusercontent.com/yrashk/kerl/master/kerl
  chmod a+x kerl
  chmod -R 0777 kerl
else
  echo "Kerl already installed"
fi

if [ ! -d $ERLANG_DIR/.erlang_${ERL_V} ]; then
  echo "Installing Erlang/OTP $ERL_V"
  mkdir -p $ERLANG_DIR/.erlang_${ERL_V}
  $ERLANG_DIR/kerl build  $ERL_V $ERL_V
  $ERLANG_DIR/kerl install $ERL_V $ERLANG_DIR/.erlang_${ERL_V}
  echo ". $ERLANG_DIR/.erlang_${ERL_V}/activate" >> /etc/bash.bashrc
else
  echo "Erlang/OTP ${ERL_V} already installed"
fi

chmod -R 0777 $ERLANG_DIR # let it be executed by anybody
. $ERLANG_DIR/.erlang_${ERL_V}/activate

# Install rebar
if [ ! -f /etc/gossiperl/rebar ]; then
  echo "Installing Rebar $REBAR_V"
  # cleanup first:
  rm -Rf ~/.rebar
  rm -Rf /usr/bin/rebar
  # we will store rebar in:
  mkdir -p ~/.rebar/rebar-src
  cd ~/.rebar/rebar-src
  # clone and ensure branch:
  git clone https://github.com/basho/rebar.git .
  git fetch origin
  git checkout -b $REBAR_V origin/$REBAR_V
  /bin/sleep 5
  # bootstrap - make sure Erlang is available:
  ./bootstrap || exit 100
  echo -e "Rebar bootstrapped."
  while [ -z "$(ls -la . | grep ' rebar$')" ]; do
    echo " -> rebar build not found yet"
    /bin/sleep 1
  done
  echo -e "Rebar build found"
  cp rebar ../rebar-$REBAR_V
  cd ..
  ln -s rebar-$REBAR_V ./rebar-current
  ln -s $(pwd)/rebar-current /usr/bin/rebar
  echo $(date) >> /etc/gossiperl/rebar
else
  echo "Rebar $REBAR_V installed."
fi

# Install Ruby:

# if [ ! -f /etc/gossiperl/ruby ]; then
#   echo "Installing latest Ruby 1.9"
#   apt-get -y --force-yes install ruby1.9.1 ruby1.9.1-dev
#   update-alternatives --set gem /usr/bin/gem1.9.1
#   gem install bundler --no-ri --no-rdoc
#   echo $(date) >> /etc/gossiperl/ruby
# else
#   echo "Ruby already installed."
# fi

# # Install Java:
# if [ ! -f /etc/gossiperl/java ]; then
#   echo "Installing Java"
#   apt-get -y install openjdk-7-jdk maven
#   echo $(date) >> /etc/gossiperl/java
# else
#   echo "Java already installed."
# fi

# # Install mono:
# if [ ! -f /etc/gossiperl/mono ]; then
#   echo "Installing mono"
#   apt-get -y install mono-complete
#   echo $(date) >> /etc/gossiperl/mono
# else
#   echo "mono already installed."
# fi
