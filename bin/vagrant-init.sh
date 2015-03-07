#!/bin/bash

SCRIPT_DIRECTORY=$(dirname "${BASH_SOURCE[0]}")

ERL_V=17.4
REBAR_V=2.0

apt-get -y update
apt-get install -y libncurses5-dev libssl-dev curl git-core build-essential

mkdir -p /etc/gossiperl

# Install Erlang:

curl https://www.openssl.org/source/openssl-1.0.1g.tar.gz | tar xz && cd openssl-1.0.1g && sudo ./config && sudo make && sudo make install
sudo ln -sf /usr/local/ssl/bin/openssl `which openssl`
cd ..

mkdir -p /erlang

if [ ! -f /erlang/kerl ]; then
  echo "Installing Kerl"
  cd /erlang
  curl -O https://raw.githubusercontent.com/yrashk/kerl/master/kerl
  chmod a+x kerl
  chmod -R 0777 kerl
else
  echo "Kerl already installed"
fi

if [ ! -d /erlang/.erlang_${ERL_V} ]; then
  echo "Installing Erlang/OTP $ERL_V"
  mkdir -p /erlang/.erlang_${ERL_V}
  /erlang/kerl build  $ERL_V $ERL_V
  /erlang/kerl install $ERL_V /erlang/.erlang_${ERL_V}
  echo ". /erlang/.erlang_${ERL_V}/activate" >> /etc/bash.bashrc
else
  echo "Erlang/OTP ${ERL_V} already installed"
fi

chmod -R 0777 /erlang # let it be executed by anybody
. /erlang/.erlang_${ERL_V}/activate

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
