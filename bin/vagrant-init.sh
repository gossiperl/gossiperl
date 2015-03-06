#!/bin/bash
ERL_V=17.4

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

chmod a+x /vagrant/bin/*.sh
chmod a+x /vagrant/bin/rebar/*.sh
chmod a+x /vagrant/bin/utils/*.sh

if [ -d /vagrant/clients/bin ]; then
  chmod a+x /vagrant/clients/bin/*.sh
fi

chmod -R 0777 /erlang # let it be executed by anybody

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
