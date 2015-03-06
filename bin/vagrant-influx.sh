#!/bin/bash

if [ ! -f /etc/init.d/influxdb ]; then

  echo "Installing InfluxDB"
  sudo apt-get update -y
  sudo apt-get install -y wget
  sudo wget http://s3.amazonaws.com/influxdb/influxdb_latest_amd64.deb
  sudo dpkg -i influxdb_latest_amd64.deb
  sudo /etc/init.d/influxdb start
  
fi