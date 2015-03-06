Vagrant.configure("2") do |config|

  config.vm.define "gossiper1" do |machine|
    machine.vm.network "private_network", ip: "192.168.50.100"
    machine.vm.provision :shell, :inline => "chmod +x /vagrant/bin/vagrant-init.sh"
    machine.vm.provision :shell, :inline => "/vagrant/bin/vagrant-init.sh"
    machine.vm.provision :shell, :inline => "chmod +x /vagrant/bin/vagrant-influx.sh"
    machine.vm.provision :shell, :inline => "/vagrant/bin/vagrant-influx.sh"
    machine.vm.box = "precise64"
    machine.vm.box_url = "http://files.vagrantup.com/precise64.box"
    machine.vm.provider "virtualbox" do |vbox|
      vbox.customize ["modifyvm", :id, "--memory", "512"]
    end
  end

  config.vm.define "gossiper2" do |machine|
    machine.vm.network "private_network", ip: "192.168.50.101"
    machine.vm.provision :shell, :inline => "chmod +x /vagrant/bin/vagrant-init.sh"
    machine.vm.provision :shell, :inline => "/vagrant/bin/vagrant-init.sh"
    machine.vm.provision :shell, :inline => "chmod +x /vagrant/bin/vagrant-influx.sh"
    machine.vm.provision :shell, :inline => "/vagrant/bin/vagrant-influx.sh"
    machine.vm.box = "precise64"
    machine.vm.box_url = "http://files.vagrantup.com/precise64.box"
    machine.vm.provider "virtualbox" do |vbox|
      vbox.customize ["modifyvm", :id, "--memory", "512"]
    end
  end

  config.vm.define "gossiper3" do |machine|
    machine.vm.network "private_network", ip: "192.168.50.102"
    machine.vm.provision :shell, :inline => "chmod +x /vagrant/bin/vagrant-init.sh"
    machine.vm.provision :shell, :inline => "/vagrant/bin/vagrant-init.sh"
    machine.vm.provision :shell, :inline => "chmod +x /vagrant/bin/vagrant-influx.sh"
    machine.vm.provision :shell, :inline => "/vagrant/bin/vagrant-influx.sh"
    machine.vm.box = "precise64"
    machine.vm.box_url = "http://files.vagrantup.com/precise64.box"
    machine.vm.provider "virtualbox" do |vbox|
      vbox.customize ["modifyvm", :id, "--memory", "512"]
    end
  end

end