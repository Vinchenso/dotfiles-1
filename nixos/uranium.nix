# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
      ./tinc.nix
      ./system.nix
    ];

  environment.systemPackages = with pkgs; [
    btrfs-progs pciutils gnome3.dconf # for virt-manager
  ];


  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/disk/by-uuid/86407c78-7a08-40c5-9f8f-918616ae43d9";
      preLVM = true;
      allowDiscards = true;
    }
  ];

  boot.initrd.kernelModules = [ "vfio" "vfio_pci" "vfio_virqfd" "vfio_iommu_type1" ];
  boot.kernelParams = [
    "amd_iommu=on" "iommu=pt" "vfio_pci.ids=10de:1f02,10de:10f9,10de:1ada,10de:1adb"
    "default_hugepagesz=1G" "hugepagesz=1G" "hugepages=8"
    "kvm.ignore_msrs=1"
  ];

  virtualisation.libvirtd.enable = true;
  # virtualisation.libvirtd.qemuRunAsRoot = false;
  virtualisation.libvirtd.onShutdown = "shutdown";
  virtualisation.libvirtd.qemuVerbatimConfig = ''
    user = "aria"
  '';

  networking.hostName = "uranium"; # Define your hostname.

  networking.firewall.allowedTCPPorts =
    [
    24800 # barrier
    ];

  services.xserver.videoDrivers = [ "nvidia" ];

  # Disable mouse acceleration
  services.xserver.libinput.enable = true;
  # services.xserver.libinput.accelProfile = "flat"; # Only affects touchpads
  services.xserver.config = ''
    Section "InputClass"
      Identifier "mouse accel"
      Driver "libinput"
      MatchIsPointer "on"
      Option "AccelProfile" "flat"
      Option "AccelSpeed" "0"
    EndSection
  '';


  services.compton.backend = "glx";
  services.compton.vSync = "opengl-swc";

  services.collectd = {
    enable = true;
    autoLoadPlugin = true;

    extraConfig = ''
      # FQDNLookup and NixOS/nixpkgs#47241 don’t seem to be easily compatible
      # https://github.com/NixOS/nixpkgs/issues/1248#issuecomment-303552124
      <Plugin network>
      Server "172.19.128.121" "25826"
      </Plugin>

      ReadThreads 5
      Interval 10
      Timeout 2

      LoadPlugin syslog
      LoadPlugin cpu
      <Plugin cpu>
        ValuesPercentage true
      </Plugin>
      LoadPlugin df
      LoadPlugin disk
      LoadPlugin interface
      LoadPlugin load
      LoadPlugin memory
      LoadPlugin processes
      LoadPlugin uptime
      LoadPlugin users
    '';
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?

}
