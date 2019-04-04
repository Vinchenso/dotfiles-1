# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [
    /etc/nixos/hardware-configuration.nix
    ./system.nix
    ./collectd.nix
    ];

  fileSystems."/".options = [ "noatime" "nodiratime" "discard" ];

  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/disk/by-uuid/29f7dde5-7594-4783-938b-8a01294c687c";
      preLVM = true;
      allowDiscards = true;
    }
  ];

  programs.light.enable = true;

  networking.hostName = "rhenium"; # Define your hostname.
  hardware.pulseaudio.package = pkgs.pulseaudioFull;

  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = false;

  services.xserver.libinput.enable = true;
  services.xserver.libinput.disableWhileTyping = true;
  services.xserver.libinput.tapping = false;

  services.tlp.enable = true;

  services.redis.enable = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?
}
