# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
    ];

  environment.systemPackages = with pkgs; [
    wget vim git (ripgrep.override {withPCRE2 = true;}) fd stow exa nix-index
    gnupg fzf atool file tmux pv binutils btrfs-progs pciutils
    gnome3.dconf # for virt-manager
  ];

  nix.trustedUsers = [ "root" "aria" ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.cleanTmpDir = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;

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
  networking.networkmanager.enable = true;
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # Set your time zone.
  time.timeZone = "Australia/Sydney";

  # List packages installed in system profile. To search, run:
  # $ nix search wget

  fonts.fonts = with pkgs; [
    fira-code-symbols noto-fonts symbola noto-fonts-cjk font-awesome_5 
  ];

  fonts.fontconfig.defaultFonts.monospace = [ "Iosevka" ]; # font installed in
  # ~/.local/share/fonts

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.mtr.enable = true;
  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };
  programs.zsh.enable = true;
  programs.zsh.promptInit = "";
  programs.mosh.enable = true;


  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.startWhenNeeded = true;

  services.unbound.enable = true;

  services.avahi.enable = true;
  services.avahi.nssmdns = true;
  services.avahi.ipv6 = true;
  services.avahi.publish.enable = true;
  services.avahi.publish.addresses = true;
  services.avahi.publish.domain = true;

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 24800 ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;
  #
  #
  # networking.extraHosts = builtins.readFile (builtins.fetchurl { name = "blocked_hosts.txt"; url = "http://pgl.yoyo.org/adservers/serverlist.php?hostformat=hosts&showintro=0&mimetype=plaintext"; });

  environment.etc.current-nixos-config.source = ./.;

  services.locate = {
    enable = true;
    locate = pkgs.mlocate;
    interval = "hourly";
  };

   services.syncthing = {
     enable = true;
     openDefaultPorts = true;
     user = "aria";
     dataDir = "/home/aria/.config/syncthing";
   };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.windowManager.bspwm.enable = true;
  services.xserver.videoDrivers = [ "nvidia" ];
  services.compton.enable = true;
  services.compton.backend = "glx";
  services.compton.vSync = "opengl-swc";
  services.xserver.xkbOptions = "compose:ralt";

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

  # Enable touchpad support.
  # services.xserver.libinput.enable = true;

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.aria = {
    extraGroups = ["wheel" "networkmanager" "video" "libvirtd"];
    isNormalUser = true;
    shell = pkgs.zsh;
    uid = 1000;
  };

  nixpkgs.config.allowUnfree = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?

}
