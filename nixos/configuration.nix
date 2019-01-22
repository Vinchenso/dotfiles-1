# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ];


    # List packages installed in system profile. To search, run:
    # $ nix search wget
    environment.systemPackages = with pkgs; [
      wget vim git ripgrep i3 fd stow exa nix-index gnupg skim atool file tmux pv
    ];



    fonts.fonts = with pkgs; [
      fira-code-symbols noto-fonts symbola noto-fonts-cjk font-awesome_5
    ];
    fonts.fontconfig.defaultFonts.monospace = [ "Iosevka" ]; # font installed in
    # ~/.local/share/fonts

    # Use the systemd-boot EFI boot loader.
    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;
    boot.kernelPackages = pkgs.linuxPackages_latest;

    fileSystems."/".options = [ "noatime" "nodiratime" "discard" ];

    boot.initrd.luks.devices = [
      {
        name = "root";
        device = "/dev/disk/by-uuid/29f7dde5-7594-4783-938b-8a01294c687c";
        preLVM = true;
        allowDiscards = true;
      }
    ];

    boot.initrd.availableKernelModules = [
      "aes_x86_64"
      "aesni_intel"
      "cryptd"
    ];

    programs.zsh.enable = true;
    programs.zsh.promptInit = "";
    programs.light.enable = true;
    programs.mosh.enable = true;

    networking.hostName = "Rhenium"; # Define your hostname.
    networking.networkmanager.enable = true;  # Enables wireless support via wpa_supplicant.

    # Configure network proxy if necessary
    # networking.proxy.default = "http://user:password@proxy:port/";
    # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

    # Select internationalisation properties.
    # i18n = {
      #   consoleFont = "Lat2-Terminus16";
      #   consoleKeyMap = "us";
      #   defaultLocale = "en_US.UTF-8";
      # };

      # Set your time zone
      time.timeZone = "Australia/Sydney";



      # Some programs need SUID wrappers, can be configured further or are
      # started in user sessions.
      programs.mtr.enable = true;
      programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

      # List services that you want to enable:

      # Enable the OpenSSH daemon.
      services.openssh.enable = true;
      services.openssh.passwordAuthentication = false;
      services.openssh.challengeResponseAuthentication = false;

      services.unbound.enable = true;

      services.avahi.enable = true;
      services.avahi.nssmdns = true;
      services.avahi.ipv6 = true;
      services.avahi.publish.enable = true;
      services.avahi.publish.addresses = true;
      services.avahi.publish.domain = true;

      # Open ports in the firewall.
      # networking.firewall.allowedTCPPorts = [ 8080 ];
      # networking.firewall.allowedUDPPorts = [ ... ];
      # Or disable the firewall altogether.
      # networking.firewall.enable = false;

      # Enable CUPS to print documents.
      # services.printing.enable = true;

      # Enable sound.
      sound.enable = true;
      hardware.pulseaudio.enable = true;
      hardware.pulseaudio.package = pkgs.pulseaudioFull;

      hardware.bluetooth.enable = true;
      hardware.bluetooth.powerOnBoot = false;

      # Enable the X11 windowing system.
      services.xserver.enable = true;
      services.xserver.layout = "us";
      services.xserver.windowManager.i3.enable = true;
      services.xserver.windowManager.i3.package = pkgs.i3-gaps;
      # services.xserver.xkbOptions = "eurosign:e";
      services.compton.enable = true;

      # Enable touchpad support.
      services.xserver.libinput.enable = true;
      services.xserver.libinput.disableWhileTyping = true;
      services.xserver.libinput.tapping = false;

      # Idealy this would be in home.nix now but that means I have to set up syncing
      # *again* and fuck that
      services.syncthing = {
        enable = true;
        openDefaultPorts = true;
        user = "aria";
        dataDir = "/home/aria/.config/syncthing";
      };

      # Enable the KDE Desktop Environment.
      # services.xserver.displayManager.sddm.enable = true;
      # services.xserver.desktopManager.plasma5.enable = true;

      # Define a user account. Don't forget to set a password with ‘passwd’.
      users.users.aria = {
        isNormalUser = true;
        uid = 1000;
        shell = pkgs.zsh;
        extraGroups = [ "wheel" "networkmanager" "video" ];
      };

      nixpkgs.config.allowUnfree = true;

      # This value determines the NixOS release with which your system is to be
      # compatible, in order to avoid breaking some software such as database
      # servers. You should change this only after NixOS release notes say you
      # should.
      system.stateVersion = "18.09"; # Did you read the comment?
}
