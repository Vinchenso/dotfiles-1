{config, pkgs, ...}:

{
    environment.systemPackages = with pkgs; [
      wget vim git (ripgrep.override {withPCRE2 = true;}) fd stow exa nix-index
      gnupg fzf atool file tmux pv binutils
    ];

  nix.trustedUsers = [ "root" "aria" ];

  fonts.fonts = with pkgs; [
    fira-code-symbols noto-fonts symbola noto-fonts-cjk font-awesome_5
  ];
  fonts.fontconfig.defaultFonts.monospace = [ "Iosevka" ];

  programs.mtr.enable = true;
  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };
  programs.zsh.enable = true;
  programs.zsh.promptInit = "";
  programs.mosh.enable = true;
  networking.networkmanager.enable = true;  # Enables wireless support via wpa_supplicant.

  services.openssh.enable = true;
  services.openssh.startWhenNeeded = true;

  services.unbound.enable = true;

  services.avahi.enable = true;
  services.avahi.nssmdns = true;
  services.avahi.ipv6 = true;
  services.avahi.publish.enable = true;
  services.avahi.publish.addresses = true;
  services.avahi.publish.domain = true;

  time.timeZone = "Australia/Sydney";

  services.xserver.autoRepeatDelay = 200;
  services.xserver.autoRepeatInterval = 25;

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.windowManager.bspwm.enable = true;
  services.xserver.xkbOptions = "compose:ralt";
  services.compton.enable = true;

  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
    user = "aria";
    dataDir = "/home/aria/.config/syncthing";
  };

  networking.extraHosts = builtins.readFile (builtins.fetchurl { name = "blocked_hosts.txt"; url = "http://pgl.yoyo.org/adservers/serverlist.php?hostformat=hosts&showintro=0&mimetype=plaintext"; });

  environment.etc.current-nixos-config.source = ./.;

  services.locate = {
    enable = true;
    locate = pkgs.mlocate;
    interval = "hourly";
  };

  users.users.aria = {
    isNormalUser = true;
    uid = 1000;
    shell = pkgs.zsh;
    extraGroups = [ "wheel" "networkmanager" "video" "mlocate" ];
  };

  nixpkgs.config.allowUnfree = true;


  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.cleanTmpDir = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;

}
