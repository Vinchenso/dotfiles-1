{config, pkgs, ...}:

{
  boot.cleanTmpDir = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.enable = true;

  environment.etc.current-nixos-config.source = ./.;
  environment.systemPackages = with pkgs; [
    wget vim git (ripgrep.override {withPCRE2 = true;}) fd stow exa nix-index
    gnupg fzf atool file tmux pv binutils
  ];

  fonts.fontconfig.defaultFonts.monospace = [ "Iosevka" ];
  fonts.fonts = with pkgs; [
    fira-code-symbols noto-fonts symbola noto-fonts-cjk font-awesome_5
  ];

  hardware.pulseaudio.enable = true;

  networking.extraHosts = builtins.readFile (builtins.fetchurl { name = "blocked_hosts.txt"; url = "http://pgl.yoyo.org/adservers/serverlist.php?hostformat=hosts&showintro=0&mimetype=plaintext"; });
  networking.networkmanager.enable = true;  # Enables wireless support via wpa_supplicant.

  nix.trustedUsers = [ "root" "aria" ];

  nixpkgs.config.allowUnfree = true;

  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };
  programs.mosh.enable = true;
  programs.mtr.enable = true;

  programs.zsh.enable = true;
  programs.zsh.promptInit = "";

  services.avahi.enable = true;
  services.avahi.ipv6 = true;
  services.avahi.nssmdns = true;
  services.avahi.publish.addresses = true;
  services.avahi.publish.domain = true;
  services.avahi.publish.enable = true;

  services.compton.enable = true;

  services.locate = {
    enable = true;
    locate = pkgs.mlocate;
    interval = "hourly";
  };

  services.openssh.enable = true;
  services.openssh.startWhenNeeded = true;

  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
    user = "aria";
    dataDir = "/home/aria/.config/syncthing";
  };

  services.unbound.enable = true;

  services.xserver.autoRepeatDelay = 200;
  services.xserver.autoRepeatInterval = 25;
  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.windowManager.bspwm.enable = true;
  services.xserver.xkbOptions = "compose:ralt";

  sound.enable = true;

  time.timeZone = "Australia/Sydney";

  users.users.aria = {
    isNormalUser = true;
    uid = 1000;
    shell = pkgs.zsh;
    extraGroups = [ "wheel" "networkmanager" "video" "mlocate" ];
  };
}
