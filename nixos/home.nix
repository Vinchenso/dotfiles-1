{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    htop qutebrowser emacs termite networkmanagerapplet dunst libnotify rofi mpv
    pass rofi-pass discord gimp (polybar.override {i3GapsSupport = true;})

    #betterlockscreen stuff
    i3lock-color imagemagick xorg.xdpyinfo xorg.xrandr bc feh
  ];

  programs = {
    firefox = {
      enable = true;
    };

    git = {
      enable = true;
      userName = "Aria Edmonds";
      userEmail = "aria@ar1as.space";
    };


    # Let Home Manager install and manage itself.
    home-manager.enable = true;
  };
}
