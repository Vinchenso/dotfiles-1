{ config, pkgs, ... }:

{
  home.packages = [
    pkgs.htop
  ];

  programs.firefox = {
    enable = true;
  };

  programs.git = {
    enable = true;
    userName = "Aria Edmonds";
    userEmail = "aria@ar1as.space";
  };
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
