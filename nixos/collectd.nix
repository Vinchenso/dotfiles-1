{config, pkgs, ...}:

{
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
}
