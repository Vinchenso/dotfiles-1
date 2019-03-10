{ config, pkgs, ... }:

{
  networking.firewall.allowedTCPPorts = [ 655 ];
  networking.firewall.allowedUDPPorts = [ 655 ];

  environment.etc."tinc/regnet/tinc-up".source = pkgs.writeScript "tinc-up-regnet" ''
    #!${pkgs.stdenv.shell}
    ${pkgs.iproute}/bin/ip link set $INTERFACE up
    ${pkgs.iproute}/bin/ip addr add 10.13.37.1/24 dev $INTERFACE
    # stevens house
    ${pkgs.iproute}/bin/ip route add 172.16.17.0/24 via 10.13.37.2
  '';

  environment.etc."tinc/regnet/tinc-down".source = pkgs.writeScript "tinc-down-regnet" ''
    #!${pkgs.stdenv.shell}
    /run/wrappers/bin/sudo ${pkgs.iproute}/bin/ip addr del 10.13.37.1/24 dev $INTERFACE
    /run/wrappers/bin/sudo ${pkgs.iproute}/bin/ip route del 172.16.17.0/24
    /run/wrappers/bin/sudo ${pkgs.iproute}/bin/ip link set $INTERFACE down
  '';

  security.sudo.extraRules = [
    {
      users = ["tinc.regnet"];
      commands = [
        {
          command = "${pkgs.iproute}/bin/ip";
          options = ["NOPASSWD"];
        }
      ];
    }
  ];

  services.tinc.networks.regnet = {
    name = "uranium";
    interfaceType = "tap";
    ed25519PrivateKeyFile = "/etc/tinc/regnet/ed25519_key.priv";
    extraConfig = ''
      Mode = Switch
      LocalDiscovery = yes
      PrivateKeyFile = /etc/tinc/regnet/rsa_key.priv
    '';
  };

  # DNSSEC has to be disabled for the zones to work
  services.unbound.enableRootTrustAnchor = false;
  services.unbound.extraConfig = ''
    stub-zone:
        name: "home.b303.me"
        stub-addr: 10.13.37.2
    forward-zone:
        name: "regnet.lan"
        forward-addr: 10.13.37.4
  '';
}
