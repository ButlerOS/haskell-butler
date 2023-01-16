{
  nixConfig.bash-prompt = "[nix(butler)] ";
  inputs = {
    hspkgs.url =
      "github:podenv/hspkgs/ba5d181089900f376f765e4a6889bd30c4f96993";
    # "path:///srv/github.com/podenv/hspkgs";
  };
  outputs = { self, hspkgs }:
    let
      pkgs = hspkgs.pkgs;
      haskellExtend = hpFinal: hpPrev: {
        butler = hpPrev.callCabal2nix "butler" self { };
      };
      hsPkgs = pkgs.hspkgs.extend haskellExtend;

      pkg-exe = pkgs.haskell.lib.justStaticExecutables hsPkgs.butler;

      desktop = with pkgs; [
        nixGLIntel
        # xvfb-run
        xdummy
        # tigervnc
        x11vnc
        xorg.xrandr
        glxinfo
        openbox
        xterm
        autocutsel
      ];

      mkContainer = name: extra-pkgs:
        let
          # Container user info
          user = "butler";
          home = "var/lib/${user}";

          # Create a passwd entry so that openssh can find the .ssh config
          createPasswd =
            "echo ${user}:x:0:0:${user}:/${home}:/bin/bash > etc/passwd";

          # Make ca-bundles.crt available to HSOpenSSL as plain file
          # https://hackage.haskell.org/package/HsOpenSSL-x509-system-0.1.0.4/docs/src/OpenSSL.X509.SystemStore.Unix.html#contextLoadSystemCerts
          fixCABundle =
            "mkdir -p etc/pki/tls/certs/ && cp etc/ssl/certs/ca-bundle.crt etc/pki/tls/certs/ca-bundle.crt";

          # Ensure the home directory is r/w for any uid
          rwHome = "mkdir -p -m 1777 ${home} tmp";

          extra-config = if extra-pkgs == [ ] then {
            extraCommands = rwHome;
          } else {
            config.Env = [
              "SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt"
              "HOME=/${home}"
              # Use fakeroot to avoid `No user exists for uid` error
              "LD_PRELOAD=${pkgs.fakeroot}/lib/libfakeroot.so"
              "TERM=xterm"
            ];
            extraCommands = "${createPasswd} && ${fixCABundle} && ${rwHome}";
          };
        in pkgs.dockerTools.buildLayeredImage (pkgs.lib.recursiveUpdate {
          name = "ghcr.io/TristanCacqueray/haskell-butler";
          contents = [ pkg-exe pkgs.openssl ] ++ extra-pkgs;
          tag = "${name}-latest";
          created = "now";
          config.Entrypoint = [ "butler" ];
          config.WorkingDir = "/${home}";
          config.Labels = {
            "org.opencontainers.image.source" =
              "https://github.com/TristanCacqueray/haskell-butler";
          };
        } extra-config);

    in {
      packages."x86_64-linux".default = pkg-exe;

      packages."x86_64-linux".containerBase = mkContainer "base" [ ];
      packages."x86_64-linux".containerTerm = mkContainer "term" [
        pkgs.coreutils
        pkgs.cacert
        pkgs.bashInteractive
        pkgs.tmux
        pkgs.curl
      ];

      apps."x86_64-linux".electron = rec {
        type = "app";
        script = pkgs.writers.writeBash "butler-electron.sh" ''
          set -xe
          exec ${pkgs.nixGLIntel}/bin/nixGLIntel ${pkgs.electron}/bin/electron ${self}/bin/electron.js $*
        '';
        program = builtins.toString script;
      };

      devShell."x86_64-linux" = hsPkgs.shellFor {
        packages = p: [ p.butler ];
        buildInputs = with pkgs;
          [
            hpack
            cabal-install
            ghcid
            haskell-language-server
            fourmolu
            hsPkgs.doctest
          ] ++ desktop;
      };
    };
}
