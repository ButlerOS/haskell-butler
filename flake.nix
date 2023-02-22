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
        ebml = hpPrev.callCabal2nix "ebml" (pkgs.fetchFromGitHub {
          owner = "TristanCacqueray";
          repo = "haskell-ebml";
          rev = "aff25512b52e48e92d77cd59019a0291a8b43bf4";
          sha256 = "sha256-U2Mo83gr7dLm+rRKOLzS9LZUaZ90ECO6Zjbv6maflyc=";
        }) { };
        # https://github.com/awkward-squad/ki/issues/19
        ki = let
          src = pkgs.fetchFromGitHub {
            owner = "awkward-squad";
            repo = "ki";
            rev = "f439a7dda99c2f71fb1d288cccb7a19ac436ce6d";
            sha256 = "sha256-HTS+0hAExe3wrFgqHx35mMyTZ823AhbXHLlxATx5ExM=";
          };
        in hpPrev.callCabal2nix "ki" "${src}/ki" { };
        # https://github.com/yesodweb/wai/pull/923
        warp-tls = let
          src = pkgs.fetchFromGitHub {
            owner = "TristanCacqueray";
            repo = "wai";
            rev = "4bc9365d9cbe0317459d0fd378ee667dc5ad95ee";
            sha256 = "sha256-IFt8/talF7jRwODMUpNT/N6GEf4zBxL3dOc1y2hSxLc=";
          };
        in hpPrev.callCabal2nix "warp-tls" "${src}/warp-tls" { };
        posix-pty = let
          src = pkgs.fetchFromGitHub {
            owner = "TristanCacqueray";
            repo = "posix-pty";
            rev = "a5dff4cf9ab47ebd1d7c754fc6ed4d5367653779";
            sha256 = "sha256-jbuJaeD7JpYK6nevyxGUq4FYqTmA6qgfloIvfNW3YY4=";
          };
        in pkgs.haskell.lib.dontCheck
        (hpPrev.callCabal2nix "posix-pty" src { });
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

      baseTools = with pkgs; [
        hpack
        cabal-install
        hlint
        tasty-discover
        fourmolu
        weeder
        hsPkgs.doctest
      ];

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

      devShells."x86_64-linux".ci = hsPkgs.shellFor {
        packages = p: [ p.butler ];
        buildInputs = baseTools;
      };
      devShell."x86_64-linux" = hsPkgs.shellFor {
        packages = p: [ p.butler ];
        buildInputs = with pkgs;
          [ sqlite ghcid haskell-language-server pkgs.gst_all_1.gstreamer ] ++ desktop
          ++ baseTools;
        GST_PLUGIN_PATH =
          "${pkgs.gst_all_1.gst-plugins-base}/lib/gstreamer-1.0/:${pkgs.gst_all_1.gst-plugins-good}/lib/gstreamer-1.0/";
      };
    };
}
