{
  nixConfig.bash-prompt = "[nix(butler)] ";
  inputs = {
    jira.url =
      "github:ButlerOS/haskell-jira-client/a8d6a1f8bdfbfccbab58827316dc430f10416fcf";
    jira.flake = false;
    hspkgs.url =
      "github:podenv/hspkgs/7a46854f28ab9b99c51353c81d5967f1f6fd9a9b";
    #  "path:///srv/github.com/podenv/hspkgs";
  };
  outputs = { self, jira, hspkgs }:
    let
      pkgs = hspkgs.pkgs;

      haskellExtend = hpFinal: hpPrev: {
        butler = hpPrev.callCabal2nix "butler" self { };
        butler-desktop = hpPrev.callCabal2nix "butler-desktop" self { };
        xstatic-codemirror = let
          src = pkgs.fetchFromGitHub {
            owner = "TristanCacqueray";
            repo = "haskell-xstatic";
            rev = "d66976c03ace001952a17d67846e8b1bc22b84ab";
            sha256 = "sha256-9PBFr2sIcFg/VG93IfnfIak3fl0af0xDhtZvneCEqsk=";
          };
        in hpPrev.callCabal2nix "xstatic-codemirror" "${src}/xstatic-codemirror"
        { };
        ebml = hpPrev.callCabal2nix "ebml" (pkgs.fetchFromGitHub {
          owner = "TristanCacqueray";
          repo = "haskell-ebml";
          rev = "4196b6be0d469bc8b7252dbf58c6e15b8146aa43";
          sha256 = "sha256-OnOfzsAZvp26nah+KkhbBUAz6BwQaOuM1dc+f6/65ak=";
        }) { };
        jira-client = hpPrev.callCabal2nix "jira-client" jira { };
        md2jira = hpPrev.callCabal2nix "jira-client" "${jira}/md2jira" { };
        posix-pty = let
          src = pkgs.fetchFromGitHub {
            owner = "TristanCacqueray";
            repo = "posix-pty";
            rev = "a5dff4cf9ab47ebd1d7c754fc6ed4d5367653779";
            sha256 = "sha256-jbuJaeD7JpYK6nevyxGUq4FYqTmA6qgfloIvfNW3YY4=";
          };
        in pkgs.haskell.lib.dontCheck
        (hpPrev.callCabal2nix "posix-pty" src { });
        ot = hpPrev.callCabal2nix "ot" (pkgs.fetchFromGitHub {
          owner = "TristanCacqueray";
          repo = "ot.hs";
          rev = "daa7dc081389dab4f567b5b75121743c0a67c93a";
          sha256 = "sha256-wBsMnXPjKAiKBgXI84NEb6fgvTBGey7+Wp+C3IpvY/c=";
        }) { };
        lucid-svg = pkgs.haskell.lib.dontCheck
          (pkgs.haskell.lib.overrideCabal hpPrev.lucid-svg { broken = false; });
        one-line-aeson-text = pkgs.haskell.lib.dontCheck
          (pkgs.haskell.lib.overrideCabal hpPrev.one-line-aeson-text {
            broken = false;
          });
        servant-auth-server = pkgs.haskell.lib.dontCheck
          (pkgs.haskell.lib.overrideCabal hpPrev.servant-auth-server {
            broken = false;
          });
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

      butlerTools = pkgs.symlinkJoin {
        name = "butler-tools";
        paths = [ pkgs.tmux pkgs.nix pkgs.bubblewrap pkgs.tini pkgs.nodejs ];
      };

      container-name = "ghcr.io/butleros/haskell-butler";
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

          linuxFHS = "mkdir usr lib64 sbin";

          extra-config = if extra-pkgs == [ ] then {
            extraCommands = rwHome;
          } else {
            config.Env = [
              "SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt"
              "HOME=/${home}"
              # Use fakeroot to avoid `No user exists for uid` error
              "LD_PRELOAD=${pkgs.fakeroot}/lib/libfakeroot.so"
              "NIX_SSL_CERT_FILE=/etc/pki/tls/certs/ca-bundle.crt"
              "BUTLER_ISOLATION=none"
              "BUTLER_TOOLS=${butlerTools}"
              "TERM=xterm"
            ];
            extraCommands =
              "${createPasswd} && ${fixCABundle} && ${rwHome} && ${linuxFHS}";
          };
        in pkgs.dockerTools.streamLayeredImage (pkgs.lib.recursiveUpdate {
          name = container-name;
          contents = [ pkg-exe pkgs.openssl butlerTools ] ++ extra-pkgs;
          tag = "${name}latest";
          created = "now";
          config.Entrypoint = [ "butler" ];
          config.WorkingDir = "/${home}";
          config.Labels = {
            "org.opencontainers.image.source" =
              "https://github.com/ButlerOS/haskell-butler";
          };
        } extra-config);

      container = mkContainer "" [
        pkgs.coreutils
        pkgs.cacert
        pkgs.bashInteractive
        pkgs.tmux
        pkgs.curl
      ];

      publish-container-release = pkgs.writeShellScriptBin "butler-release" ''
        set -e
        export PATH=$PATH:${pkgs.gzip}/bin:${pkgs.skopeo}/bin
        IMAGE="docker://${container-name}"

        echo "Logging to registry..."
        echo $GH_TOKEN | skopeo login --username $GH_USERNAME --password-stdin ghcr.io

        echo "Building and publishing the image..."
        ${container} | gzip --fast | skopeo copy docker-archive:/dev/stdin $IMAGE:${pkg-exe.version}

        echo "Tagging latest"
        skopeo copy $IMAGE:${pkg-exe.version} $IMAGE:latest
      '';

      # A container with nix that is suitable to use for butler terminal
      nix-conf = pkgs.writeTextFile {
        name = "nix.conf";
        text = ''
          experimental-features = nix-command flakes
          sandbox = false
          build-users-group =
        '';
      };
      toolbox = pkgs.dockerTools.streamLayeredImage {
        name = "ghcr.io/butleros/toolbox";
        tag = "latest";
        created = "now";
        contents = [ pkgs.nix ];
        extraCommands =
          "mkdir -p etc/nix && ln -s ${nix-conf} etc/nix/nix.conf";
        # To update, run: nix run nixpkgs#nix-prefetch-docker -- -c nix-prefetch-docker --image-name registry.fedoraproject.org/fedora --image-tag 38
        fromImage = pkgs.dockerTools.pullImage {
          imageName = "registry.fedoraproject.org/fedora";
          imageDigest =
            "sha256:b14af4b4e7abb04e3dd4d7194d9415cedc6f587b6e446581d4ec110f94f9a75f";
          sha256 = "03ly24zs05kikvf12g9rmw5nsamypi0cvj3aqqs2ygghc5hs7hzy";
          finalImageName = "registry.fedoraproject.org/fedora";
          finalImageTag = "38";
        };
      };

      baseTools = with pkgs; [
        cabal-gild
        nixfmt
        cabal-install
        hlint
        tasty-discover
        fourmolu
        weeder
        doctest
      ];

    in {
      pkgs = pkgs;
      haskellExtend = haskellExtend;
      packages."x86_64-linux".default = pkg-exe;
      packages."x86_64-linux".container = container;
      apps."x86_64-linux".publish-container-release = {
        type = "app";
        program = "${publish-container-release}/bin/butler-release";
      };

      packages."x86_64-linux".toolbox = toolbox;
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
        script = let
          nixGLSrc = pkgs.fetchFromGitHub {
            owner = "guibou";
            repo = "nixGL";
            rev = "c917918ab9ebeee27b0dd657263d3f57ba6bb8ad";
            sha256 = "sha256-KCkWZXCjH+C4Kn7fUGSrEl5btk+sERHhZueSsvVbPWc=";
          };
          nixGL = import nixGLSrc { pkgs = pkgs; };
        in pkgs.writers.writeBash "butler-electron.sh" ''
          set -xe
          exec ${nixGL.auto.nixGLDefault}/bin/nixGL ${pkgs.electron}/bin/electron ${self}/bin/electron.js $*
        '';
        program = builtins.toString script;
      };

      devShells."x86_64-linux".ci = hsPkgs.shellFor {
        packages = p: [ p.butler ];
        buildInputs = baseTools;
      };
      devShell."x86_64-linux" = hsPkgs.shellFor {
        packages = p: [ p.butler p.butler-desktop ];
        buildInputs = with pkgs;
          [
            sqlite
            ghcid
            haskell-language-server
            pkgs.gst_all_1.gstreamer
            butlerTools
          ] ++ desktop ++ baseTools;
        BUTLER_TOOLS = toString butlerTools;
        GST_PLUGIN_PATH =
          "${pkgs.gst_all_1.gst-plugins-base}/lib/gstreamer-1.0/:${pkgs.gst_all_1.gst-plugins-good}/lib/gstreamer-1.0/";
      };
      devShells."x86_64-linux".demo = pkgs.mkShell {
        buildInputs = [
          (hsPkgs.ghcWithPackages (p: [
            (pkgs.haskell.lib.dontHaddock p.butler)
            p.markdown-unlit
            p.rio
            p.string-qq
          ]))
          pkgs.ghcid
          pkgs.haskell-language-server
        ];
      };
    };
}
