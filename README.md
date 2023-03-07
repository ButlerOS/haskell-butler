# haskell-butler

> Note that this is a work in progress, please get in touch if you are interested.

This library provides a virtual operating system to run your Haskell program.
Checkout the git history to see the available modules.

## Project Status

The goal of butler is to implement modular abstractions
by simulating a traditional computer.

- [x] Logger and Clock
- [x] Process for supervision tree
- [x] Storage for persistance
- [x] Network for web application
- [x] Websocket session for authentication
- [x] Display for html5 gui
- [x] Audio channel

To validate the abstractions, the project also feature some proof of concept demos:

## HTML5 GUI

Serve gui app behind nginx with:

```
upstream butler {
     server 127.0.0.1:8042;
}

server {
    location / {
      proxy_pass http://butler/;
      proxy_http_version 1.1;
      proxy_set_header Upgrade $http_upgrade;
      proxy_set_header Connection "Upgrade";
      proxy_set_header Host $host;
    }
}
```

Start service with:

```
BUTLER_ADDR=http:8042 cabal run
```

## Multiplayer Desktop

A window-xp clone with support for multiple "seats":

- [x] Simple invitation system to share the session with other users.
- [x] Dekstop with a menu and tray bar
- [x] Window manager
- [x] Per user cursor and audio channel
- [x] Xterm and Vnc gateway

Usage:

```ShellSession
./bin/run-vnc
./bin/run-ghcid "ButlerDemos.multiDesktop"
firefox https://localhost:8080
```

Use the electron client to forward your ssh-agent and enables Ctrl-T and Ctrl-N key code:

```ShellSession
nix run .#electron https://localhost:8080
```

Container usage:

```ShellSession
podman run -p 8080:8080 -v butler-data:/var/lib/butler ghcr.io/tristancacqueray/haskell-butler:term-latest
```

## Contribute

Contributions and bug reports are welcome!
To work on this project you need a Haskell toolchain: [get-started](https://www.haskell.org/get-started/).

Run the `./bin/run-tests` script to validate a commit.

Setup binary cache with: `nix run nixpkgs/release-22.11#cachix use haskell-platform`.
