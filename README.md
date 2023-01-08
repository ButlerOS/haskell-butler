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
- [ ] Display for html5 gui
- [ ] Audio channel

To validate the abstractions, the project also feature some proof of concept demos:


## Multiplayer Desktop

A window-xp clone with support for multiple "seats":

- [x] Simple invitation system to share the session with other users.
- [x] Dekstop with a menu and tray bar
- [x] Window manager
- [ ] Per user cursor and audio channel
- [ ] Xterm and Vnc gateway

Usage:

```ShellSession
./bin/run-vnc
./bin/run-ghcid "Butler.demoDesktop"
firefox https://localhost:8080
```

Start electron client (Alt-F4 to quit):

```ShellSession
nix run .#electron https://localhost:8080
```

Container usage:

```ShellSession
podman run -p 8080:8080 -v butler-data:/var/lib/butler ghcr.io/tristancacqueray/haskell-butler:term-latest
```
