# Sway IPC Haskell binding

These are haskell bindings for the IPC protocol provided by the
Sway wayland compositor.

Please note that this is alpha-quality code without a stable API.
It was mainly written for personal use and to to play around with
fancier type-level representation of simple protocols. It has not
been tested against the i3 window manager for X11.

Consider using the mature
[i3ipc](https://github.com/leshow/i3ipc) library instead.

See `sway-ipc(7)` for details about the protocol.  
See `sway-ipc/app/Main.hs` for a usage example.
