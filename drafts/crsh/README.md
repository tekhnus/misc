**crsh**<sup>❤️</sup> is an interactive Unix shell
with outstanding built-in remote capabilities.

if you ssh into servers or containers a lot, you
probably spent your time on:
  - configuring the shell on each new host;
  - migrating the shell history between the hosts;
  - re-establishing the remote session
    after each network disconnection;
  - setting up tmux sessions.

crsh does this all for you out of the box.

usage:
  - to log into a remote server,
    use a built-in `s [server]` command
    instead of the usual `ssh [server]`;
  - to return to the local machine,
    type `s ^`;
  - to terminate the session, type `exit`
    or press Ctrl + D;
  - to detach from the session without
    terminating it, just close the terminal window;
  - to reattach to the session,
    log into the host and use a built-in
    `r [session]` command.

installation:
  - ensure that `go` is installed;
  - clone this repo;
  - type `./build && ./install`;
  - (optionally) configure your terminal
    to run `~/.local/bin/crsh`