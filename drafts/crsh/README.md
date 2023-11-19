**crsh**<sup>❤️</sup> is an interactive Unix shell
with outstanding built-in remote capabilities.

if you ssh into servers or containers a lot, you
probably spent your time on:
  - configuring the shell on each new host;
  - migrating the shell history between the hosts;
  - re-establishing the remote session
    after each network disconnection.

crsh does this all for you out of the box.
besides that, it manages persistent sessions,
sparing you from interacting with tmux.

usage:
  - to log into a remote server,
    use a built-in `s [server]` command
    instead of the usual `ssh [server]`;
  - to terminate the session, type `exit`
    or press Ctrl + D.
  - to detach from the session without
    terminating it, just close the terminal window;
  - to reattach to the session,
    log into the host and use a built-in
    `r [session]` command.