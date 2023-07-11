"""
#!/bin/bash
echo '' >>/tmp/crsh-log
echo 'STARTING NEW SESSION' >>/tmp/crsh-log
echo '' >>/tmp/crsh-log

( sleep 1; ./crsh-remote ) &
disown %1
echo "master disowned the remote" >>/tmp/crsh-log

sleep 2  # So that the fifos are initialized

echo "master will enter the loop" >>/tmp/crsh-log
while true; do
  echo "master will read a line" >>/tmp/crsh-log
  read -p '> ' -r line </dev/tty >/dev/tty
  echo "master did read a line $line" >>/tmp/crsh-log
  echo "$line"
  echo "master did send the line" >>/tmp/crsh-log
  IFS= read -r res
  echo "master did read the result $res" >>/tmp/crsh-log
  sleep 1  # So that the tty is fully outputted
done >/tmp/crsh-command </tmp/crsh-result 
"""

import subprocess
import time

# sub = subprocess.Popen("./crsh-server")

time.sleep(1)

with open("/tmp/crsh-command", "w") as cmd, open("/tmp/crsh-result", "r") as res:
    while True:
        com = input("> ")
        cmd.write(com + "\n")
        cmd.flush()
        print("written")
        status = res.readline()
        print("got status")
        time.sleep(1)