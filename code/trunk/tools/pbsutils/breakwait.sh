#!/bin/bash

ps
echo ---
pkill -9 -U $USER  waitall.sh
pkill -9 -U $USER  sleep
ps

