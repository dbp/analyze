#!/bin/bash
ps aux | grep [a]nalyze | awk '{print $2}' | xargs kill
echo 0
