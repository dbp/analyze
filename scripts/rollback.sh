#!/bin/bash
ps aux | grep [a]nalyze | awk '{print $2}' | xargs kill
cp /var/www/analyze-old /var/www/analyze
