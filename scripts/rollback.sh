#!/bin/bash
ps aux | grep [a]nalyze | awk '{print $2}' | xargs kill
cp /var/www/analyze-old /var/www/analyze
ps aux | grep [w]orker | awk '{print $2}' | xargs kill
cp /var/www/worker-old /var/www/worker
