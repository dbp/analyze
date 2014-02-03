#!/bin/bash
ps aux | grep /var/www/analyze | awk '{print $2}' | xargs kill
mv /var/www/analyze /var/www/analyze-old
mv /var/www/analyze-new /var/www/analyze
ps aux | grep /var/www/worker | awk '{print $2}' | xargs kill
mv /var/www/worker /var/www/worker-old
mv /var/www/worker-new /var/www/worker
echo 0
