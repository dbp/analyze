#!/bin/bash
ps aux | grep /var/www/analyze | awk '{print $2}' | xargs kill
mv /var/www/analyze /var/www/analyze-old
mv /var/www/analyze-new /var/www/analyze
echo 0
