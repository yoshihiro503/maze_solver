#!/bin/sh
wget --user="yoshihiro503" --password="bunbunbanban28" 'http://twitter.com/account/rate_limit_status.json' -O - | json
