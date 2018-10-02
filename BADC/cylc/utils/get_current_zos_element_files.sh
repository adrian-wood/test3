#!/bin/bash
rm -fr /tmp/zosele > /dev/null 2>&1
mkdir -p /tmp/zosele
ftp ukmet << EOF
prompt
lcd /tmp/zosele
mget 'MDB.BADC.DATA(*)'
EOF

for i in /tmp/zosele/*; do mv "$i" "$(echo "$i" | tr -d "'")"; done
