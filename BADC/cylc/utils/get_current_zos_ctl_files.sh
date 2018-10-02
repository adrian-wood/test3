#!/bin/bash
rm -fr /tmp/zosctl > /dev/null 2>&1
mkdir -p /tmp/zosctl
ftp ukmet << EOF
prompt
lcd /tmp/zosctl
mget 'MDB.BADC.*.CONTROL'
EOF

for i in /tmp/zosctl/*; do mv "$i" "$(echo "$i" | tr -d "'")"; done
