#!/bin/sh

set -x

export PATH=/usr/sbin:/sbin:/usr/bin:/bin

cd /root/autobuilder-repo-for-cron-job &&
darcs pull -a &&
mkdir -p /storage/root/autobuilder-logs &&
set -x &&
for dist in hardy sid lenny intrepid; do
  autobuilder --use-repo-cache ${dist}-build --use all-targets --do-upload --do-newdist \
	> "/storage/root/autobuilder-logs/`date +%F-%T-%Z`-${dist}" 2>&1
done
