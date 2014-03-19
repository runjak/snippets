#!/bin/sh
# This script built by @sicarius on 2014-03-19
# This script is intendet to aid some backups, and does the following things:
# 1.: Create snapshots of given logical volumes
# 2.: dd some stuff that is not lvm somewhere else
# 3.: Maybe remove old snapshots, but that's not done right now
date=`date -I`
volumes="space separated lvs on vgGunter"
snapshotSize="5G"
# Getting ready:
echo "Running backups on $date:"
# Creating snapshots:
for lv in $volumes; do
  command="lvcreate -L$snapshotSize -s -n$lv-$date /dev/vgGunter/$lv"
  echo "Creating snapshot: $command"
  $command
done
#Doing dd to lvs that are on a raid:
# Boot:
btarget="boot-$date"
bcreate="lvcreate -L1G -n $btarget vgGunter"
bdd="dd if=/dev/sda1 of=/dev/vgGunter/$btarget"
echo "Creating boot backup location: $bcreate"
$bcreate
echo "Copying boot to backup: $bdd"
$bdd
# Root:
rsource="marceline-root"
rtarget="$rsource-$date"
rsnap="lvcreate -L5G -s -n $rtarget /dev/vgRoot/$rsource"
rback="lvcreate -L20G -n $rtarget /dev/vgGunter"
rdd="dd if=/dev/vgRoot/$rtarget of=/dev/vgGunter/$rtarget"
rfree="lvremove -f /dev/vgRoot/$rtarget"
echo "Performing backup operations for root:"
echo $rsnap
$rsnap
echo $rback
$rback
echo $rdd
$rdd
echo $rfree
$rfree
