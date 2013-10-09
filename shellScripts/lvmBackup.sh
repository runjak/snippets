#!/bin/sh
# This script built by @sicarius on 2013-10-09
# The script is intended to help with backups for lvm stuff.
# To achive this, it implements some kind of rotation that is done via $step.
# $step is calculated by $seed depending on the backup time
# and $divisor setting the size of the rotation.
# At the moment the script is set to be run weekly,
# therefore seed is the ISO weeknumber.
# If dryRun=true, the script lists all expected source/target combinations.
dryRun=true
seed=`date +%V`
divisor=2
step=`echo "$seed % $divisor" | bc`
# VolumeGroup to backup from
sourceVG="/dev/vg0"
# VolumeGroup to backup to
targetVG="/dev/vgExtern"
# Logical Volumes to backup
lvs="some space seperated logical volume groups to backup"
# Iterating the volumes:
if $dryRun; then
  echo "DryRun!"
  for lv in $lvs; do
    for i in `seq 1 $divisor`; do
      step=`echo "$i - 1" | bc`
      source="$sourceVG/$lv"
      target="$targetVG/$lv.$step"
      echo "Backup: $source -> $target"
    done
  done
else
  for lv in $lvs; do
    # Setting source and target for the backup step:
    source="$sourceVG/$lv"
    target="$targetVG/$lv.$step"
    if [ -b $target -o -L $target ]; then
      stamp=`date`
      echo "Backup of $sourceVG/$lv to $targetVG/$lv.$step started on $stamp"
      # Building a snapshot:
      lvcreate -L1G -s -nbackup-snapshot $source
      snapshot="$sourceVG/backup-snapshot"
      # Using dd for the backup until something better is found:
      dd if=$snapshot of=$target
      # Removing the snapshot:
      lvremove -f $snapshot
    else
      echo "Backup target $target is missing!"
    fi
  done
fi
