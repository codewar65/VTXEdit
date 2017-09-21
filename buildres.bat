rem need appropriate script for linux
rem lazres is in the lazarus\tools path

@echo off

set PATH=%PATH%;C:\lazarus\tools;

echo work\c0.cur  > lrs.tmp
echo work\c1.cur >> lrs.tmp
echo work\c2.cur >> lrs.tmp
echo work\c3.cur >> lrs.tmp
echo work\c4.cur >> lrs.tmp

lazres vtxcursors.lrs @lrs.tmp

del lrs.tmp
