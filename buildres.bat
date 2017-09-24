rem need appropriate script for linux
rem lazres is in the lazarus\tools path

@echo off

set PATH=%PATH%;C:\lazarus\tools;

echo work\c0.cur  > lrs.tmp
echo work\c1.cur >> lrs.tmp
echo work\c2.cur >> lrs.tmp
echo work\c3.cur >> lrs.tmp
echo work\c4.cur >> lrs.tmp
echo work\c5.cur >> lrs.tmp
echo work\c6.cur >> lrs.tmp
echo work\c7.cur >> lrs.tmp
echo work\c8.cur >> lrs.tmp
echo work\c9.cur >> lrs.tmp
rem echo work\c4.cur >> lrs.tmp

lazres vtxcursors.lrs @lrs.tmp

del lrs.tmp
