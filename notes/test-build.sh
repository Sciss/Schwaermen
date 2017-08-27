#!/bin/sh
sbt schwaermen-sound/debian:packageBin schwaermen-video/debian:packageBin
sudo dpkg -i sound/target/Schwaermen-Sound_0.2.2_all.deb
sudo dpkg -i video/target/Schwaermen-Video_0.2.2_all.deb
