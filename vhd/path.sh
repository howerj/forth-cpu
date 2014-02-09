#!/bin/bash
echo "This script just makes sure that my installation of Xilinx";
echo "synthesis tools are on my path, it should be of no help to";
echo "you. TODO, fix the problem and not use this work around...";
if [ ! -d /mnt/usb/opt ]; then
  sudo mount /dev/sda2 /mnt/usb/
fi
PATH=$PATH:/mnt/usb/opt/Xilinx/14.6/ISE_DS/ISE/lib/lin/
PATH=$PATH:/mnt/usb/opt/Xilinx/14.6/ISE_DS/ISE/bin/lin/
export PATH;
echo $PATH;
make synthesis implementation bitfile
