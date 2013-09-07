#!/bin/bash
if [ ! -d /mnt/usb/opt ] then
  sudo mnt /mnt/sda2 /mnt/usb
fi
PATH=$PATH:/mnt/usb/opt/Xilinx/14.6/ISE_DS/ISE/lib/lin;
