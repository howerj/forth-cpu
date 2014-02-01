#!/bin/bash
echo "Upload script"
if [ ! -d /mnt/usb/opt ]; then
  sudo mount /dev/sda2 /mnt/usb/
fi
PATH=$PATH:/mnt/usb/opt/Xilinx/14.6/ISE_DS/ISE/lib/lin/
PATH=$PATH:/mnt/usb/opt/Xilinx/14.6/ISE_DS/ISE/bin/lin/
export PATH;
echo $PATH;
make upload
