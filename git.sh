#!/bin/bash
echo "Simple git add/commit/push script";

if
  [ -z "$1" ]
then
  echo "To use run type ./git.sh \"commit message\".";
  echo "No commit message detected, exiting.";
  exit 1;
fi;

if 
  ! git add .; 
then 
  echo "\"git add .\" failed."; 
  exit 1; 
fi;

if 
  ! git commit -m "$1"; 
then 
  echo "\"git commit -m '$1'\" failed.";  
  exit 1; 
fi;

if 
  ! git push -u origin master; 
then 
  echo "\"git push -u origin master\" failed."; 
  exit 1; 
fi;

echo "Git add/commit/push done.";
exit 0;
