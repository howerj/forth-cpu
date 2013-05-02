#/bin/bash
BLUE="\e[1;34m";
GREEN="\e[1;32m";
RED="\e[1;31m";
DEFAULT="\e[0m";
if
  [ -f $1 ] && [ ! -z $1 ];
then
  COLORMEPRETTY=$(untex $1 | wc);
  echo -e "$BLUE[lines:word:bytes]>$GREEN$COLORMEPRETTY$DEFAULT";
  exit 0;
else
  echo -e "$BLUE > No file specified, running wc on *.tex instead.$DEFAULT";
  COLORMEPRETTY=$(untex *.tex | wc);
  echo -e "$BLUE[lines:word:bytes]>$GREEN$COLORMEPRETTY$DEFAULT";
  exit 0;
fi;
