#/bin/bash
BLUE="\e[1;34m";
GREEN="\e[1;32m";
RED="\e[1;31m";
DEFAULT="\e[0m";
if
  [ -f $1.tex ];
then
  COLORMEPRETTY=$(untex $1.tex | wc);
  echo -e "$BLUE[lines:word:bytes]>$GREEN$COLORMEPRETTY$DEFAULT";
  exit 0;
else
  echo -e "$RED File \"$1\" not found.$DEFAULT";
  exit 1;
fi;
