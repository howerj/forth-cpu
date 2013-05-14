#/bin/bash
echo "gcov of \"./forth\" program";
echo "Run script with \"--complex\" argument for more detailed output";

if
  [[ $1 == "--complex" ]]
then
  if
    [ ! -f gcov.vim ];
  then
    echo "This scripts needs a file called \"gcov.vim\"";
    echo "This is avaiable here:\"https://github.com/vim-scripts/gcov.vim\"";
    echo "Downloading file:";
    wget  https://raw.github.com/vim-scripts/gcov.vim/master/syntax/gcov.vim;
  fi;

  if
    [ ! -f gcov.vim ];
  then
    echo "Download failed.";
    exit 1;
  fi;
fi;
   
./pretty.sh;
if 
  ! ./compile.sh --coverage;
then
  exit 1;
fi;
  

./forth << EOF
EOF
if
  [[ $1 == "--complex" ]]
then
  if
    ! gcov -abcf forth.c main.c;
  then
    exit 1;
  fi;
else
  if
    ! gcov forth.c main.c;
  then
    exit 1;
  fi;
fi;

if
  [[ $1 == "--complex" ]]
then
  vim -S gcov.vim -c "highlight Normal ctermfg=white ctermbg=black" forth.c.gcov;
else
  vim forth.c.gcov
fi;

exit 0;
