# this is a comment
# as am i

%macro five 
  5
%endmacro

%macro ten
  5 five + +
%endmacro

begin:
  2 2 +
ten
  2 2 +
  32768
ten
  jump begin
