# this is a comment
# as am i

%macro five 
  5
%endmacro

%macro ten
  five five + +
%endmacro

%ifndef
%ifdef
%if

begin:
  2 2 +
  ten
  2 2 +
  32768
  ten
jump begin
