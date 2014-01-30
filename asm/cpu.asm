# this is a comment
# as am i

%macro ten
  5 5 +
%endmacro
begin:
  2 2 +
ten
  2 2 +
  32768

ten
  +
  jump begin
