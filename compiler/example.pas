var x , squ ;

procedure square ;
begin
   squ := x * x
end ;
 
begin
   x := 1 ;
   while x <= 10 do
   begin
      call square ;
      {      ! squ ;  ! not implemented yet }
      x := x + 1
   end
end.
