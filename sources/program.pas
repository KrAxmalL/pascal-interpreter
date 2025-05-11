program HelloWorld;

var a: Integer;
var b: Boolean;
var e: Integer;
var x: Integer;
var cond: Boolean;

function add(c: Integer; d: Integer): Integer;
begin
   add := c + d;
end;

procedure print(k: Integer);
   function divide(divisor: Integer): Integer;
   begin
      divide := divisor / k;
   end;
begin
   e := divide(14);
end;

begin
   a := add(1, 4);
   print(5);
   b := a > e;
   if e > 5 then
      begin
         b := (add(a, e) > 5) = (4 / a = e);
         print(add(a, e));
      end
   else
      print(1);
   x := -1;
   cond := true;
   while (x < 10) = (e > 2) do
      begin
         x := x + 3;
      end;
end.