program HelloWorld;

var a: Integer;
var b: Boolean;
var c: Integer;
var e: Real;
var x: Integer;
var cond: Boolean;
var t: Real;

function add(c: Integer; d: Integer): Integer;
begin
   add := c + d;
end;

procedure print(k: Integer);
   function divide(divisor: Integer): Real;
   begin
      divide := divisor / k;
   end;
begin
   e := divide(14);
end;

begin
   a := -1;
   a := add(1, 4);
   print(5);
   b := a > e;
   if e > 5 then
      begin
         b := (add(a, c) > 5) = (4 / a = c);
         print(add(a, c));
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