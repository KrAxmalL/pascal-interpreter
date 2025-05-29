program Power;

function pow(number: Integer; power: Integer): Integer;
var index: Integer;
begin
  if (power = 0) then
    pow := 1
  else
    begin
      index := 1;
      pow := number;
      while (index < power) do
      begin
        pow := pow * number;
        index := index + 1;
      end;
    end;
end;

var number: Integer;
var expectedPow: Integer;

begin
  write('Please, enter the number and pow to calculate: ');
  read(number, expectedPow);

  writeln(number, ' to the power of ', expectedPow, ' = ', pow(number, expectedPow));
end.