program Fibonacci;

function calculateFibonacci(position: Integer): Integer;
begin
if position < 0 then
  calculateFibonacci := -1
else if position = 0 then
  calculateFibonacci := 0
else if position < 3 then
  calculateFibonacci := 1
else
  calculateFibonacci := calculateFibonacci(position - 1) +
                        calculateFibonacci(position - 2);
end;

var index: Integer;
var amount: Integer;

begin
  write('Please, enter the required number of Fibonacci numbers to calculate: ');
  read(amount);

  index := 0;
  while (index <= amount) do
    begin
      writeln('Fibonacci number at position ', index, ' - ', calculateFibonacci(index));
      index := index + 1;
    end;
end.
