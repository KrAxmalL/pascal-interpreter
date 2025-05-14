program Fibonacci;

function fibonacci(position: Integer): Integer;
begin
    if position = 0 then
        fibonacci := 0
    else if position < 3 then
        fibonacci := 1
    else
        fibonacci := fibonacci(position - 1) + fibonacci(position - 2);
end;

begin
    writeln(fibonacci(15));
end.