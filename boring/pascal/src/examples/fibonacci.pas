program fibonacci;

{ very slow! }
function fib(n : integer) : integer
begin
    if n = 0 then fib := 0
    else if n = 1 then fib := 1
    else fib := fib(n - 2) + fib(n - 1)
end;

var x : integer;
begin
    x := 0;
    while x < 10 do begin
        write(fib(x));
        inc(x)
    end
end.