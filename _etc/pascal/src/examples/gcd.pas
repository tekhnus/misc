program gcd;

function gcd(a, b : integer) : integer
begin
    if b = 0 then gcd := a
    else gcd := gcd(b, a % b)
end;

var a, b : integer;
begin
    write("Enter a and b:");
    read(a, b);
    write("GCD(a, b) =", gcd(a, b))
end.