program array_test;

var arr : array 10 of integer;
var i : integer;
begin
    i := 1;
    while i < 11 do
    begin
        arr[i - 1] := i * i;
        i := i + 1
    end;

    i := 1;
    while i < 11 do
    begin
        write(arr[i - 1]);
        i := i + 1
    end;

    i := 1;
    while i < 11 do
    begin
        dec(arr[i - 1]);
        i := i + 1
    end;

    i := 1;
    while i < 11 do
    begin
        write(arr[i - 1]);
        i := i + 1
    end
end.