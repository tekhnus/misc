program trivial_test;

function my_inc(a : integer) : void
begin
    a := a + 1
end;

var i : integer;
begin
    i := 5;
    write(i);
    inc(i); { native one }
    write(i);
    my_inc(i); { declared one }
    write(i)
end.