program input_test;

var name : string;
var a, b, c : integer;
begin
    write("Please enter your name:");
    read(name);
    write("Hello,", name);
    write("You can also use " + "+ to concatenate strings");
    write("You can input three numbers in a row. Try it!");
    read(a, b, c);
    write("You entered", a, "and", b, "and", c)
end.