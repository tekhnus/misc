-- This is YQL dialect
-- Checks all magic squares with square numbers in center and corners
-- with the value in center <= 900000 ** 4, which is approximately 6.5 * 10**23
-- and the arithmetic progressions at the diagonals correspond to *primitive* pythagorean triples.
-- No such squares with seven square numbers :'(
$Decimal = Decimal(33, 0);

$maxA11Rt = 900000L;
-- $maxA11 = cast($maxA11Rt as $Decimal) * $maxA11Rt * $maxA11Rt * $maxA11Rt;
$rangeTable = ($M) -> (ListMap(ListFromRange(1, 1 + $M), ($x) -> (AsStruct($x as value))));
/*$isSquareOld = ($x) -> {
    $rt = cast(Math::Sqrt(cast($x as Double)) as Int64);
    return cast($rt * $rt = $x as Int8);
};*/
$script = @@
def isSquare(x):
    if x < 0:
        return False
    rt = x.sqrt().to_integral()
    return rt * rt == x
@@;

$isSquare = Python3::isSquare(Callable<(Decimal(33, 0)?)->Bool>, $script);

insert into @rangeTable
select value
from as_table($rangeTable($maxA11Rt));

commit;

insert into `basic-progression`
  with truncate
select (m.value*m.value+n.value*n.value) as b,
       abs(m.value*m.value-n.value*n.value - 2*m.value*n.value) as a
from @rangeTable as m
  cross join @rangeTable as n
where m.value > n.value
  and (m.value*m.value+n.value*n.value) <= ($maxA11Rt * $maxA11Rt)
order by b, a;
commit;

$sqr = ($x) -> {
    $dec = cast($x as $Decimal);
    return $dec * $dec;
};
$square = (
  select $sqr(diagonal_one.b) as a_11,
         $sqr(diagonal_one.a) as a_00,
         2 * $sqr(diagonal_one.b) - $sqr(diagonal_one.a) as a_22,
         $sqr(diagonal_two.a) as a_02,
         2 * $sqr(diagonal_one.b) - $sqr(diagonal_two.a) as a_20,
         3 * $sqr(diagonal_one.b) - $sqr(diagonal_one.a) - $sqr(diagonal_two.a) as a_01,
         $sqr(diagonal_one.b) - $sqr(diagonal_one.a) + $sqr(diagonal_two.a) as a_10,
         $sqr(diagonal_one.a) + $sqr(diagonal_one.b) - $sqr(diagonal_two.a) as a_12,
         $sqr(diagonal_one.a) - $sqr(diagonal_one.b) + $sqr(diagonal_two.a) as a_21
  from `basic-progression` as diagonal_one
  inner join `basic-progression` as diagonal_two using (b)
  where diagonal_one.a < diagonal_two.a
  --  and diagonal_one.d + diagonal_two.d < diagonal_one.b2
);

$square_with_flags = (
  select a_00, a_01, a_02, a_10, a_11, a_12, a_20, a_21, a_22,
         cast($isSquare(a_01) as Int8) as a_01_flg,
         cast($isSquare(a_10) as Int8) as a_10_flg,
         cast($isSquare(a_12) as Int8) as a_12_flg,
         cast($isSquare(a_21) as Int8) as a_21_flg
  from $square
);

select 5 + a_01_flg + a_10_flg + a_12_flg + a_21_flg as square_cnt,
       3*a_11 as s,
       a_00, a_01, a_02, a_10, a_11, a_12, a_20, a_21, a_22
from $square_with_flags
where (a_01_flg + a_10_flg + a_12_flg + a_21_flg) > 0
order by square_cnt desc, s asc;
