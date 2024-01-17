-- This is Greenplum dialect
-- Checks all magic squares with square numbers in center and corners
-- with the value in center <= 10**16
-- and the arithmetic progressions at the diagonals correspond to *primitive* pythagorean triples.
-- No such squares with seven square numbers :'(
drop table if exists numbers;

create table numbers as
select x
from generate_series(1, 10000) as x
distributed by (x);

drop table if exists progressions;

create table progressions as
select (m::numeric(32) * m + n::numeric(32) * n)*(m::numeric(32) * m + n::numeric(32) * n) as b2,
       4 * m::numeric(32) * n * (m::numeric(32) * m - n::numeric(32) * n) as d
from numbers as m(m)
cross join numbers as n(n)
where m > n
distributed by (b2);

with
  squares as (
  select b2 as a_11,
         b2 - diagonal_one.d as a_00,
         b2 + diagonal_one.d as a_22,
         b2 - diagonal_two.d as a_02,
         b2 + diagonal_two.d as a_20,
         b2 + diagonal_one.d + diagonal_two.d as a_01,
         b2 + diagonal_one.d - diagonal_two.d as a_10,
         b2 - diagonal_one.d + diagonal_two.d as a_12,
         b2 - diagonal_one.d - diagonal_two.d as a_21
  from progressions as diagonal_one
  inner join progressions as diagonal_two using (b2)
  where diagonal_one.d < diagonal_two.d
    and diagonal_one.d + diagonal_two.d < b2
),
  squares_with_flags as (
  select *,
         ((sqrt(a_01)::bigint)^2 = a_01)::int as a_01_flg,
         ((sqrt(a_10)::bigint)^2 = a_10)::int as a_10_flg,
         ((sqrt(a_12)::bigint)^2 = a_12)::int as a_12_flg,
         ((sqrt(a_21)::bigint)^2 = a_21)::int as a_21_flg
  from squares
  )
select 5 + a_01_flg + a_10_flg + a_12_flg + a_21_flg as square_cnt,
       3*a_11 as s,
       a_00, a_01, a_02, a_10, a_11, a_12, a_20, a_21, a_22
from squares_with_flags
where (a_01_flg + a_10_flg + a_12_flg + a_21_flg) > 0
order by square_cnt desc, s asc;
