# Checks all magic squares with square numbers in center and corners
# with the value in center <= 10**15.
# No such squares with seven square numbers :'(
import math
import itertools


def square_progressions(max_middle_term):
    sqrt = math.sqrt
    sqrt_max_middle_term = sqrt(max_middle_term)

    for n in range(1, 1 + int(sqrt(sqrt_max_middle_term))):
        if n % 1_000 == 0:
            print("progression generation progress: {:.0%}".format(n / sqrt(sqrt_max_middle_term)))

        for m in range(n + 1, 1 + int(sqrt(sqrt_max_middle_term - n ** 2))):
            congruum = 4 * m * n * (m ** 2 - n ** 2)
            b = m ** 2 + n ** 2
            b2 = b ** 2
            a2 = b2 - congruum
            c2 = b2 + congruum
            for mul in range(1, 1 + int(sqrt_max_middle_term / b)):
                mul2 = mul ** 2
                yield a2 * mul2, b2 * mul2, c2 * mul2


def main():
    max_a11 = 10 ** 15

    sq = {n ** 2 for n in range(1, 1 + int(math.sqrt(3 * max_a11)))}
    print("generated square number set")

    middle_term = lambda p: p[1]
    ps = set(square_progressions(max_a11))
    print("generated square progressions, count:", len(ps))

    for a11, possible_diagonals in itertools.groupby(
        sorted(ps, key=middle_term), key=middle_term
    ):
        for (a00, _, a22), (a02, _, a20) in itertools.combinations(
            possible_diagonals, 2
        ):
            s = 3 * a11
            a01 = s - a00 - a02
            a21 = s - a20 - a22
            a10 = s - a00 - a20
            a12 = s - a22 - a02

            if a01 in sq or a10 in sq or a12 in sq or a21 in sq:
                square_count = (
                    5
                    + int(a01 in sq)
                    + int(a10 in sq)
                    + int(a12 in sq)
                    + int(a21 in sq)
                )
                if square_count > 6 or (
                    a01 > 0 and a10 > 0 and a12 > 0 and a21 > 0 and s % 10_000_000 == 0
                ):
                    print(
                        f"{square_count} {s:,}\n    {a00} {a01} {a02} {a10} {a11} {a12} {a20} {a21} {a22}"
                    )


if __name__ == "__main__":
    main()
