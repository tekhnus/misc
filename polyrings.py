# May 5, 2020.

# This is a draft of a tiny computer algrbra system.
# It only can work with factor rings of Z[t] by a principal ideal,
# allowing to perform equality checking.
# A simple, non-complete ideal simplification is implemented.

# What I tried to do is to write an implementation,
# in which the core concept is a "symbol" (e.g.,
# a symbol of addition, a symbol of generation, etc.).
# The symbols themselves are polymorphic; they have
# no own implementation. They are beging interpreted
# only in the contexts (for example, the symbol of generation
# is iterpreted in the context of a ring).

from functools import singledispatchmethod
from itertools import zip_longest

from sympy import poly, div, Poly
from sympy.abc import x as X

class sym:
    # a generic symbol.
    def __sub__(self, s):
        return binopsym(self, "-", s)

    def __add__(self, a):
        return binopsym(self, "+", a)

    def __mul__(self, m):
        return binopsym(self, "*", m)


class binopsym(sym):
    # a symbol constructed by joining two symbols with a binary operation.
    def __init__(self, a, op, b):
        self.a = a
        self.op = op
        self.b = b


class fgisym(sym):
    # a symbol representing a finitely generated ideal.
    def __init__(self, *args):
        self.gens = args


class uip:
    # ring of univariate polynomials over integers.
    def __init__(self):
        self.variable = sym()

    def test_equal(self, a, b):
        ar = self._represent(a)
        br = self._represent(b)
        # print("test_equal", ar, br)
        pairs = tuple(zip_longest(ar, br, fillvalue=0))
        return all(x == y for x, y in pairs)

    def test_congruent(self, a, b, m):
        dr = self._represent(a - b)
        mr = self._represent_ideal(m)
        # qr, rr = self._divide(dr, mr)
        # return all(x == 0 for x in rr)
        return self._test_ideal_member(dr, mr)

    @singledispatchmethod
    def _represent(self, s):
        if s is self.variable:
            return (0, 1,)
        raise TypeError(repr(s))

    @_represent.register
    def _(self, s:int):
        if s == 0:
            return ()
        return (s,)

    @_represent.register
    def _(self, s:binopsym):
        ar = self._represent(s.a)
        br = self._represent(s.b)
        pairs = tuple(zip_longest(ar, br, fillvalue=0))
        if s.op == "+":
            return tuple(x + y for x, y in pairs)
        if s.op == "-":
            return tuple(x - y for x, y in pairs)
        if s.op == "*":
            maxdeg = len(pairs)
            pairs = pairs + ((0, 0),) * maxdeg
            res = ()
            for n in range(len(pairs)):
                res = res + (sum(pairs[j][0] * pairs[n - j][1] for j in range(n + 1)),)
            return res
        raise ValueError(s.op)

    @singledispatchmethod
    def _represent_ideal(self, s):
        raise TypeError(repr(s))

    @_represent_ideal.register
    def _(self, s:fgisym):
        return tuple(self._represent(g) for g in s.gens)

    @_represent_ideal.register
    def _(self, s:binopsym):
        ar = self._represent_ideal(s.a)
        br = self._represent_ideal(s.b)
        if s.op == "+":
            return ar + br
        raise ValueError(s.op)

    def _test_ideal_member(self, xr, ir):
        ir = self._normalize(ir)

        if ir == ():
            raise NotImplementedError("Zero ideal membership not implemented :)")
        if len(ir) == 1:
            igr, = ir
            return self._divisible(xr, igr)
        else:
            raise NotImplementedError("Ideals with multiple generators not implemented")

    def _normalize(self, ir):
        # naive is better than nothing
        redundant = set()
        for ix in range(len(ir)):
            for iy in range(len(ir)):
                if ix == iy:
                    continue
                if self._divisible(ir[ix], ir[iy]):
                    redundant.add(ix)
                    break
        return tuple(ir[j] for j in range(len(ir)) if j not in redundant)

    def _divisible(self, xr, dr):
        _, remr = self._divide(xr, dr)
        return all(x == 0 for x in remr)

    def _divide(self, xr, dr):
        xs = sum(c * X**p for p, c in enumerate(xr))
        ds = sum(c * X**p for p, c in enumerate(dr))
        # print(xs, ds)
        qs, rs = div(xs, ds)
        qr, rr = tuple(reversed(Poly(qs, X).all_coeffs())), tuple(reversed(Poly(rs, X).all_coeffs()))
        return qr, rr


class frpi:
    # factor ring by principal ideal
    # (well, the ideal actually might be non-principal, but the equality checking is not implemented for this case).
    def __init__(self, ring, s):
        self._protoring = ring
        self._s = s

    def test_equal(self, a, b):
        return self._protoring.test_congruent(a, b, self._s)

    def test_congruent(self, a, b, m):
        superid = self._s + m
        return self._protoring.test_congruent(a, b, superid)


zt = uip()
t = zt.variable

print(zt.test_equal((t - 1) * (t + 1), t*t - 1))
print(zt.test_equal((t - 1) * (t + 1), 0))
print(zt.test_equal((t - 1), 0))
print(zt.test_equal(t*t, 0))
print()

r = frpi(zt, fgisym(t*t - 1))
print(r.test_equal((t - 1) * (t + 1), t*t - 1))
print(r.test_equal((t - 1) * (t + 1), 0))
print(r.test_equal((t - 1), 0))
print(r.test_equal(t*t, 0))
print()

m = frpi(r, fgisym(t - 1))
print(m.test_equal((t - 1) * (t + 1), t*t - 1))
print(m.test_equal((t - 1) * (t + 1), 0))
print(m.test_equal((t - 1), 0))
print(m.test_equal(t*t, 0))
print()
