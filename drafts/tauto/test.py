from tauto import Symbol, Proof, contradiction


a = Symbol('a')
b = Symbol('b')


proof = Proof()


with proof.assume (~(a & b)):
    with proof.assume (~(~a | ~b)):
        with proof.assume (~a | ~b):
            proof.thus (contradiction)

        with proof.assume (~a):
            proof.thus (contradiction)
            proof.thus (a)
        with proof.assume (a | ~a):
            proof.thus (a)
        proof.thus (a)

        with proof.assume (~b):
            proof.thus (contradiction)
            proof.thus (b)
        with proof.assume (b | ~b):
            proof.thus (b)
        proof.thus (b)

        proof.thus (a & b)
        proof.thus (contradiction)
        proof.thus (~a | ~b)

    with proof.assume ((~a | ~b) | ~(~a | ~b)):
        proof.thus (~a | ~b)
    proof.thus (~a | ~b)

with proof.assume (~a | ~b):
    with proof.assume (a & b):
        proof.thus (a)
        with proof.assume (~a):
            proof.thus (contradiction)

        proof.thus (b)
        with proof.assume (~b):
            proof.thus (contradiction)

        proof.thus (contradiction)

    proof.thus (~(a & b))

proof.thus (~(a & b) % (~a | ~b))
