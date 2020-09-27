def with_fields(*args):
    field_names = args

    class WithFields(object):
        def __init__(self, *args):
            for name, value in zip(field_names, args):
                setattr(self, name, value)
            self._field_names = field_names

        def __eq__(self, another):
            return type(self) == type(another) \
               and all(getattr(self, name) == getattr(another, name)
                       for name in self._field_names)

        def __hash__(self):
            return sum(hash(getattr(self, name))
                       for name in self._field_names)

    return WithFields


class Inference(object):
    def __init__(self, premises, conclusion):
        self._premises = premises
        self._conclusion = conclusion

    def primitively_entails(self, another):
        return all(premise.holds(Sys, another._premises)
                   for premise in self._premises) \
           and (another._conclusion.holds(Sys, self._premises
                                                              | another._premises
                                                              | {self._conclusion})
                    or self._conclusion.implies(Sys, self._premises | another._premises, another._conclusion)
                    )

    def __repr__(self):
        return '{} => {}'.format(self._premises, self._conclusion)


class AssumptionContext(object):
    def __init__(self, proof, wff):
        self._proof = proof
        self._wff = wff

    def __enter__(self):
        pass

    def __exit__(self, _1, _2, _3):
        self._proof.forget(self._wff)


class Sys(object):
    @staticmethod
    def obviously(premises, conclusion):
        return conclusion in premises


class Proof(object):
    def __init__(self):
        self._premises = set()
        self._knowledge = [trivium]

    def assume(self, premise):
        self._premises.add(premise)
        return AssumptionContext(self, premise)

    def forget(self, premise):
        self._premises.remove(premise)

    def obviously(self, premises, conclusion):
        inference = Inference(premises, conclusion)
        return any(k.primitively_entails(inference) for k in self._knowledge)

    def thus(self, conclusion):
        inference = Inference(set(self._premises), conclusion)

        if conclusion.holds(self, self._premises):
            self._knowledge.append(inference)
            return

        for x in self._premises:
            if x.implies(self, self._premises - {x}, conclusion):
                self._knowledge.append(inference)
                return

        print('KNOWLEDGE:')
        for knowledge in self._knowledge:
            print(' ', knowledge)
        print('TRYING:')
        print(' ', inference)
        raise ValueError('Cannot prove')


class Formula(object):
    def __and__(self, another):
        return Conjunction(self, another)

    def __or__(self, another):
        return Disjunction(self, another)

    def __invert__(self):
        return Negation(self)

    def __mod__(self, another):
        return Equivalence(self, another)

    def holds(self, system, premises):
        if(self in premises):
            return True
        return system.obviously(premises, self)

    def implies(self, system, other_premises, conclusion):
        return False



class Truth(Formula, with_fields()):
    def __repr__(self):
        return "T"


truth = Truth()
trivium = Inference(set(), truth)


class Conjunction(Formula, with_fields("left", "right")):
    def __repr__(self):
        return "({} ^ {})".format(self.left, self.right)

    def holds(self, system, premises):
        return (system.obviously(premises, self.left)
            and system.obviously(premises, self.right)) \
             or super().holds(system, premises)

    def implies(self, system, other_premises, conclusion):
        return (system.obviously(other_premises | {self.right}, conclusion)
                or system.obviously(other_premises | {self.left}, conclusion)) \
            or super().implies(system, other_premises, conclusion)


class Disjunction(Formula, with_fields("left", "right")):
    def __repr__(self):
        return "({} v {})".format(self.left, self.right)

    def holds(self, system, premises):
        return system.obviously(premises, self.right) \
            or system.obviously(premises, self.left) \
            or (self.left == ~self.right) or (~self.left == self.right) \
            or super().holds(system, premises)

    def implies(self, system, other_premises, conclusion):
        return (system.obviously(other_premises | {self.right}, conclusion)
                and system.obviously(other_premises | {self.left}, conclusion)) \
            or super().implies(system, other_premises, conclusion)


class Negation(Formula, with_fields("of")):
    def __repr__(self):
        return "~{}".format(self.of)

    def holds(self, system, premises):
        return system.obviously(premises | {self.of}, contradiction) \
                or super().holds(system, premises)



class Equivalence(Formula, with_fields("left", "right")):
    def __repr__(self):
        return "({} <> {})".format(self.left, self.right)

    def holds(self, system, premises):
        return (system.obviously(premises | {self.left}, self.right) \
           and system.obviously(premises | {self.right}, self.left)) \
           or super().holds(system, premises)


class Symbol(Formula, with_fields("name")):
    def __repr__(self):
        return self.name


class Contradiction(Formula, with_fields()):
    def __repr__(self):
        return '⚡'

    def holds(self, system, premises):
        return any(x == ~y for x in premises for y in premises) \
            or super().holds(system, premises)

    def implies(self, system, other_premises, conclusion):
        return True


contradiction = Contradiction()
