# Written around Feb 29, 2020.
# This is my take on a simpler dataclass implementation.
from inspect import signature, Signature, Parameter


def set_attributes(constructor):
    return set_attributes_sig(constructor, signature(constructor))


def set_attributes_sig(constructor, sig):
    def modified_constructor(self, *args, **kwargs):
        constructor(self, *args, **kwargs)
        arguments = sig.bind(self, *args, **kwargs)
        arguments.apply_defaults()
        for attribute, value in arguments.arguments.items():
            if attribute == "self":
                continue
            setattr(self, attribute, value)
    return modified_constructor


def set_attributes_param(sig):
    return lambda constructor: set_attributes_sig(constructor, sig)


class Foo(object):
    @set_attributes
    def __init__(self, a, b, c=5, d=8):
        pass

f = Foo(3, b=7, d=9)
print(vars(f))


def simple_type(validator):
    con_sig = Signature([Parameter("self", Parameter.POSITIONAL_OR_KEYWORD)] + list(signature(validator).parameters.values()))
    class SimpleObject(object):
        @set_attributes_param(con_sig)
        def __init__(self, *args, **kwargs):
            validator(*args, **kwargs)
    return SimpleObject


@simple_type
def bar(a, b, c=5, d=8):
    pass


b = bar(3, b=7, d=9)
print(vars(b))
