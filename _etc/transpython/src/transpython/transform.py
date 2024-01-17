from .functor import Covariant


class BaseTransform(object):
    def __or__(self, after):
        return self.Composition(self, after)


class Transform(BaseTransform):
    class Composition(BaseTransform):
        def __init__(self, left, right):
            self.left = left
            self.right = right

        @property
        def Composition(self):
            return type(self)

        def __getattr__(self, attr):
            return Covariant(attr).__get__(self, type(self))

        def __repr__(self):
            return "{!r} | {!r}".format(self.left, self.right)
