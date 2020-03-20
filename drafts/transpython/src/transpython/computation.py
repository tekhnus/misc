import marshal

from .transform import Transform


class Computation(Transform):
    class Composition(Transform.Composition):
        def __call__(self, argument):
            return self.right(self.left(argument))

    def __init__(self, function):
        self._function = function

    def __call__(self, argument):
        return self._function(argument)

    @property
    def stream_mapper(self):
        return Computation(
            lambda stream: (self._function(x) for x in stream)
        )

    @property
    def as_json_pipe(self):
        from . import Process

        return Process([
            "python", "-c",
            """
import json
import marshal
from types import FunctionType
from sys import stdin, stdout


code = marshal.loads({code!r})
function = FunctionType(code, {{}}, {name!r})

try:
    inp = json.load(stdin)
except ValueError:
    inp = None

outp = function(inp)
json.dump(outp, stdout)
"""
            .format(
                code=marshal.dumps(self._function.__code__),
                name=self._function.__name__
            )
        ])

    def __repr__(self):
        return self._function.__name__
