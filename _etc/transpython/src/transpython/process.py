import subprocess
import shlex

from .transform import Transform
from .functor import Contravariant


class Process(Transform):
    def __init__(self, args):
        self._args = args

    @property
    def remote(self):
        return RemoteProcess(self._args)

    @property
    def runner(self):
        from . import Computation

        @Computation
        def run_on_input(stream):
            process = subprocess.Popen(
                self._args,
                stdin=stream, stdout=subprocess.PIPE,
                universal_newlines=True
            )
            return process.stdout

        return run_on_input

    def __repr__(self):
        return " ".join(
            "'{}'".format(arg) for arg in self._args
        )


class Remote(Transform):
    class Composition(Transform.Composition):
        command = Contravariant("command")


class RemoteProcess(Remote):
    def __init__(self, args):
        self._args = args

    @property
    def command(self):
        from . import Computation

        @Computation
        def prepend_before(tail):
            return " ".join(self._args) + " | " + tail

        return prepend_before


class Tunnel(Remote):
    def __init__(self, destination):
        self._destination = destination

    @property
    def command(self):
        from . import Computation

        @Computation
        def prepend_before(tail):
            return "ssh " + self._destination + " " + shlex.quote(tail)

        return prepend_before
