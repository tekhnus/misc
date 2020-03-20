import json
from functools import reduce

from . import worker


class Branch:
    def worker(self):
        raise NotImplementedError("worker not implemented")


class ConcatenateJSON(Branch):
    def __init__(self, filename):
        self._filename = filename

    def worker(self):
        return worker.ConcatenateJSON(self._filename)


class Compose(Branch):
    def __init__(self, left, right):
        self._left = left
        self._right = right

    def worker(self):
        return worker.Compose(self._left.worker(), self._right.worker())


def compose(*args):
    return reduce(Compose, args)


class ExtractColumn(Branch):
    def __init__(self, column):
        self._column = column

    def worker(self):
        return worker.ExtractColumn(self._column)


class Product(Branch):
    def __init__(self, one, two):
        self._one = one
        self._two = two

    def worker(self):
        return worker.Product(self._one.worker(), self._two.worker())


def product(*args):
    return reduce(Product, args)


class Add(Branch):
    def __init__(self, left, right):
        self._left = left
        self._right = right

    def worker(self):
        return worker.BinaryOperator(self._left.worker(), lambda a, b: a + b, self._right.worker())


class Sum(Branch):
    def __init__(self, root):
        self._root = root

    def worker(self):
        return worker.Accumulator(self._root.worker(), lambda a, b: a + b)


class PartitionBy(Branch):
    def __init__(self, key):
        self._key = key

    def worker(self):
        return worker.PartitionBy(self._key.worker())
