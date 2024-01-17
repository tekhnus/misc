import json
import pathlib


class Worker:
    def process(self, key, row):
        raise NotImplementedError("process not implemented")

    def close(self):
        return iter([])


class Compose(Worker):
    def __init__(self, left, right):
        self._left = left
        self._right = right

    def process(self, key, row):
        for left_key, left_row in self._left.process(key, row):
            for right_key, right_row in self._right.process(left_key, left_row):
                yield right_key, right_row

    def close(self):
        for left_key, left_row in self._left.close():
            for right_key, right_row in self._right.process(left_key, left_row):
                yield right_key, right_row
        for right_key, right_row in self._right.close():
            yield right_key, right_row


class ExtractColumn(Worker):
    def __init__(self, column):
        self._column = column

    def process(self, key, row):
        yield key, {self._column: row[self._column]}


class Product(Worker):
    def __init__(self, one, another):
        self._one = one
        self._another = another

    def process(self, key, row):
        for (ok, one), (tk, two) in zip(self._one.process(key, row), self._another.process(key, row)):
            yield ok, dict(one, **two)

    def close(self):
        for (ok, one), (tk, two) in zip(self._one.close(), self._another.close()):
            yield ok, dict(one, **two)


class ConcatenateJSON(Worker):
    def __init__(self, filename):
        self._filename = pathlib.Path(filename)

    def process(self, key, row):
        yield None, row

    def close(self):
        with self._filename.open() as reader:
            for line in reader:
                yield None, json.loads(line)


class BinaryOperator(Worker):
    def __init__(self, left, operation, right):
        self._left = left
        self._operation = operation
        self._right = right

    def process(self, key, row):
        for (lk, left), (rk, right) in zip(self._left.process(key, row), self._right.process(key, row)):
            left_val = next(iter(left.values()))
            right_val = next(iter(right.values()))
            yield lk, {"_unnamed_": self._operation(left_val, right_val)}


class Accumulator(Worker):
    def __init__(self, root, accumulator):
        self._root = root
        self._accumulator = accumulator
        self._init = {}

    def process(self, key, row):
        for outkey, root in self._root.process(key, row):
            val = next(iter(root.values()))
            if outkey in self._init:
                self._init[outkey] = self._accumulator(self._init[outkey], val)
            else:
                self._init[outkey] = val
        return iter([])
    
    def close(self):
        for key, val in self._init.items():
            yield key, {"_unnamed_": val}


class PartitionBy(Worker):
    def __init__(self, keyer):
        self._keyer = keyer

    def process(self, key, row):
        _, actual_key = next(self._keyer.process(key, row))
        yield repr(actual_key), row
