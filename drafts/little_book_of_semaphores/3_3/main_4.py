from threading import Thread, Lock, Semaphore


class Turnstile:
    def __init__(self):
        self._pass = Semaphore(value=0)

    def lift(self):
        self._pass.release()

    def let_through(self):
        self._pass.acquire()
        self._pass.release()


class Barrier:
    def __init__(self, count):
        self._keeper = Lock()
        self._count = count
        self._reached = 0
        self._start = Turnstile()

    def reached(self):
        with self._keeper:
            self._reached += 1

        if self._reached == self._count:
            self._start.lift()

        self._start.let_through()


io = Lock()
br = Barrier(2)


def worker_a():
    with io:
        print("A1")
    br.reached()
    with io:
        print("A2")


def worker_b():
    with io:
        print("B1")
    br.reached()
    with io:
        print("B2")


a = Thread(target=worker_a)
b = Thread(target=worker_b)

a.start()
b.start()

a.join()
b.join()
