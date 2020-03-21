from threading import Thread, Lock, Semaphore


class Rendezvous:
    def __init__(self, count):
        self._keeper = Lock()
        self._count = count
        self._reached = 0
        self._start = Semaphore(value=0)

    def reached(self):
        with self._keeper:
            self._reached += 1

        if self._reached == self._count:
            self._start.release()

        self._start.acquire()
        self._start.release()


io = Lock()
rn = Rendezvous(2)


def worker_a():
    with io:
        print("A1")
    rn.reached()
    with io:
        print("A2")


def worker_b():
    with io:
        print("B1")
    rn.reached()
    with io:
        print("B2")


a = Thread(target=worker_a)
b = Thread(target=worker_b)

a.start()
b.start()

a.join()
b.join()
