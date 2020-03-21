from threading import Thread, Lock, Semaphore


class Rendezvous:
    def __init__(self, *names):
        self._tokens = {name: Semaphore(value=0)
                        for name in names}
        self._count = len(names)

    def reached(self, name):
        for _ in range(self._count):
            self._tokens[name].release()
        for other_name, other_token in self._tokens.items():
            other_token.acquire()


io = Lock()
rn = Rendezvous("A", "B")


def worker_a():
    with io:
        print("A1")
    rn.reached("A")
    with io:
        print("A2")


def worker_b():
    with io:
        print("B1")
    rn.reached("B")
    with io:
        print("B2")


a = Thread(target=worker_a)
a.start()
b = Thread(target=worker_b)
b.start()

a.join()
b.join()
