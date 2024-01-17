from threading import Thread, Lock, Semaphore


class Turnstile:
    def __init__(self, count):
        self._token = Semaphore(value=count)

    def open(self, count):
        for _ in range(count):
            self._token.release()

    def pass_through(self):
        self._token.acquire()


class Barrier:
    def __init__(self, count):
        self._count = count

        self._lock = Lock()
        self._inside = 0
    
        self._entrance = Turnstile(count=self._count)
        self._exit = Turnstile(count=0)

    def reached(self):
        self._entrance.pass_through()
        with self._lock:
            self._inside += 1
            if self._inside == self._count:
                self._exit.open(self._count)

        self._exit.pass_through()
        with self._lock:
            self._inside -= 1
            if self._inside == 0:
                self._entrance.open(self._count)


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
