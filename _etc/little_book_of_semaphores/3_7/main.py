from threading import Thread, Lock, Semaphore


class Turnstile:
    def __init__(self, opened=False):
        self._token = Semaphore(value=int(opened))

    def open(self):
        self._token.release()

    def close(self):
        self._token.acquire()

    def pass_through(self):
        self._token.acquire()
        self._token.release()


class Barrier:
    def __init__(self, count):
        self._count = count

        self._lock = Lock()
        self._inside = 0
    
        self._entrance = Turnstile(opened=True)
        self._exit = Turnstile(opened=False)

    def reached(self):
        self._entrance.pass_through()
        with self._lock:
            self._inside += 1
            if self._inside == self._count:
                self._entrance.close()
                self._exit.open()

        self._exit.pass_through()
        with self._lock:
            self._inside -= 1
            if self._inside == 0:
                self._exit.close()
                self._entrance.open()


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
