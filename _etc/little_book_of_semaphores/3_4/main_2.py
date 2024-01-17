from threading import Thread, Semaphore


class Mutex:
    def __init__(self):
        self._token = Semaphore()

    def __enter__(self):
        self._token.acquire()

    def __exit__(self, _1, _2, _3):
        self._token.release()

    
count = 0
mu = Mutex()


def worker_a():
    global count
    with mu:
        count += 1


def worker_b():
    global count
    with mu:
        count += 1


a = Thread(target=worker_a)
b = Thread(target=worker_b)

a.start()
b.start()

a.join()
b.join()

print(count)
