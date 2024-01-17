# TODO: check this solution.
from threading import Thread, Semaphore


class StrongMutex:
    def __init__(self):
        self._busy = False
        self._waiting = []
        self._lock = Semaphore(value=1)

    def acquire(self):
        self._lock.acquire()
        if self._busy:
            waiter = Semaphore(value=0)
            self._waiting.append(waiter)
        else:
            self._busy = True
            waiter = Semaphore(value=1)
        self._lock.release()
        waiter.acquire()
        

    def release(self):
        self._lock.acquire()
        if self._waiting:
            waiter = self._waiting.pop(0)
        else:
            self._busy = False
            waiter = Semaphore(value=0)
        self._lock.release()
        waiter.release()
