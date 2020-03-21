# This can (and probably will) block, as the cycle in acquire() is not atomic
from threading import Thread, Semaphore


class StrongMutex:
    def __init__(self):
        self._semaphore = Semaphore(value=1)

        self._entered_id = 0
        self._lock_entered_id = Semaphore(value=1)

        self._exited_id = 0
        self._lock_exited_id = Semaphore(value=1)

    def acquire(self):
        self._lock_entered_id.acquire()
        endered_id = self._entered_id = self._entered_id + 1
        self._lock_entered_id.release()

        for _ in range(entered_id):
            self._semaphore.acquire()

    def release(self):
        self._lock_exited_id.acquire()
        exited_id = self._exited_id = self._exited_id + 1
        self._lock_exited_id.release()

        for _ in range(exited_id + 1):
            self._semaphore.release()
