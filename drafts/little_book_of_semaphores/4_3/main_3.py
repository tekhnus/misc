from threading import Semaphore


class StrongMutex:
    def __init__(self):
        self._entrance = Semaphore(value=1)
        self._more_allowed = 5

    def acquire(self):
        self._entrance.acquire()
        self._entrance.release()

        self._more_allowed -= 1
        if not self._more_allowed:
            self._entrance.acquire()
            

    def release(self):
        pass
