from threading import Thread, Semaphore


class Lightswitch:
    def __init__(self):
        self._inside = 0
        self._lock = Semaphore(value=1)
        self._empty = Semaphore(value=1)

    def acquire_the(self, semaphore):
        self._lock.acquire()
        if self._inside == 0:
            semaphore.acquire()
        self._inside += 1
        self._lock.release()

    def release_the(self, semaphore):
        self._lock.acquire()
        self._inside -= 1
        if self._inside == 0:
            semaphore.release()
        self._lock.release()


lightswitch = Lightswitch()
writer_lock = Semaphore(value=1)


def reader_worker():
    lightswitch.acquire_the(writer_lock)
    # Reading...
    lightswitch.release_the(writer_lock)


def writer_worker():
    writer_lock.acquire()
    # Writing...
    writer_lock.release()
