# When writer enters, no other readers can enter until all writers leave.
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


reader_lightswitch = Lightswitch()
writer_lock = Semaphore(value=1)
writer_lightswitch = Lightswitch()
reader_lock = Semaphore(value=1)


def reader_worker():
    reader_lock.acquire()
    reader_lock.release()
    reader_lightswitch.acquire_the(writer_lock)
    # Reading...
    reader_lightswitch.release_the(writer_lock)


def writer_worker():
    writer_lightswitch.acquire_the(reader_lock)
    writer_lock.acquire()
    # Writing...
    writer_lock.release()
    writer_lightswitch.release_the(reader_lock)
