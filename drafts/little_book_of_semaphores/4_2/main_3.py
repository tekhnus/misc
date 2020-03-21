from threading import Thread, Semaphore


class Room:
    def __init__(self):
        self._inside = 0
        self._lock = Semaphore(value=1)
        self._empty = Semaphore(value=1)

    def enter(self):
        self._lock.acquire()
        if self._inside == 0:
            self._empty.acquire()
        self._inside += 1
        self._lock.release()

    def exit(self):
        self._lock.acquire()
        self._inside -= 1
        if self._inside == 0:
            self._empty.release()
        self._lock.release()

    def enter_alone(self):
        self._empty.acquire()

    def leave_alone(self):
        self._empty.release()


room = Room()


def reader_worker():
    room.enter()
    # Reading...
    room.exit()


def writer_worker():
    room.enter_alone()
    # Writing...
    room.exit_alone()
