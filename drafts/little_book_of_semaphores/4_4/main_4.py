from threading import Semaphore, Thread, Lock
from time import sleep


class LockSystem:
    def __init__(self):
        self._last_lock_id = 0

        self._states = {}
        self._notifications = []

        self._master_lock = Lock()

    def obtain(self):
        self._last_lock_id += 1

        self._states[self._last_lock_id] = 'released'

        return self._last_lock_id

    def acquire(self, *args):
        with self._master_lock:
            already_acquired_locks = []
            for lock_id in args:
                if self._states[lock_id] == 'acquired':
                    already_acquired_locks.append(lock_id)
                self._states[lock_id] = 'acquired'

            if already_acquired_locks:
                last_blocker_released = Semaphore(value=0)
                self._notifications.append((last_blocker_released, already_acquired_locks))
            else:
                last_blocker_released = Semaphore(value=1)

        last_blocker_released.acquire()

    def release(self, lock_id):
        with self._master_lock:
            if self._states[lock_id] == 'released':
                raise RuntimeError('A lock cannot be released twice')
            self._states[lock_id] = 'released'

            for last_blocker_released, already_acquired_locks in self._notifications:
                if lock_id in already_acquired_locks:
                    self._states[lock_id] = 'acquired'
                    already_acquired_locks.remove(lock_id)
                    if not already_acquired_locks:
                        last_blocker_released.release()
                        self._notifications.remove((last_blocker_released, already_acquired_locks))
                    

count = 5
io = Lock()
table = LockSystem()
forks = [table.obtain() for _ in range(5)]


def philosopher_worker(philosopher_id):
    for times in range(100):
        # Thinking.
        with io:
            print(philosopher_id, times)
        left_fork_id = philosopher_id
        right_fork_id = (philosopher_id + 1) % count

        table.acquire(forks[left_fork_id], forks[right_fork_id])
        sleep(0.1)
        table.release(forks[left_fork_id])
        table.release(forks[right_fork_id])


threads = [Thread(target=philosopher_worker, args=(n,)) for n in range(count)]
for thread in threads:
    thread.start()
for thread in threads:
    thread.join()
