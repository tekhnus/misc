# This seems to prevent deadlock, but the performance is slow,
# as if the neightbour of eating man awakes, it locks and prevents
# the others from eating.
from threading import Semaphore, Thread, Lock
from time import sleep


count = 5

io = Lock()
forks = [Semaphore(1) for _ in range(count)]
fork_lock = Semaphore(1)


def philosopher_worker(philosopher_id):
    for times in range(100):
        with io:
            print(philosopher_id, times)
        #
        left_fork_id = philosopher_id
        right_fork_id = (philosopher_id + 1) % count
        # Think.
        fork_lock.acquire()
        forks[left_fork_id].acquire()
        sleep(0.01)
        forks[right_fork_id].acquire()
        fork_lock.release()
        # Eat.
        forks[left_fork_id].release()
        forks[right_fork_id].release()


threads = [Thread(target=philosopher_worker, args=(n,)) for n in range(count)]
for thread in threads:
    thread.start()
for thread in threads:
    thread.join()
