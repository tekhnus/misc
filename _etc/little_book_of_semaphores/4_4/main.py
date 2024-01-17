from threading import Semaphore, Thread, Lock
from time import sleep


count = 5

io = Lock()
forks = [Semaphore(1) for _ in range(count)]


def philosopher_worker(philosopher_id):
    for times in range(100):
        with io:
            print(philosopher_id, times)
        #
        left_fork_id = philosopher_id
        right_fork_id = (philosopher_id + 1) % count
        if philosopher_id == 0:
            left_fork_id, right_fork_id = right_fork_id, left_fork_id
        # Think.
        forks[left_fork_id].acquire()
        sleep(0.01)
        forks[right_fork_id].acquire()
        # Eat.
        forks[left_fork_id].release()
        forks[right_fork_id].release()


threads = [Thread(target=philosopher_worker, args=(n,)) for n in range(count)]
for thread in threads:
    thread.start()
for thread in threads:
    thread.join()
