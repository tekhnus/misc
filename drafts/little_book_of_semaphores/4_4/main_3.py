from threading import Semaphore, Thread, Lock
from time import sleep


count = 5

io = Lock()
states = ['thinking'] * count
lock = Semaphore(1)
permission_signal = [Semaphore(0) for _ in range(count)]


def update_permissions(philosopher_id):
    left_philosopher_id = (philosopher_id - 1) % 5
    right_philosopher_id = (philosopher_id + 1) % 5

    if states[philosopher_id] == 'hungry' and \
       states[left_philosopher_id] == 'thinking' and \
       states[right_philosopher_id] == 'thinking':
        states[philosopher_id] = 'eating'
        permission_signal[philosopher_id].release()


def philosopher_worker(philosopher_id):
    for times in range(100):
        # Thinking.
        with io:
            print(philosopher_id, times)
        left_fork_id = philosopher_id
        right_fork_id = (philosopher_id + 1) % count

        left_philosopher_id = (philosopher_id - 1) % count
        right_philosopher_id = (philosopher_id + 1) % count
        # Declaring himself hungry.
        lock.acquire()
        states[philosopher_id] = 'hungry'
        update_permissions(philosopher_id)
        lock.release()
        # Waiting for forks.
        permission_signal[philosopher_id].acquire()
        # Eating.
        # ...
        # Releasing forks.
        lock.acquire()
        states[philosopher_id] = 'thinking'
        update_permissions(left_philosopher_id)
        update_permissions(right_philosopher_id)
        lock.release()


threads = [Thread(target=philosopher_worker, args=(n,)) for n in range(count)]
for thread in threads:
    thread.start()
for thread in threads:
    thread.join()
