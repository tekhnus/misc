from threading import Semaphore, Lock, Thread
from time import sleep
from random import random
io = Lock()


free_cashdesk = Semaphore(1)
give_more = Semaphore(0)
done = Semaphore(0)
servings = 20


def savage_worker(savage_id):
    global servings

    while True:
        # Get serving.
        free_cashdesk.acquire()
        if not servings:
            give_more.release()
            done.acquire()
        servings -= 1
        free_cashdesk.release()
        # Eat.
        with io:
            print(savage_id, 'eats')
        sleep(random())


def cook_worker():
    global servings

    while True:
        # Serve.
        give_more.acquire()
        with io:
            print('The cook serves')
        servings += 20
        done.release()

threads = (
    [Thread(target=cook_worker)] +
    [Thread(target=savage_worker, args=(savage_id,)) for savage_id in range(20)]
)
for thread in threads:
    thread.start()
for thread in threads:
    thread.join()
