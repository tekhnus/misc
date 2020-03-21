from threading import Thread, Semaphore, Lock
from time import sleep


io = Lock()

queue = []
lock = Semaphore(value=1)
signal_new_element = Semaphore(value=0)
signal_free_space = Semaphore(value=5)


def producer_worker(producer_id):
    for task_id in 'ABCDEFGHIJKLMN':
        # Produce an element.
        sleep(0.5)
        # Wait till there is free space.
        signal_free_space.acquire()
        # Put the element in the queue.
        lock.acquire()
        queue.append((producer_id, task_id))
        lock.release()
        # Signal about a new element.
        signal_new_element.release()
        # Log.
        with io:
            print('Producer', producer_id, ': put', task_id)


def consumer_worker(consumer_id):
    while True:
        # Wait till there is new item in the queue.
        signal_new_element.acquire()
        # Take the element from the queue.
        lock.acquire()
        task_id = queue.pop(0)
        lock.release()
        # Signal about freed space.
        signal_free_space.release()
        # Process the element.
        with io:
            print('Consumer', consumer_id, ' : started', task_id)
        sleep(3)
        with io:
            print('Consumer', consumer_id, ' : finished', task_id)



threads = ([Thread(target=consumer_worker, args=(consumer_id,))
            for consumer_id in range(3)] +
           [Thread(target=producer_worker, args=(producer_id,))
            for producer_id in range(2)])

for thread in threads:
    thread.start()
for thread in threads:
    thread.join()
