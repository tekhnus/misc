from threading import Thread, Lock, Semaphore


io = Lock()
a_started = Semaphore(value=0)
b_started = Semaphore(value=0)


def worker_a():
    with io:
        print("A1")
    a_started.release()
    b_started.acquire()
    with io:
        print("A2")


def worker_b():
    with io:
        print("B1")
    b_started.release()
    a_started.acquire()
    with io:
        print("B2")


a = Thread(target=worker_a)
a.start()
b = Thread(target=worker_b)
b.start()

a.join()
b.join()
