from threading import Thread, Semaphore


count = 0
sem = Semaphore(value=1)


def worker_a():
    global count
    sem.acquire()
    count += 1
    sem.release()


def worker_b():
    global count
    sem.acquire()
    count += 1
    sem.release()


a = Thread(target=worker_a)
b = Thread(target=worker_b)

a.start()
b.start()

a.join()
b.join()

print(count)
