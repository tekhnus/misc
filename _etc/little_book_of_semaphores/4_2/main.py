# Mistake!
# Writing permission is locked during reading,
# so there is actually no concurrent reading!
from threading import Thread, Semaphore


read_permission = Semaphore(value=1)
write_permission = Semaphore(value=1)


def reader_worker():
    read_permission.acquire()
    write_permission.acquire()
    read_permission.release()
    # Reading...
    write_permission.release()


def writer_worker():
    write_permission.acquire()
    read_permission.acquire()
    # Writing...
    read_permission.release()
    write_permission.release()



