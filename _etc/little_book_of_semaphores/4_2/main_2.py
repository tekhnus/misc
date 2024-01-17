from threading import Thread, Semaphore


write_permission = Semaphore(value=1)
master_token = Semaphore(value=1)
readers = 0


def reader_worker():
    global readers

    master_token.acquire()
    if readers == 0:
        write_permission.acquire()
    readers += 1
    master_token.release()

    # Reading...

    master_token.acquire()
    readers -= 1
    if readers == 0:
        write_permission.release()
    master_token.release()


def writer_worker():
    write_permission.acquire()

    # Writing...

    write_permission.release()
