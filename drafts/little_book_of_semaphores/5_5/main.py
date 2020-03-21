# TODO: check this solution.
lock = Semaphore(1)

waiting_reindeer = 0
waiting_elves = 0

please_wake_up = Semaphore(0)
sleigh_is_ready = Semaphore(0)
come_in = Semaphore(1)
goodbye = Semaphore(0)


def reindeer_worker():
    with lock:
        waiting_reindeer += 1
        if waiting_reindeer == 9:
            please_wake_up.release()
    sleigh_is_ready.acquire()
    sleigh_is_ready.release()
    # Getting hitched...


def elve_worker():
    come_in.acquire()
    with lock:
        waiting_elves += 1
        if waiting_elves == 3:
            please_wake_up.release()
        else:
            come_in.release()
    # Getting help...
    goodbye.acquire()
    with lock:
        waiting_elves -= 1
        if waiting_elves == 0:
            come_in.release()
        else:
            goodbye.release()
    

def santa_worker():
    while True:
        please_wake_up.receive()
        with lock:
            if waiting_reindeer == 9:
                # Preparing the sleigh...
                waiting_reindeer = 0
                sleigh_is_ready.release()
            if waiting_elves == 3:
                # Helping...
                goodbye.release()
        
                
            
                
                
    
