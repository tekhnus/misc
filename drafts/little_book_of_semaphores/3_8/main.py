from threading import Thread, Lock, Semaphore


io = Lock()

leader_token = Semaphore(value=1)
wanted_token = Semaphore(value=0)
thanks_token = Semaphore(value=0)
leader_name = None


def leader_worker(name):
    global leader_name

    leader_token.acquire()
    leader_name = name
    wanted_token.release()
    thanks_token.acquire()
    # dancing here
    with io:
        print('I am', name)
    leader_token.release()


def follower_worker():
    wanted_token.acquire()
    my_leader = leader_name
    thanks_token.release()
    # dancing here
    with io:
        print('Following', my_leader)


threads = []
for n in range(ord('a'), ord('g')):
    threads.append(Thread(target=leader_worker, args=(chr(n),)))
    threads.append(Thread(target=follower_worker))

for thread in threads:
    thread.start()
for thread in threads:
    thread.join()
