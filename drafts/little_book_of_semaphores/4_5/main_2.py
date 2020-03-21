# This solution is somewhat close to one presented on the book,
# but pusher and smoker responsibilities are united in single worker.
from threading import Semaphore, Lock, Thread


io = Lock()

tobacco = Semaphore(0)
paper = Semaphore(0)
match = Semaphore(0)


def agent_a():
    while True:
        with io:
            print('agent_a')
        tobacco.release()
        paper.release()


def agent_b():
    while True:
        with io:
            print('agent_b')
        paper.release()
        match.release()


def agent_c():
    while True:
        with io:
            print('agent_c')
        tobacco.release()
        match.release()


lock = Semaphore(1)
available = list()

match_complemented = Semaphore(0)
tobacco_complemented = Semaphore(0)
paper_complemented = Semaphore(0)


def pusher_common():
    if 'tobacco' in available and 'paper' in available:
        available.remove('tobacco')
        available.remove('paper')
        match_complemented.release()
    if 'paper' in available and 'match' in available:
        available.remove('paper')
        available.remove('match')
        tobacco_complemented.release()
    if 'tobacco' in available and 'match' in available:
        available.remove('tobacco')
        available.remove('match')
        paper_complemented.release()
    


def pusher_a():
    while True:
        tobacco.acquire()

        lock.acquire()
        available.append('tobacco')
        pusher_common()
        lock.release()


def pusher_b():
    while True:
        paper.acquire()

        lock.acquire()
        available.append('paper')
        pusher_common()
        lock.release()


def pusher_c():
    while True:
        match.acquire()

        lock.acquire()
        available.append('match')
        pusher_common()
        lock.release()


def smoker_a():
    while True:
        tobacco_complemented.acquire()
        with io:
            print('smoker_a')
        

def smoker_b():
    while True:
        paper_complemented.acquire()
        with io:
            print('smoker_b')


def smoker_c():
    while True:
        match_complemented.acquire()
        with io:
            print('smoker_c')


threads = [Thread(target=f) for f in [agent_a, agent_b, agent_c, pusher_a, pusher_b, pusher_c, smoker_a, smoker_b, smoker_c]]
for thread in threads:
    thread.start()
for thread in threads:
    thread.join()
