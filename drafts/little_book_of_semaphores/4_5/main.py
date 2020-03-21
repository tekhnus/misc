# This solution is somewhat close to one presented on the book,
# but pusher and smoker responsibilities are united in single worker.
from threading import Semaphore, Lock, Thread


io = Lock()

agent = Semaphore(1)
tobacco = Semaphore(0)
paper = Semaphore(0)
match = Semaphore(0)


def agent_a():
    while True:
        agent.acquire()
        with io:
            print('agent_a')
        tobacco.release()
        paper.release()


def agent_b():
    while True:
        agent.acquire()
        with io:
            print('agent_b')
        paper.release()
        match.release()


def agent_c():
    while True:
        agent.acquire()
        with io:
            print('agent_c')
        tobacco.release()
        match.release()


lock = Semaphore(1)
available = set()
turnstile = Semaphore(0)


def smoker_a():
    while True:
        tobacco.acquire()
        lock.acquire()
        available.add('tobacco')
        if len(available) == 2:
            turnstile.release()
            turnstile.release()
        lock.release()

        turnstile.acquire()
        if available == {'tobacco', 'paper'}:
            with io:
                print('smoker_a')
            available.clear()
            agent.release()


def smoker_b():
    while True:
        paper.acquire()

        lock.acquire()
        available.add('paper')
        if len(available) == 2:
            turnstile.release()
            turnstile.release()
        lock.release()

        turnstile.acquire()
        if available == {'paper', 'match'}:
            with io:
                print('smoker_b')
            available.clear()
            agent.release()


def smoker_c():
    while True:
        match.acquire()

        lock.acquire()
        available.add('match')
        if len(available) == 2:
            turnstile.release()
            turnstile.release()
        lock.release()

        turnstile.acquire()
        if available == {'tobacco', 'match'}:
            with io:
                print('smoker_c')
            available.clear()
            agent.release()


threads = [Thread(target=f) for f in [agent_a, agent_b, agent_c, smoker_a, smoker_b, smoker_c]]
for thread in threads:
    thread.start()
for thread in threads:
    thread.join()
