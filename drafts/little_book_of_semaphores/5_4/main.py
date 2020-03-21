# TODO: check this solution!
from threading import Semaphore, Thread, Lock
from collections import deque
io = Lock()


hey_barber = Semaphore(0)

room = Semaphore(1)
standing = deque()
sitting = deque()
serviced = 0

cashdesk = Semaphore(1)
paying = 0

satisfied = Semaphore(0)
done = Semaphore(0)


def customer_worker():
    global serviced, paying

    you_next = Semaphore(0)
    with room:
        if len(sitting) + len(standing) + serviced >= 20:
            return
        elif len(sitting) >= 4:
            standing.append(you_next)
        else:
            sitting.append(you_next)
            hey_barber.release()

    you_next.acquire()
    # Being cut...
    satisfied.release()

    with room:
        serviced -= 1
        paying += 1

    with cashdesk:
        hey_barber.release()
        # Paying...
        done.acquire()


def barber_worker():
    global serviced, paying

    while True:
        hey_barber.acquire()

        with room:
            with io: print('awoken', ';', 'standing:', len(standing), ';', 'sitting:', len(sitting), ';', end=' ')

            if paying:
                with io: print('accepting payment')

                # Accepting payment...
                paying -= 1
                done.release()

                continue
            else:
                with io: print('doing haircut')

                you_next = sitting.popleft()
                serviced += 1

                if standing:
                    sitting.append(standing.popleft())
                    hey_barber.release()

                you_next.release()
        # Cutting...
        satisfied.acquire()


threads = (
    [Thread(target=customer_worker) for _ in range(200)] +
    [Thread(target=barber_worker) for _ in range(3)]
)
for thread in threads:
    thread.start()
for thread in threads:
    thread.join()
