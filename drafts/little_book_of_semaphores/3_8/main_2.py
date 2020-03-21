from threading import Thread, Semaphore, Lock


class BlockingQueue:
    def __init__(self):
        self._sender_busy = Lock()
        self._message_ready = Semaphore()
        self._message_received = Semaphore()
        self._message = None

    def send(self, message):
        with self._sender_busy:
            self._message = message
            self._message_ready.release()
            self._message_received.acquire()

    def receive(self):
        self._message_ready.acquire()
        message = self._message
        self._message_received.release()
        return message


leaders = BlockingQueue()
io = Lock()


def leader_worker(name):
    leaders.send(name)


def follower_worker():
    my_leader = leaders.receive()
    with io:
        print('my leader is', my_leader)


threads = []
for n in range(ord('a'), ord('g')):
    threads.append(Thread(target=leader_worker, args=(chr(n),)))
    threads.append(Thread(target=follower_worker))

for thread in threads:
    thread.start()
for thread in threads:
    thread.join()
