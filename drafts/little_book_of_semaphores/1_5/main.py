from threading import Thread


count = 0


def worker():
    global count
    for _ in range(100000):
        count += 1


threads = [Thread(target=worker)
           for _ in range(100)]

for thread in threads:
    thread.start()

for thread in threads:
    thread.join()

print(count)
