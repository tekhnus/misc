room_lock = Semaphore(1)
enrollment = Semaphore(0)
queue = []
client_pleased = Semaphore(0)

waiting_customers = 0


def customer_worker():
    room_lock.acquire()
    if waiting_customers < 7:
        waiting_customers += 1
        next_client = Semaphore(0)
        queue.append(next_client)
        gonna_leave = False
    else:
        gonna_leave = True
    room_lock.release()

    if gonna_leave:
        # Go away.
        return

    # Enroll.
    enrollment.release()
    # Wait for invitation.
    next_client.acquire()
    # Cutting hair...
    # Telling the baber to stop.
    client_pleased.release()
    # Leave the waiting room.
    room_lock.acquire()
    waiting_customers -= 1
    room_lock.release()


def barber_worker():
    while True:
        # Wait for a customer.
        enrollment.acquire()
        # Inspect the queue.
        room_lock.acquire()
        next_client = queue.pop(0)
        room_lock.release()
        # Invite him.
        next_client.release()
        # Cutting hair...
        # Waiting for the client.
        client_pleased.acquire()
