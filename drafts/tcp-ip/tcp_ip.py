import random
import dataclasses


@dataclasses.dataclass
class TCPSegment:
    src_port: int
    dst_port: int
    seq_number: int = 0
    ack_number: int = 0
    ack_flag: bool = False
    syn_flag: bool = False
    fin_flag: bool = False
    data: object = None


def tcp_client(network, src_port, dst_port):
    seq_number = random.randrange(1000)
    our_syn = TCPSegment(src_port=src_port, dst_port=dst_port, seq_number=seq_number, syn_flag=True)
    yield network.send(our_syn)
    seq_number += 1

    their_ack = yield network.recv()
    assert their_ack.ack_flag
    assert their_ack.ack_number == seq_number

    ack_number = their_ack.seq_number + 1
    our_ack = TCPSegment(src_port=src_port, dst_port=dst_port, seq_number=seq_number, ack_number=ack_number, ack_flag=True)
    yield network.send(our_ack)

    message = yield network.recv()
    assert message.ack_flag
    assert message.ack_number == seq_number
    ack_number += len(message.data)

    message_ack = TCPSegment(src_port=src_port, dst_port=dst_port, ack_number=ack_number, ack_flag=True)
    yield network.send(message_ack)

    server_fin = yield network.recv()
    assert server_fin.fin_flag
    assert server_fin.ack_flag
    assert server_fin.ack_number == seq_number
    ack_number += 1

    our_ack_fin = TCPSegment(src_port=src_port, dst_port=dst_port, seq_number=seq_number, ack_number=ack_number, ack_flag=True, fin_flag=True)
    yield network.send(our_ack_fin)
    seq_number += 1

    their_ack = yield network.recv()
    assert their_ack.ack_flag
    assert their_ack.ack_number == seq_number
    

def tcp_server(network, src_port, dst_port):
    their_syn = yield network.recv()
    assert their_syn.syn_flag
    ack_number = their_syn.seq_number + 1

    seq_number = random.randrange(1000)
    our_ack_syn = TCPSegment(src_port=src_port, dst_port=dst_port, seq_number=seq_number, ack_number=ack_number, syn_flag=True, ack_flag=True)
    yield network.send(our_ack_syn)
    seq_number += 1

    their_ack = yield network.recv()
    assert their_ack.ack_flag
    assert their_ack.ack_number == seq_number

    message = TCPSegment(src_port=src_port, dst_port=dst_port, seq_number=seq_number, data="Hello, world!", ack_flag=True, ack_number=ack_number)
    yield network.send(message)
    seq_number += len(message.data)

    their_ack = yield network.recv()
    assert their_ack.ack_flag
    assert their_ack.ack_number == seq_number

    fin = TCPSegment(src_port=src_port, dst_port=dst_port, seq_number=seq_number, ack_number=ack_number, fin_flag=True, ack_flag=True)
    yield network.send(fin)
    seq_number += 1
    
    their_ack_and_fin = yield network.recv()
    assert their_ack_and_fin.ack_flag
    assert their_ack_and_fin.fin_flag
    assert their_ack_and_fin.ack_number == seq_number
    ack_number += 1

    our_ack = TCPSegment(src_port=src_port, dst_port=dst_port, seq_number=seq_number, ack_number=ack_number, ack_flag=True)
    yield network.send(our_ack)


class Channel:
    def __init__(self):
        pass

    def send(self, value):
        return ("send", self, value)

    def recv(self):
        return ("recv", self)


def go(*args):
    ready = {routine: None for routine in args}
    paused = {}

    while ready:
        for routine, value in ready.items():
            try:
                res = routine.send(value)
            except StopIteration:
                pass
            else:
                paused[routine] = res
        ready = {}
        for a, v_a in paused.items():
            for b, v_b in paused.items():
                if a in ready or b in ready:
                    continue
                if v_a[0] == "send" and v_b[0] == "recv" and v_a[1] is v_b[1]:
                    ready[a] = None
                    ready[b] = v_a[2]
        for r in ready:
            del paused[r]
    if paused:
        raise RuntimeError(f"dead lock {paused}")


def main():
    net = Channel()
    cli = tcp_client(net, 1234, 80)
    ser = tcp_server(net, 80, 1234)
    go(cli, ser)


if __name__ == "__main__":
    main()
