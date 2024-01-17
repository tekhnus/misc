import re
import random
import itertools
import collections


def ngrams(word, n):
    for i in range(len(word) - n + 1):
        yield word[i:i+n]


def main():
    n = 2
    top = 50

    tc = collections.Counter()
    with open("dict.opcorpora.txt") as f:
        while True:
            try:
                h = next(f)
            except StopIteration:
                break
            word, props = next(f).split(maxsplit=1)
            if "NOUN" in props and "Name" not in props and "Patr" not in props and "Surn" not in props:
                # print(word)
                tc.update(ngrams(word, n))
            for line in f:
                line = line.rstrip()
                if not line:
                    break
    top = [w for w, _ in tc.most_common(top)]
    words = random.sample(top, 5)
    for w in words:
        print(w)

    input("Press ENTER to see stats")

    triads = list(itertools.combinations(words, 4))
    tric = collections.Counter()
    with open("dict.opcorpora.txt") as f:
        while True:
            try:
                h = next(f)
            except StopIteration:
                break
            word, props = next(f).split(maxsplit=1)
            if "NOUN" in props and "Name" not in props and "Patr" not in props and "Surn" not in props:
                for tri in triads:
                    if all(seq in word for seq in tri):
                        print(word)
                        tric[tri] += 1
            for line in f:
                line = line.rstrip()
                if not line:
                    break
    for t, cnt in tric.items():
        print(t, cnt)


if __name__ == "__main__":
    main()
