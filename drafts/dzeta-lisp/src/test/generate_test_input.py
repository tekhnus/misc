from random import sample

def generate(order):
    if order == 0:
        return 'foo'

    result = '(' + ' '.join(map(generate, sample(range(order), max(order//2, 1)))) + ')'
    return result

with open('test.dz', 'w') as src:
    print(generate(34), file=src)
