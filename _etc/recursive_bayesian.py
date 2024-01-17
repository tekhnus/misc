def update(state, z, observation):
    new_state = {}
    s = 0
    for x, p in state.items():
        new_state[x] = p * observation.given(current_state=x)(z)
        s += new_state[x]
    for x, p in new_state.items():
        new_state[x] = p / s
    return DiscreteDistribution(new_state)


def predict(state, next_state):
    new_state = {
        y: sum(p * next_state.given(current_state=x)(y) for x, p in state.items())
        for y in state.keys()
    }
    return DiscreteDistribution(new_state)
