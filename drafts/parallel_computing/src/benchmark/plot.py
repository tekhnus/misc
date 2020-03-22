#!/usr/bin/python

import sys
import matplotlib.pyplot as plt

inp = sys.stdin.read()
data = eval(inp)

numthreads = [d["numthreads"] for d in data] 
single = numthreads.index(1)
keys = data[single]["timings"].keys()
single_times = data[0]["timings"]
for k in keys:
    times = [d["timings"][k] for d in data]
    single_time = single_times[k]
    speedups = [single_time / time for time in times]
    plt.plot(numthreads, speedups, "o-", label=k)
plt.plot(numthreads, numthreads, label="y=x")
plt.legend()
plt.xticks(numthreads)
plt.yticks(range(min(numthreads), max(numthreads) + 1))
plt.grid()
plt.show()

