#!/usr/bin/env python
# Usage: examples/logistic_regression examples.db:track_hello
from sys import argv
from itertools import groupby, islice
from operator import itemgetter
from random import Random

import matplotlib
matplotlib.use("pdf")
import matplotlib.pyplot as plt

import twitterology as tw
import twitterology.features as tf
from model import MODEL, FULL_MODEL

import numpy as np

from sklearn.linear_model import LogisticRegression
from sklearn.metrics import roc_curve, auc
from sklearn.model_selection import cross_val_score, StratifiedKFold

from tqdm import tqdm


def main():
    database, table = argv[1].split(":")
    storage = tw.sources.sqlite(database, table)

    user_count = sum(1 for _ in storage.distinct("user__id_str"))

    tweets = storage.find(order_by="user__id_str")
    timelines = groupby(tweets, itemgetter("user__id_str"))

    samples_a = []
    samples_b = []
    samples_f = []

    for user_id, timeline in tqdm(timelines, total=user_count):
        timeline = list(timeline)

        features_a = MODEL.features.features(timeline[::2])
        features_b = MODEL.features.features(timeline[1::2])
        features_f = FULL_MODEL.features.features(timeline)

        samples_a.append([user_id, features_a])
        samples_b.append([user_id, features_b])
        samples_f.append([user_id, features_f])

    samples_a = np.array(samples_a)
    samples_b = np.array(samples_b)
    samples_f = np.array(samples_f)

    np.save("db/{}/samples_a.npy".format(table), samples_a)
    np.save("db/{}/samples_b.npy".format(table), samples_b)
    np.save("db/{}/samples_f.npy".format(table), samples_f)


if __name__ == "__main__":
    main()
