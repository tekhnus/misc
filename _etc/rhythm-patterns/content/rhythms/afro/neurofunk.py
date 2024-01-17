#!/usr/bin/env python
from models import *


neurofunk = Rhythm("Нейрофанк",
                   Section("Вступление",
                           Voice("Джембе", """xx_x_x_xx_x_x___
                                              xx_x_x_xx_x_x___
                                              xx_bb_xx_bb_x_b_
                                              xx_bb_xx_x_xx___""")),
                   Section("Ритм",
                           Voice("Джембе А", "b__bx_b___b_x___"),
                           Voice("Джембе Б", "b___x__b_b_bx_xx")))


if __name__ == "__main__":
    print(neurofunk.to_html())
