#!/usr/bin/env python
from models import *


fanga = Rhythm("Фанга",
               Section("Вступление",
                       Voice("Сигнал",   "xxxx_xx_x_x_xx__"),
                       Voice("Джембе",   "________x_x_xx__")),
               Section("Проигрыш А",
                       Voice("Сигнал",   "xxxx_xx_x_x_x___"),
                       Voice("Джембе",   "________x_x_x___"),
                       Voice("Сигнал",   "xxxx_xx_x_x_xx__"),
                       Voice("Джембе",   "________x_x_xx__"),
                       Voice("Сигнал",   "xxxx_xx_x_x_x___"),
                       Voice("Джембе",   "________x_x_x___")),
               Section("Проигрыш Б",
                       Voice("Джембе", """b__x_xx_b_b_xx__
                                          b__x_xx_x_x_x___
                                          b__x_xx_b_b_xx__
                                          b__x_xx_x_x_x___""")),
               Section("Проигрыш В",
                       Voice("Джембе", """xxxx_xx_x_x_xx__
                                          xxxx_xx_x_x_x___
                                          xxxx_xx_x_x_xx__
                                          xxxx_xx_x_x_x___""")),
               Section("Ритм",
                       Voice("Джембе А", "b__._.._b_b_xx__"),
                       Voice("Джембе Б", "b_bb_b..b__bb_xx")),
               Section("Брейк",
                       Voice("Сигнал",   "xxxx_xx_x_x_xx__"),
                       Voice("Джембе А", "b__._.._x_x_xx__"),
                       Voice("Джембе Б", "b_bb_b..x_x_xx__")),
               Section("Проигрыш А",
                       Voice("Сигнал", """xxxx_xx_x_x_x___
                                          xxxx_xx_x_x_xx__
                                          xxxx_xx_x_x_x___"""),
                       Voice("Джембе", """________x_x_x___
                                          ________x_x_xx__
                                          ________x_x_x___""")),
               Section("Ритм",
                       Voice("Джембе А", "b__._.._b_b_xx__"),
                       Voice("Джембе Б", "b_bb_b..b__bb_xx")),
               Section("Брейк",
                       Voice("Сигнал",   "xxxx_xx_x_x_xx__"),
                       Voice("Джембе А", "b__._.._x_x_xx__"),
                       Voice("Джембе Б", "b_bb_b..x_x_xx__")),
               Section("Проигрыш А",
                       Voice("Сигнал", """xxxx_xx_x_x_x___
                                          xxxx_xx_x_x_xx__
                                          xxxx_xx_x_x_x___"""),
                       Voice("Джембе", """________x_x_x___
                                          ________x_x_xx__
                                          ________x_x_x___""")),
               Section("Проигрыш Б",
                       Voice("Джембе", """b__x_xx_b_b_xx__
                                          b__x_xx_x_x_x___
                                          b__x_xx_b_b_xx__
                                          b__x_xx_x_x_x___""")),
               Section("Проигрыш В",
                       Voice("Джембе", """xxxx_xx_x_x_xx__
                                          xxxx_xx_x_x_x___
                                          xxxx_xx_x_x_xx__
                                          xxxx_xx_x_x_x___""")))


if __name__ == "__main__":
    print(fanga.to_html())
