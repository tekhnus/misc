#!/usr/bin/env python
from models import *


lolo = Rhythm("Лоло",
              Section("Вступление",
                      Voice("Сигнал", """xx_x_x_xx_x_x__x
                                         x_xx___xx_x_x__x
                                         x_xx___xx_x_x___"""),
                      Voice("Джембе", """_______________x
                                         x_xx___xx_x_x__x
                                         x_xx___xx_x_x___""")),
              Section("Проигрыш",
                      Voice("Джембе", """x_bb_x_bb_x_b___
                                         x_bb_x_bb_x_b___
                                         x_bb_x_bb_x_bb_x
                                         _b_x_bb_x_b_x___""")),
              Section("Ритм",
                      Voice("Джембе",   "b_xx__x___xx__x_")),
              Section("Брейк",
                      Voice("Сигнал", """xxxxxxxxxxxxxxxx
                                         xxxxxxxxxxxxxxxx
                                         xxxxxxxxxxxxx___
                                         x_xx_x_xx_x_x___"""),
                      Voice("Джембе", """b_xx__x___xx__x_
                                         b_xx__x___xx__x_
                                         b_xx__x___xx__x_
                                         b_xx__x___xx____""")),
              Section("Проигрыш",
                      Voice("Джембе", """x_bb_x_bb_x_b___
                                         x_bb_x_bb_x_b___
                                         x_bb_x_bb_x_bb_x
                                         _b_x_bb_x_b_x___""")),
              Section("Соло",
                      Voice("Джембе", """________________
                                         xx__x___x___x___  
                                         xx__x___x_xxxxx_
                                         x_xx_x_xx_x_x___
                                         ________________
                                         xx__x___x___x___
                                         xx__x___x_xxxxx_
                                         x_xx_x_xx_x_x___
                                         ________________
                                         wwxxx___wwxxx___
                                         x___x___x_xxxxx_
                                         x_xx_x_xx_x_x___
                                         ________________
                                         wwxxx___wwxxx___
                                         x___x___x_xxxxx_
                                         x_xx_x_xx_x_x___
                                         """)),
              Section("Проигрыш",
                      Voice("Джембе", """x_bb_x_bb_x_b___
                                         x_bb_x_bb_x_b___
                                         x_bb_x_bb_x_bb_x
                                         _b_x_bb_x_b_x___""")),
              Section("Ритм",
                      Voice("Джембе",   "b_xx__x___xx__x_")),
              Section("Брейк",
                      Voice("Сигнал", """xxxxxxxxxxxxxxxx
                                         xxxxxxxxxxxxxxxx
                                         xxxxxxxxxxxxx___
                                         x_xx_x_xx_x_x___"""),
                      Voice("Джембе", """b_xx__x___xx__x_
                                         b_xx__x___xx__x_
                                         b_xx__x___xx__x_
                                         b_xx__x___xx____""")),
              Section("Проигрыш",
                      Voice("Джембе", """x_bb_x_bb_x_b___
                                         x_bb_x_bb_x_b___
                                         x_bb_x_bb_x_bb_x
                                         _b_x_bb_x_b_x___""")))


if __name__ == "__main__":
    print(lolo.to_html())
