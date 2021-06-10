#!/usr/bin/env python
from models import *


djagbe = Rhythm("Джагбе",
                Section("Вступление",
                        Voice("Сигнал",    """x_xx_x_xx_xxx__b
                                              x_xx_bx_xx_bx_xx
                                              x_xx__x_x_x_x___"""),
                        Voice("Джембе",    """_______________b
                                              x_xx_bx_xx_bx_xx
                                              x_xx__x_x_x_x___""")),
                Section("Ритм",
                        Voice("Джембе А",    "b_xx_bx_"),
                        Voice("Джембе Б",    "xx_bxxb_"),
                        Voice("Джембе В",    "x__xx_..x_bxx_.."),
                        Voice("Колокольчик", "x_xx_xx_"),
                        Voice("Кенкени",     "x_____x_"),
                        Voice("Сангба",      "._bb______bb__._"),
                        Voice("Дундунба",    "__bb______b_____")))


if __name__ == "__main__":
    print(djagbe.to_html())
