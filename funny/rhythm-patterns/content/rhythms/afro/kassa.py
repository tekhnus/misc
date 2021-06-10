#!/usr/bin/env python
from models import *


djagbe = Rhythm("Касса",
                Section("Брейк",
                        Voice("Кенкени",  """b_xx_b_xx_b_xx__
                                             b_x_b_x_b_x_b___
                                             __________bbb___
                                             ________bbbbb___
                                             bxxbxxbxxbxxb_b_
                                             b___x___xxx_b___
                                             !_!!_!_!!_!_!!__
                                             !_!!_!_!!_!!!___""")))


if __name__ == "__main__":
    print(djagbe.to_html())
