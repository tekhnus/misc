#!/usr/bin/env python
from models import *


fankani = Rhythm("Фанкани",
                 Section("Ритм",
                         Voice("Джембе",   "x__xx_b_x_..x_b_")),
                 Section("Брейк",
                         Voice("Джембе", """x__xx_b_x_..x_b_
                                            x__xx_b_x_..x___
                                            w_.._._.._xxx___"""),
                         Voice("Сигнал", """xxxxxxxxxxxxxxxx
                                            xxxxxxxxxxxxx___
                                            w_.._._.._xxx___""")),
                 Section("Соло",
                         Voice("Джембе", """w__.x_x_________
                                            .._xxxx_________
                                            w__.x_x_________
                                            .._x_.x_________
                                            w__.x_x_________
                                            .._xxxx_________
                                            w__.x_x_________
                                            .._x_.x_________
                                            .._xx__b..xx._x_
                                            .._xx__b..xx..x_
                                            .._xx__b..xx._x_
                                            .._xx__b..xx..x_
                                            bbbbxxxxxxxxxxxx
                                            bbbbxxxxxxxxx___
                                            w_.._._.._xxx___""")))


if __name__ == "__main__":
    print(fankani.to_html())
