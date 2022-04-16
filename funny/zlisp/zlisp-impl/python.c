def test():
    x = 3
    def myclosure():
        yield y
    c = myclosure()
    y = 5
    print(next(c))
test()

