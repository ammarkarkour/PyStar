def top_level():
    def test():
        return 7
    
    def test2():
        return 3
    
    x = test()
    y = test2()
    return (x * y)