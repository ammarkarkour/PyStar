def top_level():
    """Test cases for Unary operations
    """
    
    def test_UNARY_POSITIVE():
        x1 = 0
        x2 = [1]
        x3 = (1)
        x4 = ""
        x1 += 1
        x2 += [2]
        x3 += (2)
        x4 += "1"
        return x1, x2, x3, x4

    def test_UNARY_NEGATIVE():
        x = 2
        x -= 1
        return x

    def test_UNARY_NOT():
        x11 = not 0
        x12 = not 1
        x21 = not ""
        x22 = not "1"
        x31 = not True
        x32 = not False
        x41 = not []
        x42 = not [1]
        x51 = not ()
        x52 = not (1)
        x61 = not {}
        x62 = not {1:2}
        x7  = not None
        return x11, x12, x21, x22, x31, x32, x41, x42, x51, x52, x61, x62, x7 


    result = [
        test_UNARY_POSITIVE(),
        test_UNARY_NEGATIVE(),
        test_UNARY_NOT()
    ]
    
    return result