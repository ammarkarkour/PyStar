def top_level():
    """Test cases for Binary operations
    """
    
    def test_BINARY_MULTIPLY():
        x1 = 2 * 3
        x2 = True * False
        x3 = 2 * True
        x4 = 2 * "1"
        x5 = "1" * 2
        x6 = [1] * 2
        x7 = 2 * [1]
        x8 = (1) * 2
        x9 = 2 * (1)
        x10 = (1) * "same"
        return x1, x2, x3, x4, x5, x6, x7, x8, x9, x10

    def test_BINARY_FLOOR_DIVIDE():
        x1 = 10 // 2
        x2 = 2 // 10
        x3 = 0 // 10
        return x1, x2, x3
    
    def test_BINARY_MODULO():
        x1 = 10 % 3
        x2 = 2 % 10
        x3 = 0 % 10
        return x1, x2, x3
    
    def test_BINARY_ADD():
        x1 = 0 + 1
        x2 = [1] + [2]
        x3 = (1,2) + (3,4)
        x4 = "1" + "2"
        x5 = (1) + 3
        return x1, x2, x3, x4, x5
    
    def test_BINARY_SUBTRACT():
        x1 = 9 - 1
        x2 = 2 - 3
        return x1, x2
    
    def test_BINARY_SUBSCR():
        x1 = [1,2,3][1]
        x2 = (1,2,3)[1]
        x3 = "123"[1]
        x4 = {1:2}[1]
        return x1, x2, x3, x4     
    
    def test_BINARY_SUBSCR_SLICES():
        l2 = [1,2,3,4,5,6,7,8,9]
        x1 = l2[:]
        x2 = l2[::-1]
        x3 = l2[::2]
        x4 = l2[::-2]
        x5 = l2[0:15:]
        x6 = l2[-10::-1]
        x7 = l2[5:1:]
        x8 = l2[1:5:-1]
        
        return x1, x2, x3, x4, x5, x6, x7, x8
           
    
    result = [
        test_BINARY_MULTIPLY(),
        test_BINARY_FLOOR_DIVIDE(),
        test_BINARY_MODULO(),
        test_BINARY_ADD(),
        test_BINARY_SUBTRACT(),
        test_BINARY_SUBSCR(),
        test_BINARY_SUBSCR_SLICES(),
    ]
    
    return result