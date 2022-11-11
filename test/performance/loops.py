def top_level():
    """Testing running time for while loop 
    """
    
    def test_while_loop(n):
        while n > 0:
            n -= 1
                
    return test_while_loop(1000000)