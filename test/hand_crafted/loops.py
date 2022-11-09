def top_level():
    """Tests for loops
    """
    
    def run_for_loop(obj):
        obj_elems = []
        for elem in obj:
            obj_elems += [elem]
        return obj_elems

    def test_while_loop():
        x = 0
        while x < 10:
            x += 1
        return x 
    
    def test_list_for_loop():
        l = [0,1,2,3,4,5,6,7,8,9]
        return run_for_loop(l)
    
    def test_tuple_for_loop():
        t = (0,1,2,3,4,5,6,7,8,9)
        return run_for_loop(t)
    
    def test_string_for_loop():
        s = "0123456789"
        return run_for_loop(s)        
    
    def test_dict_for_loop():
        d = {1:2, 3:4, 5:6, 7:8, 9:10}
        return run_for_loop(d)
    
    result = [
        test_while_loop(),
        test_list_for_loop(),
        test_tuple_for_loop(),
        test_string_for_loop(),
        test_dict_for_loop()
    ]
    
    return result
