def top_level():
    """Tests for calling/defining functions
    """
    
    def test_function_def_with_no_params():
        return 1
    
    def test_function_def_with_params(x,y,z):
        return x, y, z
    
    def test_recursive(x):
        if x <= 0:
            return 0    
        return x + test_recursive(x-1)
        
    def test_function_to_be_called():
        return 1
    
    def test_closure():
        return test_function_to_be_called()
    
    
    result = [
        test_function_def_with_no_params(),
        test_function_def_with_params(1,2,3),
        test_recursive(5),
        test_closure()
    ]
    
    return result