def top_level():
    
    def do_nothing(n):        
        if n <= 0:
            return 0
        return 1 + do_nothing(n-1)
    
    def run_recursion_stack_sice():
        result = do_nothing(10000)
        return result
    
    return run_recursion_stack_sice()