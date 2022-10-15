import os
import sys
import ast
import time
import subprocess
from config import Config
from translator import Translator

def parse_state_str(state_str:str):
    """Parses a stdout string into the testing format
    
    'INT:i'        ---> i:int
    'STR:s'        ---> s:str
    'BOOL:b'       ---> b:bool
    'LIST:l'       ---> l:list      (recursive)
    'TUPLE:t'      ---> t:tuple     (recursive)
    'DICT:d'       ---> d:dict      (recursive)
    'FUNCTION:f'   ---> f:str
    'EXCEPTION:e'  ---> e:str
    'USERDEF:'     ---> 'USERDEF'
    'NONE:None'    ---> None
    'ERR:r'        ---> r:str
    'CODEOBJECT:l' ---> l:list      (recursive)
    'FRAME:l'      ---> l:list      (recursive)
    'UNFUNOBJ:'    ---> 'UNFUNOBJ'
    'BINFUNBLT:'   ---> 'BINFUNBLT'
    'UNFUNBLT:'    ---> 'UNFUNBLT'
    'STATE:l'      ---> l:list      (recursive)

    Args:
        state_str (_type_): _description_
    """
    state_list = state_str.split(":")
    typ = state_list[0]
    val = state_str[len(typ)+1:]

    if typ == 'INT':
        return int(val)
    elif typ == 'STR':
        return val
    elif typ == 'BOOL':
        return val == 'True'
    elif typ == 'LIST':
        return list(map(parse_state_str, val.strip('][').split(',')))
    elif typ == 'TUPLE':
        return tuple(map(parse_state_str, val.strip('()').split(',')))        
    elif typ == 'USERDEF':
        return 'USERDEF'
    elif typ == 'NONE':
        return None
    elif typ == 'RESULT':
        return parse_state_str(val)
    elif typ == 'DICT':
        kv_pairs = map(parse_state_str, val.strip('{}').split(','))
        result_dict = {}
        for kv in kv_pairs:
            kv_list = kv.split(":")
            k, v = kv_list[0], kv_list[1]
            result_dict[parse_state_str(k)] = parse_state_str(v)
        return result_dict
    #ERR,EXCEPTION,FUNCTION,CODEEOBJECT,FRAME,UNFUNOBJ,UNFUNBLT,BINFUNBLT,STATE
    else:
        return state_str

if __name__=="__main__":
    args = sys.argv
    if len(args) != 3:
        sys.exit("Wrong Inpuit format: python autotest.py IN_FILE.py OUT_FILE")

    # Read the python file
    input_file = args[1]
    f = open(input_file, 'r')
    source_code = f.read()
    f.close()

    # Compile code to get the code object, and disassible it
    code_obj = compile(source_code, input_file, 'exec')

    # Init Translator
    t = Translator(code_obj.co_consts[0])
    
    # Create fstar file
    output_file = args[2]
    t.write_fstar_code_to_file(output_file)
    
    # translate f* test file into ocaml file (Makefile through terminal)
                             
    correct_exec = os.system('/bin/bash -c "echo switch to bash" &&'
                             f'mv {output_file}.fst ../src/test &&'
                              'cd ../src &&'
                             f'make autotestTranslate NAME={output_file}')
    assert(correct_exec == 0)
    
    # compile ocaml file code into executable (Makefile through terminal)
    correct_exec = os.system(f'cd ../src && make autotestCompile NAME={output_file}')
    assert(correct_exec == 0)

    # run the executable (Measure time to test performance) (Makefile through terminal)
    if Config.PERFROMANCE_TEST:
        t0 = time.time()
    
    result = subprocess.run([f'./{output_file}.exe'], stdout=subprocess.PIPE, 
                            shell=True, cwd='/home/akarkour/pystar/PyStar/src/out')
    assert(result.returncode == 0)
         
    # compare states
    state_str = parse_state_str(result.stdout.decode("utf-8"))
    print(state_str)