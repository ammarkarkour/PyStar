import os
import sys
import time
import subprocess
from config import Config
from translator import Translator

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
    t = Translator(code_obj)
    
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
    # print(result.stdout.decode("utf-8") )
    assert(result.returncode == 0)
         
    # compare states
    state_list = result.stdout.decode("utf-8").split("---\n")
    print(state_list)