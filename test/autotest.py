import sys
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