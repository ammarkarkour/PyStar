"""
Note: this is wrong as it only tests the opcodes on the top level function.
- We need all opcodes, so we need recursive look for opcodes.
"""



import dis
import json
from glob import glob

def analyse_opcode(files_path, supported_operations):
    """
    Loop through the python files under the files_path directory, and for each 
    file compile it and get its bytecode opcode instructions. Analyse 
    the instructions by building a dictionary that contains all instructions
    that we don't support, and how many files they are in, and how many
    occurencies of it exist across the different files.

    Args:
        files_path (str): Path to the dir that contains the tests files.
        supported_operations (list str): List of supported bytecode operations.
    
    Returns:
        opcode_dict (dict): A dcitionary of the form:
            {
                "opcode": 
                    {
                        "num_files": (n:int),
                        "num_occurrences": (m:int),
                    }
            } 
    """
    python_files = glob(f'{files_path}/*.py')
    missing_operations = {}
    
    for f_path in python_files:
        
        # Read source code 
        f = open(f_path, 'r')
        source_code = f.read()
        f.close()
        
        # Compile code to get the code object, and disassible it
        code_obj = compile(source_code, f_path, 'exec')
        instructions = dis.get_instructions(code_obj.co_consts[0])

        # f's not supported operations
        f_missing_operations = {} 
        
        # Sort instructions
        for instr in instructions:
            opname = instr.opname
            
            if opname not in supported_operations:
                if opname in f_missing_operations:
                    f_missing_operations[opname]['num_occurrences'] += 1
                
                else:
                    if opname in missing_operations:
                        f_missing_operations[opname] = missing_operations[opname]
                        f_missing_operations[opname]['num_files'] += 1
                
                    else:
                        f_missing_operations[opname] = {
                            'num_files': 1,
                            'num_occurrences': 1
                        }
        
        # Add f_missing_operations to missing_operations
        missing_operations.update(f_missing_operations)        

    return missing_operations



"""
# define hyper params
files_path = ''
supported_operations = []

# analyse
result_dict = analyse_opcode(
        '',
        supported_operations
    )

print(json.dumps(result_dict, indent=4, sort_keys=True))
"""