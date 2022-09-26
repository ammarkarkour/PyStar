import dis
from types import CodeType


class Translator:
    """ 
        This class translates Python source code into compiled fstar codeObj
        that is ready to be executed by py*. 
    """
    def __init__(self, code_obj) -> None:
        self.code_obj = code_obj
        self.co_id = code_obj.__str__().split()[4][:-1]
        self.instructions = dis.get_instructions(code_obj)

    def  get_fstar_bytecode(self) -> str:
        """ Gets ready to print bytecode instructions of self.code_obj
        """
        bytecode = f'let bc_{self.co_id} = CODE [\n'
        for instr in self.instructions:
            opname = instr.opname
            arg = instr.arg if instr.arg != None else ""
            bytecode += f'  {opname} {arg};\n'
        return (bytecode+"]\n")
    
    def get_fstar_names(self) -> str:
        """ Gets ready to print code object names of self.code_obj
        """
        names = f'let names_{self.co_id} = [\n'
        for n in self.code_obj.co_names:
            names += f'  \"{n}\";\n'
        return (names+"]\n")    
    
    def get_fstar_varnames(self) -> str:
        """ Gets ready to print code object varnames of self.code_obj
        """
        varnames = f'let varnames_{self.co_id} = [\n'
        for v in self.code_obj.co_varnames:
            varnames += f'  \"{v}\";\n'
        return (varnames+"]\n")  

    def add_cls_cons(self, x) -> str:
        """ Wraps a python object by its py* object constructor
        """
        if x == None:
            return 'createNone()'
        elif isinstance(x, int):
            return f'createInt {x}'
        elif isinstance(x, str):
            return f'createString "{x}"'
        elif isinstance(x, bool):
            return f'createBool {x}'
        elif isinstance(x, list):
            l = map(lambda y: f'{self.add_cls_cons(y)}; ', x) 
            return f'createList([{l}])'
        elif isinstance(x, tuple):
            t = map(lambda y: f'{self.add_cls_cons(y)}; ', x) 
            return f'createTuple([{t}])'
        elif isinstance(x, dict):
            keys = list(x.keys())
            values = list(x.values())
            zipped_keys_values = list(zip(keys, values))
            return self.add_cls_cons(zipped_keys_values)
    
    def get_fstar_consts(self) -> str:
        """ Gets ready to print fstar contansts of self.code_obj
        """
        consts = f'let consts_{self.co_id} = [\n'
        for c in self.code_obj.co_consts:
            if isinstance(c, CodeType):
                c_child = Translator(c)
                consts = f'{c_child.get_fstar_codeObj()}\n{consts}'
                consts += f'  CODEOBJECT(co_{c_child.co_id});\n'
            else:
                consts += f'  PYTYP({self.add_cls_cons(c)});\n'
        return (consts+"]\n")
    
    def get_fstar_codeObj(self) -> str:
        """  Gets ready to print fstar code object from self.code_obj
        """
        bytecode = self.get_fstar_bytecode()
        consts = self.get_fstar_consts()
        varnames = self.get_fstar_varnames()
        names = self.get_fstar_names()
        fstar_codeObj = (
            f'let co_{self.co_id} = '
            '{\n'
            f'  co_code = bc_{self.co_id};\n'
            f'  co_consts = consts_{self.co_id};\n'
            f'  co_varnames = varnames_{self.co_id};\n'
            f'  co_names = names_{self.co_id}\n'
            '}\n'
        )
        return f'{bytecode}\n{consts}\n{varnames}\n{names}\n{fstar_codeObj}'
                
    def write_fstar_code_to_file(self, filename):
        """Writes the code to filename
        """
        headers = (
            f'module {filename}\n\n'
            '(* imported modules *)\n'
            'open Structs\n'
            'open VM\n'
            'open Utils\n'
            'open PyBuiltinObjects\n'
            '(* ---------------- *)\n'
        )
        codeObj = self.get_fstar_codeObj()
        run_code = f'let res = runCode co_{self.co_id}'
        fstar_code = f'{headers}\n{codeObj}\n{run_code}'
        
        # Write the code to file
        f = open(f'{filename}.fst', 'w')
        done = f.write(fstar_code)
        f.close()
        
        return done