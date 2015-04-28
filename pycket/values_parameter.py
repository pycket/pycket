
from pycket       import values
from pycket.base  import W_Object
from pycket.cont  import continuation, BaseCont
from pycket.error import SchemeException
from rpython.rlib import jit

# This is a Scheme_Parameterization in Racket
class RootParameterization(object):
    def __init__(self):
        # This table maps ParamKey -> W_ThreadCell
        self.table = {}

# This is a Scheme_Config in Racket
# Except that Scheme_Config uses a functional hash table and this uses a list that we copy
class W_Parameterization(W_Object):
    _immutable_fields_ = ["root", "keys", "vals"]
    errorname = "parameterization"
    def __init__(self, root, keys, vals):
        #assert len(params) == len(vals)
        self.keys = keys
        self.vals = vals
        self.root = root

    @jit.unroll_safe
    def extend(self, params, vals):
        # why doesn't it like this assert?
        # assert len(params) == len(vals)
        # FIXME this is awful
        total = len(params) + len(self.keys)
        keys = [p.key for p in params]
        new_keys = [None] * total
        new_vals = [None] * total
        for i in range(total):
            if i < len(params):
                new_keys[i] = keys[i]
                new_vals[i] = values.W_ThreadCell(vals[i], True)
            else:
                new_keys[i] = self.keys[i-len(params)]
                new_vals[i] = self.vals[i-len(params)]

        return W_Parameterization(self.root, new_keys, new_vals)

    @jit.unroll_safe
    def get(self, param):
        k = param.key
        for (i, key) in enumerate(self.keys):
            if key is k:
                return self.vals[i]
        val = self.root.table[k]
        assert val
        return val

    def tostring(self):
        return "#<parameterization>"

# This will need to be thread-specific
top_level_config = W_Parameterization(RootParameterization(), [], [])

def find_param_cell(cont, param):
    assert isinstance(cont, BaseCont)
    p = cont.get_mark_first(values.parameterization_key)
    assert isinstance(p, W_Parameterization)
    assert isinstance(param, W_Parameter)
    v = p.get(param)
    assert isinstance(v, values.W_ThreadCell)
    return v

@continuation
def param_set_cont(cell, env, cont, vals):
    from pycket.interpreter import check_one_val, return_value
    v = check_one_val(vals)
    cell.set(v)
    return return_value(values.w_void, env, cont)

# a token
class ParamKey(object):
    pass

class W_Parameter(W_Object):
    errorname = "parameter"
    _immutable_fields_ = ["guard", "key"]
    def __init__(self, val, guard=None):
        self.key = ParamKey()
        if guard is values.w_false:
            self.guard = None
        else:
            self.guard = guard
        cell = values.W_ThreadCell(val, True)
        top_level_config.root.table[self.key] = cell

    def iscallable(self):
        return True

    def get(self, cont):
        return self.get_cell(cont).get()

    def get_cell(self, cont):
        cell = find_param_cell(cont, self)
        assert isinstance(cell, values.W_ThreadCell)
        return cell

    def call(self, args, env, cont):
        from pycket.interpreter import return_value
        if len(args) == 0:
            return return_value(self.get(cont), env, cont)
        elif len(args) == 1:
            cell = find_param_cell(cont, self)
            assert isinstance(cell, values.W_ThreadCell)
            if self.guard:
                return self.guard.call([args[0]], env, param_set_cont(cell, env, cont))
            else:
                cell.set(args[0])
                return return_value(values.w_void, env, cont)
        else:
            raise SchemeException("wrong number of arguments to parameter")

    def tostring(self):
        return "#<parameter-procedure>"

