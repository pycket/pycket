
from pycket                          import values
from pycket.arity                    import Arity
from pycket.base                     import W_Object
from pycket.cont                     import call_cont, continuation, BaseCont
from pycket.error                    import SchemeException
from pycket.hash.persistent_hash_map import make_persistent_hash_type
from rpython.rlib                    import jit, objectmodel
from rpython.rlib.rarithmetic        import r_uint

@objectmodel.always_inline
def compute_hash(x):
    assert objectmodel.we_are_translated() or type(x) is ParamKey
    return r_uint(objectmodel.compute_hash(x))

@objectmodel.always_inline
def equal(a, b):
    assert objectmodel.we_are_translated() or type(a) is ParamKey
    assert objectmodel.we_are_translated() or type(b) is ParamKey
    return a is b

ParameterizationHashTable = make_persistent_hash_type(
    super   = W_Object,
    name    = "ParameterizationHashTable",
    hashfun = compute_hash,
    equal   = equal)

# This is a Scheme_Parameterization in Racket
class RootParameterization(object):
    def __init__(self):
        # This table maps ParamKey -> W_ThreadCell
        self.table = {}

# This is a Scheme_Config in Racket
# Except that Scheme_Config uses a functional hash table and this uses a list that we copy
class W_Parameterization(W_Object):
    _immutable_fields_ = ["root", "map"]
    errorname = "parameterization"
    def __init__(self, root, map):
        self.root = root
        self.map  = map

    @jit.look_inside_iff(lambda self, params, vals: \
            jit.loop_unrolling_heuristic(params, len(params), values.UNROLLING_CUTOFF) and
            jit.loop_unrolling_heuristic(vals, len(vals), values.UNROLLING_CUTOFF))
    def extend(self, params, vals):
        assert len(params) == len(vals)
        map = self.map
        for i, param in enumerate(params):
            cell = values.W_ThreadCell(vals[i], True)
            map = map.assoc(param.get_key(), cell)
        return W_Parameterization(self.root, map)

    @jit.elidable
    def get(self, param):
        key = param.key
        result = self.map.val_at(key, None)
        if result is not None:
            return result
        result = self.root.table[key]
        assert result is not None
        return result

    def tostring(self):
        return "#<parameterization>"

# This will need to be thread-specific
top_level_config = W_Parameterization(RootParameterization(), ParameterizationHashTable.EMPTY)

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
# Must be a subclass of W_Object to fit into immutable hash tables
class ParamKey(W_Object):
    _attrs_ = []
    def __init__(self):
        pass

class W_BaseParameter(W_Object):
    errorname = "parameter"
    _attrs_ = ["guard"]
    _immutable_fields_ = ["guard"]

    ARITY = Arity.oneof(0, 1)

    def __init__(self, guard=None):
        self.guard = None if guard is values.w_false else guard

    def iscallable(self):
        return True

    def get_key(self):
        raise NotImplementedError("abstract base class")

    def get_arity(self):
        return W_BaseParameter.ARITY

    def tostring(self):
        return "#<parameter-procedure>"

class W_Parameter(W_BaseParameter):
    _immutable_fields_ = ["key"]

    def __init__(self, val, guard=None):
        W_BaseParameter.__init__(self, guard)
        self.key = ParamKey()
        cell = values.W_ThreadCell(val, True)
        top_level_config.root.table[self.key] = cell

    def get(self, cont):
        return self.get_cell(cont).get()

    def get_cell(self, cont):
        cell = find_param_cell(cont, self)
        assert isinstance(cell, values.W_ThreadCell)
        return cell

    def get_key(self):
        return self.key

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

class W_DerivedParameter(W_BaseParameter):
    _immutable_fields_ = ["parameter", "wrap"]

    def __init__(self, param, guard, wrap):
        W_BaseParameter.__init__(self, guard)
        self.parameter = param
        self.wrap      = wrap

    def get(self, cont):
        return self.parameter.get(cont)

    def get_cell(self, cont):
        return self.parameter.get_cell(cont)

    def get_key(self):
        return self.parameter.get_key()

    def call(self, args, env, cont):
        from pycket.interpreter import return_value

        if len(args) == 0:
            return self.parameter.call(args, env, call_cont(self.wrap, env, cont))
        elif len(args) == 1:
            if self.guard:
                return self.guard.call(args, env,
                        call_cont(self.parameter, env, cont))
            return self.parameter.call(args, env, cont)
        else:
            raise SchemeException("wrong number of arguments to parameter")

