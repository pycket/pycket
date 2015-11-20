#! /usr/bin/env python
# -*- coding: utf-8 -*-

from pycket                    import values
from pycket                    import values_hash
from pycket                    import values_struct
from pycket.cont               import continuation, label, guarded_loop, call_cont, call_extra_cont
from pycket.error              import SchemeException
from pycket.impersonators.base import (
    ChaperoneMixin,
    ImpersonatorMixin,
    ProxyMixin,
    W_ImpPropertyDescriptor,
    chaperone_reference_cont,
    check_chaperone_results,
    check_chaperone_results_loop,
    get_base_object,
    impersonate_reference_cont
)
from pycket.prims.expose       import make_call_method
from pycket.values             import UNROLLING_CUTOFF
from rpython.rlib              import jit, objectmodel
from rpython.rlib.objectmodel  import import_from_mixin

class W_InterposeBox(values.W_Box):
    errorname = "interpose-box"
    _immutable_fields_ = ["unbox", "set"]

    import_from_mixin(ProxyMixin)

    def __init__(self, box, unboxh, seth, prop_keys, prop_vals):
        assert isinstance(box, values.W_Box)
        self.unboxh = unboxh
        self.seth = seth
        self.init_proxy(box, prop_keys, prop_vals)

    def immutable(self):
        return self.inner.immutable()

    def post_unbox_cont(self, env, cont):
        raise NotImplementedError("abstract method")

    def post_set_box_cont(self, val, env, cont):
        raise NotImplementedError("abstract method")

    @label
    def unbox(self, env, cont):
        after = self.post_unbox_cont(env, cont)
        return self.inner.unbox(env, after)

    @label
    def set_box(self, val, env, cont):
        after = self.post_set_box_cont(val, env, cont)
        return self.seth.call([self.inner, val], env, after)

class W_ChpBox(W_InterposeBox):
    errorname = "chp-box"

    import_from_mixin(ChaperoneMixin)

    def post_unbox_cont(self, env, cont):
        return chaperone_reference_cont(self.unboxh, [self.inner], None, env, cont)

    def post_set_box_cont(self, val, env, cont):
        return check_chaperone_results([val], env,
                imp_box_set_cont(self.inner, env, cont))

    def immutable(self):
        return self.inner.immutable()

@continuation
def imp_box_set_cont(b, env, cont, vals):
    from pycket.interpreter import check_one_val
    return b.set_box(check_one_val(vals), env, cont)

class W_ImpBox(W_InterposeBox):
    import_from_mixin(ImpersonatorMixin)

    errorname = "imp-box"

    def post_unbox_cont(self, env, cont):
        return impersonate_reference_cont(self.unboxh, [self.inner], None, env, cont)

    def post_set_box_cont(self, val, env, cont):
        return imp_box_set_cont(self.inner, env, cont)

@continuation
def imp_vec_set_cont(v, i, env, cont, vals):
    from pycket.interpreter import check_one_val
    return v.vector_set(i.value, check_one_val(vals), env, cont)

class W_InterposeVector(values.W_MVector):
    errorname = "interpose-vector"
    _immutable_fields_ = ["refh", "seth"]

    import_from_mixin(ProxyMixin)

    @jit.unroll_safe
    def __init__(self, v, r, s, prop_keys, prop_vals):
        assert isinstance(v, values.W_MVector)
        self.refh = r
        self.seth = s
        self.init_proxy(v, prop_keys, prop_vals)

    def length(self):
        return get_base_object(self).length()

    def post_set_cont(self, new, i, env, cont):
        raise NotImplementedError("abstract method")

    def post_ref_cont(self, i, env, cont):
        raise NotImplementedError("abstract method")

    @label
    def vector_set(self, i, new, env, cont):
        idx = values.W_Fixnum(i)
        after = self.post_set_cont(new, idx, env, cont)
        return self.seth.call([self.inner, idx, new], env, after)

    @label
    def vector_ref(self, i, env, cont):
        idx = values.W_Fixnum(i)
        after = self.post_ref_cont(idx, env, cont)
        return self.inner.vector_ref(i, env, after)

# Vectors
class W_ImpVector(W_InterposeVector):
    import_from_mixin(ImpersonatorMixin)

    errorname = "impersonate-vector"

    def post_set_cont(self, new, i, env, cont):
        return imp_vec_set_cont(self.inner, i, env, cont)

    def post_ref_cont(self, i, env, cont):
        return impersonate_reference_cont(self.refh, [self.inner, i], None, env, cont)

class W_ChpVector(W_InterposeVector):
    import_from_mixin(ChaperoneMixin)

    errorname = "chaperone-vector"

    def post_set_cont(self, new, i, env, cont):
        return check_chaperone_results([new], env,
                imp_vec_set_cont(self.inner, i, env, cont))

    def post_ref_cont(self, i, env, cont):
        return chaperone_reference_cont(self.refh, [self.inner, i], None, env, cont)

# Are we dealing with a struct accessor/mutator/propert accessor or a
# chaperone/impersonator thereof.
def valid_struct_proc(x):
    v = get_base_object(x)
    return (isinstance(v, values_struct.W_StructFieldAccessor) or
            isinstance(v, values_struct.W_StructFieldMutator) or
            isinstance(v, values_struct.W_StructPropertyAccessor))

@continuation
def imp_struct_set_cont(orig_struct, setter, field, app, env, cont, _vals):
    from pycket.interpreter import check_one_val
    val = check_one_val(_vals)
    if setter is values.w_false:
        return orig_struct.set_with_extra_info(field, val, app, env, cont)
    return setter.call_with_extra_info([orig_struct, val], env, cont, app)

class W_InterposeContinuationMarkKey(values.W_ContinuationMarkKey):
    errorname = "interpose-continuation-mark-key"
    _immutable_fields_ = ["get_proc", "set_proc"]

    import_from_mixin(ProxyMixin)

    def __init__(self, mark, get_proc, set_proc, prop_keys, prop_vals):
        assert get_proc.iscallable()
        assert set_proc.iscallable()
        self.get_proc = get_proc
        self.set_proc = set_proc
        self.init_proxy(mark, prop_keys, prop_vals)

    def post_set_cont(self, body, value, env, cont):
        raise NotImplementedError("abstract method")

    def post_get_cont(self, value, env, cont):
        raise NotImplementedError("abstract method")

    @label
    def get_cmk(self, value, env, cont):
        return self.get_proc.call([value], env,
                self.post_get_cont(value, env, cont))

    @label
    def set_cmk(self, body, value, update, env, cont):
        return self.set_proc.call([value], env,
                self.post_set_cont(body, value, env, cont))

@continuation
def imp_cmk_post_set_cont(body, inner, env, cont, _vals):
    from pycket.interpreter import check_one_val
    val = check_one_val(_vals)
    return inner.set_cmk(body, val, cont, env, cont)

@continuation
def imp_cmk_post_get_cont(key, env, cont, _vals):
    from pycket.interpreter import check_one_val
    val = check_one_val(_vals)
    return key.get_cmk(val, env, cont)

class W_ChpContinuationMarkKey(W_InterposeContinuationMarkKey):
    import_from_mixin(ChaperoneMixin)

    def post_get_cont(self, value, env, cont):
        return check_chaperone_results([value], env,
                imp_cmk_post_get_cont(self.inner, env, cont))

    def post_set_cont(self, body, value, env, cont):
        return check_chaperone_results([value], env,
                imp_cmk_post_set_cont(body, self.inner, env, cont))

class W_ImpContinuationMarkKey(W_InterposeContinuationMarkKey):
    import_from_mixin(ImpersonatorMixin)

    def post_get_cont(self, value, env, cont):
        return imp_cmk_post_get_cont(self.inner, env, cont)

    def post_set_cont(self, body, value, env, cont):
        return imp_cmk_post_set_cont(body, self.inner, env, cont)

class W_InterposeHashTable(values_hash.W_HashTable):
    errorname = "interpose-hash-table"
    _immutable_fields_ = ["set_proc", "ref_proc", "remove_proc", "key_proc", "clear_proc"]

    import_from_mixin(ProxyMixin)

    def __init__(self, inner, ref_proc, set_proc, remove_proc, key_proc,
                 clear_proc, prop_keys, prop_vals):
        assert isinstance(inner, values_hash.W_HashTable)
        assert set_proc.iscallable()
        assert ref_proc.iscallable()
        assert remove_proc.iscallable()
        assert key_proc.iscallable()
        assert clear_proc is values.w_false or clear_proc.iscallable()
        self.set_proc    = set_proc
        self.ref_proc    = ref_proc
        self.remove_proc = remove_proc
        self.key_proc    = key_proc
        self.clear_proc  = clear_proc
        self.init_proxy(inner, prop_keys, prop_vals)

    def post_ref_cont(self, key, env, cont):
        raise NotImplementedError("abstract method")

    def post_set_cont(self, key, val, env, cont):
        raise NotImplementedError("abstract method")

    def hash_keys(self):
        return get_base_object(self).hash_keys()

    @label
    def hash_set(self, key, val, env, cont):
        raise NotImplementedError("abstract method")

    @label
    def hash_ref(self, key, env, cont):
        after = self.post_ref_cont(key, env, cont)
        return self.ref_proc.call([self.inner, key], env, after)

@continuation
def imp_hash_table_ref_cont(ht, old, env, cont, _vals):
    from pycket.interpreter import return_value
    if _vals.num_values() != 2:
        return return_value(None, env, cont)
        raise SchemeException("hash-ref handler produced the wrong number of results")
    key, post = _vals.get_all_values()
    after = imp_hash_table_post_ref_cont(post, ht, old, env, cont)
    return ht.hash_ref(key, env, after)

@continuation
def imp_hash_table_post_ref_cont(post, ht, old, env, cont, _vals):
    from pycket.interpreter import check_one_val, return_multi_vals
    val = check_one_val(_vals)
    if val is None:
        # XXX this looks wrong, check_one_val raises if there are multiple
        # values
        return return_multi_vals(_vals, env, cont)
    return post.call([ht, old, val], env, cont)

@continuation
def chp_hash_table_ref_cont(ht, old, env, cont, _vals):
    if _vals.num_values() != 2:
        raise SchemeException("hash-ref handler produced the wrong number of results")
    key, post = _vals.get_all_values()
    after = check_chaperone_results([key], env,
                imp_hash_table_post_ref_cont(post, ht, old, env, cont))
    return ht.hash_ref(key, env, after)

class W_ImpHashTable(W_InterposeHashTable):
    import_from_mixin(ImpersonatorMixin)

    def post_set_cont(self, key, val, env, cont):
        pass

    def post_ref_cont(self, key, env, cont):
        return imp_hash_table_ref_cont(self.inner, key, env, cont)

class W_ChpHashTable(W_InterposeHashTable):
    import_from_mixin(ChaperoneMixin)

    def post_set_cont(self, key, val, env, cont):
        pass

    def post_ref_cont(self, key, env, cont):
        return chp_hash_table_ref_cont(self.inner, key, env, cont)

