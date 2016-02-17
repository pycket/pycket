from rpython.rlib.heapprof import HeapProf

class HeapProf(HeapProf):
    def is_int(self, w_obj):
        from pycket.values import W_Fixnum
        return type(w_obj) is W_Fixnum

    def get_int_val(self, w_obj):
        """ w_obj must be a boxed integer. returns the unboxed value of that
        integer. """
        from pycket.values import W_Fixnum
        assert type(w_obj) is W_Fixnum
        return w_obj.value

    def should_propagate_info(self):
        from rpython.rlib.objectmodel import we_are_translated
        from rpython.rlib.jit import we_are_jitted
        return not we_are_translated() or we_are_jitted()

