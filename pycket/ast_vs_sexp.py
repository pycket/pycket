from pycket import interpreter as interp
from pycket import values, values_string, vector, util, values_regex
from pycket.prims.correlated import W_Correlated
from pycket.error import SchemeException
from pycket.hash import simple, equal, base
from pycket.assign_convert import assign_convert
from pycket.util import PerfRegion

mksym = values.W_Symbol.make

def to_rpython_list(r_list, unwrap_correlated=False, reverse=False, improper=False):
    # assumes r_list is proper
    length = 0
    acc = r_list
    while(acc is not values.w_null):
        length += 1
        if improper:
            length += 1
            break
        acc = acc.cdr()
    acc = r_list
    py_ls = [None]*length
    out = False
    for n in range(length):
        a = None
        if improper and not isinstance(acc, values.W_List):
            a = acc.get_obj() if (unwrap_correlated and isinstance(acc, W_Correlated)) else acc
            out = True
        else:
            a = acc.car().get_obj() if (unwrap_correlated and isinstance(acc.car(), W_Correlated)) else acc.car()
        if reverse:
            py_ls[length-n-1] = a
        else:
            py_ls[n] = a
        if out:
            break
        acc = acc.cdr()
    return py_ls, length

dir_sym = mksym(":D:")
bundle_sym = mksym(":B:")
linklet_sym = mksym("linklet")

import_sym = mksym("Import")
export_sym = mksym("Export")

def ast_to_sexp(form):
    from pycket.prims.linklet import W_Linklet, W_LinkletBundle, W_LinkletDirectory

    #util.console_log("ast->sexp is called with form : %s" % form.tostring(), 8)

    if is_val_type(form, extra=[vector.W_Vector, base.W_HashTable, values.W_List, values.W_Symbol]):
        return form
    elif isinstance(form, W_Linklet):
        name = form.name # W_Symbol
        importss = form.importss # [[Import ...] ...]
        exports = form.exports # {int_id:Export ...}
        body_forms = form.forms # rlist of ASTs

        # The AST contains gensymed references to all the variables
        # the linklet closes on, so we need to serialize all the
        # information we have in Import and Export objects to recreate
        # the same gensymed bindings at the instantiation of the
        # deserialized linklet.
        # So it will look like this when serialized:
        #
        # (linklet name (((Import grp gen_id int_id ext_id) ...) ...) ((Export int_id gen_int_id ext_id) ...) body)

        importss_rlist = [None]*len(importss)
        for index, imp_group in enumerate(importss):
            len_group = len(imp_group)
            importss_inst = [None]*len_group
            for i, imp_obj in enumerate(imp_group):
                importss_inst[i] = values.to_list([import_sym, imp_obj.group, imp_obj.id, imp_obj.int_id, imp_obj.ext_id])
            importss_rlist[index] = values.to_list(importss_inst)
        importss_list = values.to_list(importss_rlist)

        exports_rlist = [None]*len(exports)
        i = 0
        for k, exp_obj in exports.iteritems():
            exports_rlist[i] = values.to_list([export_sym, k, exp_obj.int_id, exp_obj.ext_id])
            i += 1

        exports_list = values.to_list(exports_rlist)

        body_forms_rlist = [None]*len(body_forms)
        for index, ast_form in enumerate(body_forms):
            body_forms_rlist[index] = ast_form.to_sexp()

        linklet_rlist = [linklet_sym, name, importss_list, exports_list] + body_forms_rlist
        linklet_s_exp = values.to_list(linklet_rlist)

        return linklet_s_exp
    elif isinstance(form, W_LinkletBundle) or isinstance(form, W_LinkletDirectory):
        bd_sym = None
        if isinstance(form, W_LinkletBundle):
            bd_sym = mksym(":B:")
        else:
            bd_sym = mksym(":D:")

        mapping = form.get_mapping()
        l = mapping.length()
        keys = [None]*l
        vals = [None]*l

        if isinstance(mapping, equal.W_EqualHashTable):
            i = 0
            for k, v in mapping.hash_items():
                keys[i] = k
                vals[i] = ast_to_sexp(v)
                i += 1

            return values.W_Cons.make(bd_sym, equal.W_EqualHashTable(keys, vals, immutable=True))
        elif isinstance(mapping, simple.W_EqImmutableHashTable):
            i = 0
            for k, v in mapping.iteritems():
                keys[i] = k
                vals[i] = ast_to_sexp(v)
                i += 1

            return values.W_Cons.make(bd_sym, simple.make_simple_immutable_table(simple.W_EqImmutableHashTable, keys, vals))
        else:
            raise SchemeException("Something wrong with the bundle/directory mapping : %s" % mapping.tostring())
    else:
        return form.to_sexp()

def def_vals_to_ast(def_vals_sexp, exports, all_toplevels, linkl_imports, mutated_ids):
    ls, ln = to_rpython_list(def_vals_sexp)
    if not ln == 3:
        raise SchemeException("defs_vals_to_ast : unhandled define-values form : %s" % def_vals_sexp.tostring())

    names = ls[1] # def_vals_sexp.cdr().car()
    names_ls, names_ln = to_rpython_list(names, unwrap_correlated=True)

    the_name = names_ls[0].variable_name() if names_ln > 0 else ""
    body = sexp_to_ast(ls[2], [], exports, all_toplevels, linkl_imports, mutated_ids, cell_ref=[], name=the_name)

    return interp.DefineValues(names_ls, body, names_ls)

def lam_to_ast(lam_sexp, lex_env, exports, all_toplevels, linkl_imports, mutated_ids, cell_ref, name=""):
    from pycket.expand import SourceInfo

    lam_sexp_elements, l = to_rpython_list(lam_sexp)
    if not (l == 3 or l == 2):
        raise SchemeException("lam_to_ast : unhandled lambda form : %s" % lam_sexp.tostring())

    if lam_sexp.car() is mksym("lambda"):
        lam_sexp = lam_sexp.cdr()

    formals_ = lam_sexp.car()
    rest = None
    formals_ls = []
    formals_len = 0
    if isinstance(formals_, values.W_Symbol):
        # check for a "rest"
        rest = formals_
        lex_env.append(rest)
    else:
        # two passes over the formals
        # 1) determine the rest arg and the number of formal args
        while (formals_ is not values.w_null):
            if isinstance(formals_, values.W_Symbol):
                rest = formals_
                lex_env.append(formals_)
                break
            elif formals_.car() is mksym("."):
                # another check for a "rest"
                if formals_.cdr() is values.w_null:
                    raise SchemeException("lam_to_ast : invalid lambda form : %s" % lam_sexp.tostring())
                rest = formals_.cdr().car()
                lex_env.append(rest)
                break
            formals_len += 1
            formals_ = formals_.cdr()

        # 2) make the r_list for formals
        formals_ls = [None]*formals_len
        formals_ = lam_sexp.car() # reset
        index = 0
        while isinstance(formals_, values.W_Cons) and formals_.car() is not mksym("."):
            formals_ls[index] = formals_.car()
            index += 1
            formals_ = formals_.cdr()

    body = sexp_to_ast(lam_sexp.cdr().car(), formals_ls + lex_env, exports, all_toplevels, linkl_imports, mutated_ids, cell_ref=[], name=name)
    dummy = 1
    return interp.make_lambda(formals_ls, rest, [body], SourceInfo(dummy, dummy, dummy, dummy, name))

def let_like_to_ast(let_sexp, lex_env, exports, all_toplevels, linkl_imports, mutated_ids, is_letrec, cell_ref):

    let_ls, let_len = to_rpython_list(let_sexp)

    # just a sanity check
    if not (let_ls[0] is mksym("let-values") or (let_ls[0] is mksym("letrec-values") and is_letrec)):
        raise SchemeException("let_to_ast : unhandled let form : %s" % let_sexp.tostring())

    varss_rhss, varss_len = to_rpython_list(let_ls[1])

    if is_letrec:
        # populate lex_env
        for rhs in varss_rhss: # rhs : ((id ...) rhs-expr)
            ids, ids_len = to_rpython_list(rhs.car(), unwrap_correlated=True) # (id ...)
            lex_env += ids

    varss_list = [None] * varss_len
    rhss_list = [None] * varss_len
    num_ids = 0
    i = 0
    for w_vars_rhss in varss_rhss:
        varr, varr_len = to_rpython_list(w_vars_rhss.car(), unwrap_correlated=True)
        varss_list[i] = varr

        rhsr = sexp_to_ast(w_vars_rhss.cdr().car(), lex_env, exports, all_toplevels, linkl_imports, mutated_ids, cell_ref=[])
        rhss_list[i] = rhsr
        i += 1
        num_ids += varr_len

    ids = [None] * num_ids
    index = 0
    for vars_ in varss_list:
        for var_ in vars_:
            ids[index] = var_ # W_Symbol
            index += 1

    let_body_ls = let_ls[2:]
    body_ls = [None]*(let_len-2)

    for index, b in enumerate(let_body_ls):
        body_ls[index] = sexp_to_ast(b, ids + lex_env, exports, all_toplevels, linkl_imports, mutated_ids, cell_ref=[])

    if varss_len == 0:
        return interp.Begin.make(body_ls)

    if is_letrec:
        return interp.make_letrec(varss_list, rhss_list, body_ls)
    else:
        return interp.make_let(varss_list, rhss_list, body_ls)

def is_val_type(form, extra=[]):
    val_types = [values.W_Number,
                 values.W_Void,
                 values.W_Bool,
                 values.W_Bytes,
                 values_regex.W_AnyRegexp,
                 values_string.W_String,
                 values.W_ImmutableBytes,
                 values.W_Character] + extra
    for t in val_types:
        if isinstance(form, t):
            return True
    return False

def is_imported(id_sym, linkl_importss):
    for imp_index, imports_group in enumerate(linkl_importss):
        for imp in imports_group:
            if id_sym is imp.int_id:
                return imp.id
    return None

begin_sym = mksym("begin")
begin0_sym = mksym("begin0")
def_val_sym = mksym("define-values")
wcm_sym = mksym("with-continuation-mark")
variable_ref_sym = mksym("#%variable-reference")
caselam_sym = mksym("case-lambda")
lam_sym = mksym("lambda")
let_sym = mksym("let-values")
letrec_sym = mksym("letrec-values")
set_bang_sym = mksym("set!")
quote_sym = mksym("quote")
if_sym = mksym("if")

var_ref_sym = mksym("variable-ref")
var_ref_no_check_sym = mksym("variable-ref/no-check")
var_set_check_undef_sym = mksym("variable-set!/check-undefined")
var_set_sym = mksym("variable-set!")

var_prim_syms = [var_ref_sym, var_ref_no_check_sym, var_set_check_undef_sym, var_set_sym]

var_ref_mod_var = interp.ModuleVar(var_ref_sym, "#%kernel", var_ref_sym, None)
var_ref_no_check_mod_var = interp.ModuleVar(var_ref_no_check_sym, "#%kernel", var_ref_no_check_sym, None)
var_set_check_undef_mod_var = interp.ModuleVar(var_set_check_undef_sym, "#%kernel", var_set_check_undef_sym, None)
var_set_mod_var = interp.ModuleVar(var_set_sym, "#%kernel", var_set_sym, None)

known_mod_vars = {} # cache for kernel primitive ModuleVars

def sexp_to_ast(form, lex_env, exports, all_toplevels, linkl_importss, mutated_ids, cell_ref=[], name=""):

    if isinstance(form, W_Correlated):
        return sexp_to_ast(form.get_obj(), lex_env, exports, all_toplevels, linkl_importss, mutated_ids, cell_ref, name)
    elif is_val_type(form):
        return interp.Quote(form)
    elif isinstance(form, values.W_Symbol):
        if form in cell_ref:
            return interp.CellRef(form)
        if form in lex_env:
            return interp.LexicalVar(form)
        if form in exports and (form in mutated_ids or form not in all_toplevels):
            # dynamically find the W_LinkletVar for the exported variable
            # possible point of optimization
            rands = [interp.LinkletVar(exports[form].int_id)]
            return interp.App.make(var_ref_mod_var, rands)
        if form in all_toplevels:
            return interp.ToplevelVar(form, is_free=False)

        import_var_int_id = is_imported(form, linkl_importss)
        if import_var_int_id: # this is gensymed internal variable name
            # dynamically find the W_LinkletVar for the imported variable
            # possible point of optimization
            rands = [interp.LinkletVar(import_var_int_id)]
            return interp.App.make(var_ref_no_check_mod_var, rands)

        # kernel primitive ModuleVar
        if form in known_mod_vars:
            return known_mod_vars[form]

        m_var = interp.ModuleVar(form, "#%kernel", form, None)
        known_mod_vars[form] = m_var
        return m_var
    elif isinstance(form, values.W_List):
        first_form = form.car()
        ### these are for the desearialization of the linklet body
        if first_form in var_prim_syms:
            linklet_var_sym = form.cdr().car()
            rator, rands = None, None
            if first_form is var_set_sym or first_form is var_set_check_undef_sym:
                rator = var_set_mod_var if first_form is var_set_sym else var_set_check_undef_mod_var
                linklet_var = interp.LinkletVar(linklet_var_sym)
                new_val = sexp_to_ast(form.cdr().cdr().car(), lex_env, exports, all_toplevels, linkl_importss, mutated_ids, cell_ref, name)
                mode = interp.Quote(values.w_false) # FIXME: possible optimization
                rands = [linklet_var, new_val, mode]
                return interp.App.make(rator, rands)
            if first_form is var_ref_sym or first_form is var_ref_no_check_sym:
                rator = var_ref_mod_var if first_form is var_ref_sym else var_ref_no_check_mod_var
                rands = [interp.LinkletVar(linklet_var_sym)]
            return interp.App.make(rator, rands)
        ###
        if first_form is begin_sym:
            begin_exprs, ln = to_rpython_list(form.cdr())
            return interp.Begin.make([sexp_to_ast(f, lex_env, exports, all_toplevels, linkl_importss, mutated_ids, cell_ref, name) for f in begin_exprs])
        elif first_form is begin0_sym:
            fst = sexp_to_ast(form.cdr().car(), lex_env, exports, all_toplevels, linkl_importss, mutated_ids, cell_ref, name)
            rst_exprs, rest_len = to_rpython_list(form.cdr().cdr())
            rst = [sexp_to_ast(f, lex_env, exports, all_toplevels, linkl_importss, mutated_ids, cell_ref, name) for f in rst_exprs]
            if rest_len == 0:
                return fst
            else:
                return interp.Begin0.make(fst, rst)
        elif first_form is def_val_sym:
            return def_vals_to_ast(form, exports, all_toplevels, linkl_importss, mutated_ids)
        elif first_form is wcm_sym:
            from pycket.prims.general import elidable_length
            if elidable_length(form) != 4:
                raise SchemeException("Unrecognized with-continuation-mark form : %s" % form.tostring())
            key = sexp_to_ast(form.cdr().car(), lex_env, exports, all_toplevels, linkl_importss, mutated_ids, cell_ref, name)
            val = sexp_to_ast(form.cdr().cdr().car(), lex_env, exports, all_toplevels, linkl_importss, mutated_ids, cell_ref, name)
            body = sexp_to_ast(form.cdr().cdr().cdr().car(), lex_env, exports, all_toplevels, linkl_importss, mutated_ids, cell_ref, name)
            return interp.WithContinuationMark(key, val, body)
        elif first_form is variable_ref_sym:
            if form.cdr() is values.w_null: # (variable-reference)
                return interp.VariableReference(None, None)
            elif form.cdr().cdr() is values.w_null: # (variable-reference id)
                if isinstance(form.cdr().car(), values.W_Symbol):
                    var = sexp_to_ast(form.cdr().car(), lex_env, exports, all_toplevels, linkl_importss, mutated_ids, cell_ref, name)
                    return interp.VariableReference(var, "dummy-path.rkt") # FIXME
                elif isinstance(form.cdr().car(), values.W_Fixnum):
                    # because we're 'writing' variable-reference with is_mutable information
                    is_mut = False
                    if form.cdr().car().toint() != 0:
                        is_mut = True
                    return interp.VariableReference(None, None, is_mut)
                else:
                    raise SchemeException("Invalid variable-reference form : %s -- arg type : %s" % (form.tostring(), form.cdr().car()))
            elif form.cdr().cdr().cdr() is values.w_null: # (variable-reference 1 2)
                raise SchemeException("Unhandled variable-reference form : %s" % (form.tostring()))
            else:
                # This is to handle varrefs serialized by Pycket
                # no Racket varref has more than 1 argument
                var_ = form.cdr().car()
                path_ = form.cdr().cdr().car()
                mut_ = form.cdr().cdr().cdr().car()
                var = None
                path = None
                mut = False

                if var_ is not values.w_false:
                    var = sexp_to_ast(var_, lex_env, exports, all_toplevels, linkl_importss, mutated_ids, cell_ref, name)

                if isinstance(path_, values.W_Object) and path_ is not values.w_false:
                    path = path_.tostring()
                elif isinstance(path_, str):
                    path = path_

                if mut_ is values.w_true:
                    mut = True

                return interp.VariableReference(var, path, mut)

        elif first_form is caselam_sym:
            maybe_rec_sym_part = values.w_null
            if form.cdr() is not values.w_null:
                maybe_rec_sym_part = form.cdr().car() # (recursive-sym <sym>)
            rec_sym = None
            new_lex_env = lex_env
            lams_part = form.cdr()

            if isinstance(maybe_rec_sym_part, values.W_Cons) and maybe_rec_sym_part is not values.w_null:
                if maybe_rec_sym_part.car() is mksym("recursive-sym"):
                    # then we're reading a caselam that we wrote
                    lams_part = form.cdr().cdr()
                    if maybe_rec_sym_part.cdr() is not values.w_null:
                        rec_sym = maybe_rec_sym_part.cdr().car()
                        new_lex_env = lex_env + [rec_sym]

            lams_expr, ln = to_rpython_list(lams_part)
            lams = [lam_to_ast(f, new_lex_env, exports, all_toplevels, linkl_importss, mutated_ids, cell_ref, name) for f in lams_expr]
            return interp.CaseLambda(lams, rec_sym)
        elif first_form is lam_sym:
            return interp.CaseLambda([lam_to_ast(form, lex_env, exports, all_toplevels, linkl_importss, mutated_ids, cell_ref, name)])
        elif first_form is let_sym:
            return let_like_to_ast(form, lex_env, exports, all_toplevels, linkl_importss, mutated_ids, False, cell_ref)
        elif first_form is letrec_sym:
            return let_like_to_ast(form, lex_env, exports, all_toplevels, linkl_importss, mutated_ids, True, cell_ref)
        elif first_form is set_bang_sym:
            import_id = is_imported(form.cdr().car(), linkl_importss)
            if import_id:
                raise SchemeException("cannot mutate imported variable : %s" % form.tostring())
            cr = cell_ref
            target = form.cdr().car()
            rhs = sexp_to_ast(form.cdr().cdr().car(), lex_env, exports, all_toplevels, linkl_importss, mutated_ids, cell_ref, name)
            # if it's for an exported variable, don't emit a set!
            # we're going to variable-set! the exported variable
            if target in exports:
                rator = var_set_check_undef_mod_var
                mode = interp.Quote(values.w_false) # FIXME: possible optimization
                rands = [interp.LinkletVar(exports[target].int_id), rhs, mode]
                return interp.App.make(rator, rands)
            if target in lex_env:
                cr = [target] if not cr else [target] + cr
            var = sexp_to_ast(form.cdr().car(), lex_env, exports, all_toplevels, linkl_importss, mutated_ids, cell_ref=cr, name=name)
            rhs = sexp_to_ast(form.cdr().cdr().car(), lex_env, exports, all_toplevels, linkl_importss, mutated_ids, cell_ref, name)
            assert isinstance(var, interp.Var)
            return interp.SetBang(var, rhs)
        elif first_form is quote_sym:
            if form.cdr() is values.w_null or form.cdr().cdr() is not values.w_null:
                raise SchemeException("malformed quote form : %s" % form.tostring())
            return interp.Quote(form.cdr().car())
        elif first_form is if_sym:
            tst_w = form.cdr().car()
            thn_w = form.cdr().cdr().car()
            els_w = form.cdr().cdr().cdr().car()
            tst = sexp_to_ast(tst_w, lex_env, exports, all_toplevels, linkl_importss, mutated_ids, cell_ref, name)
            thn = sexp_to_ast(thn_w, lex_env, exports, all_toplevels, linkl_importss, mutated_ids, cell_ref, name)
            els = sexp_to_ast(els_w, lex_env, exports, all_toplevels, linkl_importss, mutated_ids, cell_ref, name)
            return interp.If.make(tst, thn, els)
        else:
            form_rator = sexp_to_ast(first_form, lex_env, exports, all_toplevels, linkl_importss, mutated_ids, cell_ref)

            rands_ls, rands_len = to_rpython_list(form.cdr())
            rands = [sexp_to_ast(r, lex_env, exports, all_toplevels, linkl_importss, mutated_ids, cell_ref, name) for r in rands_ls]

            return interp.App.make(form_rator, rands)
    else:
        raise SchemeException("Don't know what to do with this form yet : %s" % form.tostring())

def looks_like_linklet(sexp):
    # (linklet () () ...)
    # we know the sexp is not w_null
    if not isinstance(sexp, values.W_Cons):
        return False
    if sexp.car() is not linklet_sym:
        return False
    if not isinstance(sexp.cdr(), values.W_Cons):
        return False
    if not isinstance(sexp.cdr().cdr(), values.W_Cons):
        return False

    maybe_name = sexp.cdr().car()

    named = isinstance(maybe_name, values.W_Symbol)

    if named and not isinstance(sexp.cdr().cdr().cdr(), values.W_Cons):
        return False

    rest = sexp.cdr() if (not named) else sexp.cdr().cdr()

    # check the imports/exports
    _imports = rest.car()
    _exports = rest.cdr().car()
    # FIXME : also check the imports and exports' inner structures
    if not isinstance(_imports, values.W_List) or not isinstance(_exports, values.W_List):
        return False

    return True

class Import(object):
    def __init__(self, group, id, int_id, ext_id):
        self.group = group
        self.id = id
        self.int_id = int_id
        self.ext_id = ext_id

def get_imports_from_w_importss_sexp(w_importss):
    from pycket.interpreter import Gensym
    importss_acc, importss_len = to_rpython_list(w_importss)
    importss_list = [None]*importss_len
    for index, importss_current in enumerate(importss_acc):
        importss_group_ls, group_len = to_rpython_list(importss_current)
        inner_acc = [None]*group_len
        for i, c in enumerate(importss_group_ls):
            if isinstance(c, values.W_Symbol):
                w_imp_sym = Gensym.gensym(c.tostring())
                inner_acc[i] = Import(values.W_Fixnum(index), w_imp_sym, c, c)
            elif isinstance(c, values.W_List):
                if c.cdr().cdr() is not values.w_null:
                    raise SchemeException("Unhandled renamed import form : %s" % c.tostring())
                external_id = c.car().get_obj() if isinstance(c.car(), W_Correlated) else c.car()
                internal_id = c.cdr().car().get_obj() if isinstance(c.cdr().car(), W_Correlated) else c.cdr().car()
                w_internal_id = Gensym.gensym(internal_id.tostring())
                inner_acc[i] = Import(values.W_Fixnum(index), w_internal_id, internal_id, external_id)
            elif isinstance(c, W_Correlated):
                cc = c.get_obj()
                w_cc = Gensym.gensym(cc.tostring())
                inner_acc[i] = Import(values.W_Fixnum(index), w_cc, cc, cc)
            else:
                raise SchemeException("uncrecognized import : %s" % c.tostring())
        importss_list[index] = inner_acc
    return importss_list

class Export(object):
    def __init__(self, int_gensym, ext_id):
        self.int_id = int_gensym
        self.ext_id = ext_id

def get_exports_from_w_exports_sexp(w_exports):
    from pycket.interpreter import Gensym
    r_exports, exports_len = to_rpython_list(w_exports)
    exports = {}
    for i, exp in enumerate(r_exports):
        if isinstance(exp, values.W_WrappedConsProper):
            car = exp.car()
            internal_name = car.get_obj() if isinstance(car, W_Correlated) else car
            cadr =  exp.cdr().car()
            external_name = cadr.get_obj() if isinstance(cadr, W_Correlated) else cadr
            w_internal_name = Gensym.gensym(internal_name.tostring())
            # don't gensym the external_id
            exports[internal_name] = Export(w_internal_name, external_name)
        else:
            c_exp = exp.get_obj() if isinstance(exp, W_Correlated) else exp
            w_c_exp = Gensym.gensym(c_exp.tostring())
            exports[c_exp] = Export(w_c_exp, c_exp)
    return exports

# collect the ids in define-values forms
def get_toplevel_defined_ids(forms_ls):
    linkl_toplevels = {} # {W_Symbol:None}
    for form in forms_ls:
        if isinstance(form, W_Correlated):
            form = form.get_obj()
        if isinstance(form, values.W_List) and form.car() is mksym("define-values"):
            ids = form.cdr().car()
            ids_ls, ids_len = to_rpython_list(ids, unwrap_correlated=True)
            # create LinkletVar for each id
            for id in ids_ls:
                if id in linkl_toplevels:
                    raise SchemeException("duplicate binding name : %s" % id.tostring())
                linkl_toplevels[id] = None

    return linkl_toplevels

def extend_dict(a, b):
    for k,v in b.iteritems():
        a[k] = v
    return a

def extend_dicts(list_of_dicts):
    a = {}
    for d in list_of_dicts:
        a = extend_dict(a, d)
    return a

def find_mutated(form):
    if isinstance(form, W_Correlated):
        return find_mutated(form.get_obj())
    elif isinstance(form, values.W_Cons):
        if not form.is_proper_list():
            elements, _ = to_rpython_list(form, unwrap_correlated=True, improper=True)
            return extend_dicts([find_mutated(f) for f in elements])
        c = form.car()
        if c is set_bang_sym:
            return extend_dict({form.cdr().car():None}, find_mutated(form.cdr().cdr().car()))
        elif isinstance(c, values.W_Cons) and c is not values.w_null:
            all_exprs, _ = to_rpython_list(form, unwrap_correlated=True)
            return extend_dicts([find_mutated(f) for f in all_exprs])
        else:
            rest_exprs, _ = to_rpython_list(form.cdr(), unwrap_correlated=True)
            return extend_dicts([find_mutated(f) for f in rest_exprs])
    else:
        return {}

def process_w_body_sexp(w_body, importss_list, exports, from_zo=False):
    body_forms_ls, body_length = to_rpython_list(w_body, unwrap_correlated=True)
    cur_toplevels = {}

    # make a recursive (!arbitrarily deep!) pass to find set!ed ids
    mutated = find_mutated(w_body) # {W_Symbol:None}

    # another pass to find toplevel defined ids
    all_toplevels = get_toplevel_defined_ids(body_forms_ls)
    variable_set_lines = 0
    for d in all_toplevels:
        if d in exports:
            variable_set_lines += 1
    # for each exported defined id, we need to add a variable-set! for
    # the exported var with the defined id
    total_forms_len = body_length + variable_set_lines
    body_forms = [None]*(total_forms_len)
    added = 0
    current_index = 0
    # this juggling is because we don't know how many extra ast forms
    # we're going to add for the exported defined ids
    for b in body_forms_ls:
        b_form = sexp_to_ast(b, [], exports, all_toplevels, importss_list, mutated)
        if not from_zo: # no need to normalize if it's alread normalized
            with PerfRegion("compile-normalize"):
                b_form = interp.Context.normalize_term(b_form)
        with PerfRegion("compile-assign-convert"):
            b_form = assign_convert(b_form)
        body_forms[current_index+added] = b_form
        current_index += 1
        if isinstance(b_form, interp.DefineValues):
            for n in b_form.names:
                if n in exports:
                    rator = interp.ModuleVar(var_set_sym, "#%kernel", var_set_sym, None)
                    exp_var = interp.LinkletVar(exports[n].int_id)
                    top_var = interp.ToplevelVar(n, is_free=False)
                    mode = interp.Quote(values.w_false) # FIXME: possible optimization
                    rands = [exp_var, top_var, mode]
                    body_forms[current_index+added] = interp.App.make(rator,rands)
                    added += 1

    return body_forms

def looks_like_an_import(sexp):
    # should be (Import grp gen_id int_id ext_id)
    if not isinstance(sexp, values.W_Cons):
        return False
    if sexp.car() is not import_sym:
        return False
    if not isinstance(sexp.cdr(), values.W_Cons):
        return False
    if not isinstance(sexp.cdr().cdr(), values.W_Cons):
        return False
    if not isinstance(sexp.cdr().cdr().cdr(), values.W_Cons):
        return False
    if not isinstance(sexp.cdr().cdr().cdr().cdr(), values.W_Cons):
        return False
    return True

# We can't use the same thing with what compile-linklet uses anymore,
# becuse what we serialize is now specific to Pycket (contains some
# extra info than a regular linklet s-expr that the expander would
# pass)
def deserialize_importss(w_importss):
    importss_acc, importss_len = to_rpython_list(w_importss)
    importss_list = [None]*importss_len
    for index, importss_current in enumerate(importss_acc):
        importss_group_ls, group_len = to_rpython_list(importss_current)
        inner_acc = [None]*group_len
        for i, c in enumerate(importss_group_ls):
            if looks_like_an_import(c):
                w_grp_index = c.cdr().car()
                id          = c.cdr().cdr().car()
                int_id      = c.cdr().cdr().cdr().car()
                ext_id      = c.cdr().cdr().cdr().cdr().car()
                inner_acc[i] = Import(w_grp_index, id, int_id, ext_id)
            else:
                raise SchemeException("looks like an invalid serialization of import : %s" % c.tostring())
        importss_list[index] = inner_acc
    return importss_list

def looks_like_an_export(sexp):
    # should be (Import grp gen_id int_id ext_id)
    if not isinstance(sexp, values.W_Cons):
        return False
    if sexp.car() is not export_sym:
        return False
    if not isinstance(sexp.cdr(), values.W_Cons):
        return False
    if not isinstance(sexp.cdr().cdr(), values.W_Cons):
        return False
    if not isinstance(sexp.cdr().cdr().cdr(), values.W_Cons):
        return False
    return True

# See the comment for deserialize_importss
def deserialize_exports(w_exports):
    r_exports, exports_len = to_rpython_list(w_exports)
    exports = {}
    for i, exp in enumerate(r_exports):
        if looks_like_an_export(exp):
            k = exp.cdr().car()
            gen_int_id = exp.cdr().cdr().car()
            ext_id = exp.cdr().cdr().cdr().car()
            exports[k] = Export(gen_int_id, ext_id)
        else:
            raise SchemeException("looks like an invalid serialization of export : %s" % exp.tostring())
    return exports

def deserialize_loop(sexp):
    from pycket.prims.linklet import W_Linklet, W_LinkletBundle, W_LinkletDirectory
    from pycket.env import w_global_config

    if isinstance(sexp, values.W_Cons):
        c = sexp.car()
        if c is dir_sym:
            dir_map = sexp.cdr()
            return W_LinkletDirectory(deserialize_loop(dir_map))
        elif c is bundle_sym:
            bundle_map = sexp.cdr()
            return W_LinkletBundle(deserialize_loop(bundle_map))
        elif looks_like_linklet(sexp):
            # Unify this with compile_linklet
            if isinstance(sexp.cdr().car(), values.W_List):
                w_name = mksym("anonymous")
                w_importss = sexp.cdr().car()
                w_exports = sexp.cdr().cdr().car()
                w_body = sexp.cdr().cdr().cdr()
            else:
                w_name = sexp.cdr().car()
                w_importss = sexp.cdr().cdr().car()
                w_exports = sexp.cdr().cdr().cdr().car()
                w_body = sexp.cdr().cdr().cdr().cdr()

            importss_list = deserialize_importss(w_importss)

            # Process the exports
            exports = deserialize_exports(w_exports)

            # Process the body
            with PerfRegion("compile-sexp-to-ast"):
                body_forms = process_w_body_sexp(w_body, importss_list, exports, from_zo=True)

            return W_Linklet(w_name, importss_list, exports, body_forms)
        else:
            # get the length
            ls = sexp
            length = 0
            is_improper = False
            while ls is not values.w_null:
                if isinstance(ls, values.W_Cons):
                    length += 1
                    ls = ls.cdr()
                else:
                    is_improper = True
                    ls = values.w_null

            # allocate an r_list (to avoid reversing w_list)
            if is_improper:
                sexp_ls = [None]*(length+1)
            else:
                sexp_ls = [None]*length

            # second pass, get the elements
            ls = sexp
            for i in range(length-1, -1, -1):
                sexp_ls[i] = ls.car()
                ls = ls.cdr()

            if is_improper:
                sexp_ls[length] = ls

            # make the new list
            new = values.w_null
            for s in sexp_ls:
                new = values.W_Cons.make(deserialize_loop(s), new)

            return new
    elif isinstance(sexp, simple.W_EqImmutableHashTable):
        l = sexp.length()
        keys = [None]*l
        vals = [None]*l
        i = 0
        for k, v in sexp.iteritems():
            keys[i] = k
            vals[i] = deserialize_loop(v)
            i += 1

        return simple.make_simple_immutable_table(simple.W_EqImmutableHashTable, keys, vals)
    elif isinstance(sexp, equal.W_EqualHashTable):
        l = sexp.length()
        keys = [None]*l
        vals = [None]*l
        i = 0
        for k, v in sexp.hash_items():
            keys[i] = k
            vals[i] = deserialize_loop(v)
            i += 1

        return equal.W_EqualHashTable(keys, vals, immutable=True)
    elif isinstance(sexp, equal.W_EqualAlwaysHashTable):
        l = sexp.length()
        keys = [None]*l
        vals = [None]*l
        i = 0
        for k, v in sexp.hash_items():
            keys[i] = k
            vals[i] = deserialize_loop(v)
            i += 1

        return equal.W_EqualAlwaysHashTable(keys, vals, immutable=True)
    elif isinstance(sexp, vector.W_Vector):
        new = [None]*sexp.length()
        items = sexp.get_strategy().ref_all(sexp)
        for index, obj in enumerate(items):
            new[index] = deserialize_loop(obj)

        return vector.W_Vector.fromelements(new, sexp.immutable())
    else:
        return sexp
