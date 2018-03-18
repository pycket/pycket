#! /usr/bin/env python
# -*- coding: utf-8 -*-

import pytest
from pycket.values import W_Symbol
from pycket.error import SchemeException
from pycket.prims.linklet import W_LinkletInstance, w_uninitialized

from pycket.linklet_test.testhelper import (make_linklet, inst, get_val, defines, variables, get_var_val)


@pytest.mark.linkl
def test_instantiate_basic():
    l1 = make_linklet("(linklet () (x) (define-values (x) 4))")

    l1_inst = inst(l1)
    assert isinstance(l1_inst, W_LinkletInstance)
    assert get_val(l1_inst, "x").value == 4

@pytest.mark.linkl
def test_instantiate_target():
    l1 = make_linklet("(linklet () (x) (define-values (x) 4) (+ x x))")
    targ = make_linklet("(linklet () ())")

    targ_inst = inst(targ)
    result = inst(l1, target=targ_inst)
    assert result.value == 8
    assert get_val(targ_inst, "x").value == 4

@pytest.mark.linkl
def test_instantiate_target_def_overwrite():
    l1 = make_linklet("(linklet () (x) (define-values (x) 4) (+ x x))")
    targ = make_linklet("(linklet () () (define-values (x) 1) (define-values (y) 2))")
    targ_inst = inst(targ)
    result = inst(l1, target=targ_inst)
    assert result.value == 8
    assert get_val(targ_inst, "x").value == 4
    assert get_val(targ_inst, "y").value == 2

    # if linklet doesn't export, then target's def stay the same
    l1 = make_linklet("(linklet () () (define-values (x) 4) (+ x x))")
    targ = make_linklet("(linklet () () (define-values (x) 1) (define-values (y) 2))")
    targ_inst = inst(targ)
    result = inst(l1, target=targ_inst)
    assert result.value == 8
    assert get_val(targ_inst, "x").value == 1
    assert get_val(targ_inst, "y").value == 2

    # if target doesn't have it, then it doesn't matter if linklet exports or not,
    # put the variable in the target
    l1 = make_linklet("(linklet () () (define-values (z) 4) z)")
    targ = make_linklet("(linklet () ())")
    targ_inst = inst(targ)
    result = inst(l1, target=targ_inst)
    assert result.value == 4
    assert get_val(targ_inst, "z").value == 4

    # imported variables doesn't get into target at all ...
    l1 = make_linklet("(linklet () (x) (define-values (x) 4))")
    l2 = make_linklet("(linklet ((x)) () (+ x x))")
    targ = make_linklet("(linklet () () )")
    l1_inst = inst(l1)
    targ_inst = inst(targ)
    result = inst(l2, [l1_inst], target=targ_inst)
    assert result.value == 8
    assert not defines(targ_inst, "x")

    # ... let alone overwrite any var inside the target
    l1 = make_linklet("(linklet () (x) (define-values (x) 4))")
    l2 = make_linklet("(linklet ((x)) () (+ x x))")
    targ = make_linklet("(linklet () () (define-values (x) 1))")
    l1_inst = inst(l1)
    targ_inst = inst(targ)
    result = inst(l2, [l1_inst], target=targ_inst)
    assert result.value == 8
    assert get_val(targ_inst, "x").value == 1

@pytest.mark.linkl
def test_instantiate_defs_export_names():
    l1 = make_linklet("(linklet () ((x x15)) (define-values (x) 4))")
    l1_inst = inst(l1)
    assert not defines(l1_inst, "x")
    assert defines(l1_inst, "x15")

    # LinkletVars will be referred by the external name (e.g. (+ x15 x15)
    l1 = make_linklet("(linklet () ((x x15)) (define-values (x) 4) (+ x x))")
    targ = make_linklet("(linklet () ())")
    targ_inst = inst(targ)

    result = inst(l1, [], target=targ_inst)
    assert result.value == 8
    assert not defines(targ_inst, "x")
    assert defines(targ_inst, "x15")

@pytest.mark.linkl
def test_instantiate_discarding_defs():
    l1 = make_linklet("(linklet () ((x x15)) (define-values (x) 4) (define-values (x15) 75))")
    l1_inst = inst(l1)

    assert not defines(l1_inst, "x")
    assert get_val(l1_inst, "x15").value == 4 #### Not 75!
    k,v = get_var_val(l1_inst, "x15.1")
    assert v.value == 75

    l1 = make_linklet("(linklet () ((x x15) k) (define-values (x) 4) (define-values (x15) 75) (define-values (k) x15))")
    l1_inst = inst(l1)

    assert not defines(l1_inst, "x")
    assert get_val(l1_inst, "x15").value == 4 #### Not 75!
    assert get_val(l1_inst, "k").value == 75 #### Not 4!

    #vars = variables(l1_inst)
    k,v = get_var_val(l1_inst, "x15.1")
    assert v.value == 75

@pytest.mark.linkl
def test_instantiate_use_targets_def():
    l1 = make_linklet("(linklet () (x) (+ x x))")
    targ = make_linklet("(linklet () () (define-values (x) 10))")
    targ_inst = inst(targ)
    result = inst(l1, target=targ_inst)
    assert result.value == 20

    # use linklet's definition if both linklet and target have it
    l2 = make_linklet("(linklet () () (define-values (x) 4) (+ x x))") # doesn't export x
    targ = make_linklet("(linklet () () (define-values (x) 10))")
    targ_inst = inst(targ)    
    result = inst(l2, target=targ_inst)
    assert result.value == 8
    assert get_val(targ_inst, "x").value == 10

@pytest.mark.linkl
def test_instantiate_basic_import():
    l1 = make_linklet("(linklet () (x) (define-values (x) 4))")
    l2 = make_linklet("(linklet ((x)) () (+ x x))")
    targ = make_linklet("(linklet () ())")
    l1_inst = inst(l1)
    targ_inst = inst(targ)
    result = inst(l2, [l1_inst], target=targ_inst)
    assert result.value == 8

    l1 = make_linklet("(linklet () (x) (define-values (x) 4))")
    l2 = make_linklet("(linklet ((x)) (y) (define-values (y) (+ x x)) (+ y y))")
    targ = make_linklet("(linklet () ())")
    l1_inst = inst(l1)
    targ_inst = inst(targ)
    result = inst(l2, [l1_inst], target=targ_inst)
    assert result.value == 16
    assert get_val(targ_inst, "y").value == 8
    assert not defines(targ_inst, "x")

    # target's defs are overwritten only if the linklet has a definition
    # not with an imported variable
    l1 = make_linklet("(linklet () (x) (define-values (x) 4))")
    l2 = make_linklet("(linklet ((x)) () (+ x x))")    
    targ2 = make_linklet("(linklet () () (define-values (x) 1000))")
    targ2_inst = inst(targ2)
    result = inst(l2, [l1_inst], target=targ2_inst)
    assert result.value == 8
    assert get_val(targ2_inst, "x").value == 1000

    ## same thing with the import renaming
    l1 = make_linklet("(linklet () (x) (define-values (x) 4))")
    l3 = make_linklet("(linklet (((x x2))) () (+ x2 x2))")
    targ3 = make_linklet("(linklet () () (define-values (x) 1000) (define-values (x2) 2000))")
    targ3_inst = inst(targ3)
    result = inst(l3, [l1_inst], target=targ3_inst)
    assert result.value == 8
    assert get_val(targ3_inst, "x").value == 1000
    assert get_val(targ3_inst, "x2").value == 2000

    ## slightly trickier
    l1 = make_linklet("(linklet () (x) (define-values (x) 4))")
    l3 = make_linklet("(linklet (((x x2))) () (define-values (x) 14) (+ x2 x))")
    targ3 = make_linklet("(linklet () () (define-values (x) 1000) (define-values (x2) 2000))")
    targ3_inst = inst(targ3)
    result = inst(l3, [l1_inst], target=targ3_inst)
    assert result.value == 18
    assert get_val(targ3_inst, "x").value == 1000
    assert get_val(targ3_inst, "x2").value == 2000

@pytest.mark.linkl
def test_instantiate_basic_export():
    l1 = make_linklet("(linklet () (a) (define-values (a) 4))")
    l2 = make_linklet("(linklet ((a)) () (+ a a))")
    targ = make_linklet("(linklet () ())")
    l1_inst = inst(l1)
    targ_inst = inst(targ)
    result = inst(l2, [l1_inst], target=targ_inst)
    assert result.value == 8

    # export-rename
    l1 = make_linklet("(linklet () ((a1 a)) (define-values (a1) 4))")
    l2 = make_linklet("(linklet ((a)) () (+ a a))")
    targ = make_linklet("(linklet () ())")
    l1_inst = inst(l1)
    targ_inst = inst(targ)
    result = inst(l2, [l1_inst], target=targ_inst)
    assert result.value == 8

@pytest.mark.linkl
def test_instantiate_uninitialize_undefined_exports():
    l1 = make_linklet("(linklet () (x))")
    targ = make_linklet("(linklet () ())")
    targ_inst = inst(targ)
    inst(l1, [], target=targ_inst)
    assert targ_inst.is_var_uninitialized(W_Symbol.make("x"))

    # don't touch if target has it
    targ2 = make_linklet("(linklet () () (define-values (x) 10))")
    targ2_inst = inst(targ2)
    inst(l1, [], target=targ2_inst)
    assert not targ2_inst.is_var_uninitialized(W_Symbol.make("x"))

    # target exports the same var with another external name
    l1 = make_linklet("(linklet () (x2) (+ x2 x2))")
    targ3 = make_linklet("(linklet () ((x x2)) (define-values (x) 10))")
    targ3_inst = inst(targ3)
    result = inst(l1, [], target=targ3_inst)
    assert result.value == 20
    assert get_val(targ3_inst, "x2").value == 10

@pytest.mark.linkl
def test_instantiate_export_rename():
    l1 = make_linklet("(linklet () ((x1 x)) (define-values (x1) 4))")
    l2 = make_linklet("(linklet ((x)) ((y1 y)) (define-values (y1) x) (+ x y1))")
    targ = make_linklet("(linklet () ())")

    l1_inst = inst(l1)
    assert get_val(l1_inst, "x").value == 4
    targ_inst = inst(targ)
    result = inst(l2, [l1_inst], target=targ_inst)
    assert result.value == 8
    assert get_val(targ_inst, "y").value == 4
    assert not defines(targ_inst, "x")

@pytest.mark.linkl
def test_instantiate_import_rename():
    l1 = make_linklet("(linklet () (x) (define-values (x) 4))")
    l2 = make_linklet("(linklet () (x) (define-values (x) 10))")
    l3 = make_linklet("(linklet (((x x1))((x x2))) () (+ x1 x2))")
    targ = make_linklet("(linklet () ())")

    l1_inst = inst(l1)
    l2_inst = inst(l2)    
    targ_inst = inst(targ)
    result = inst(l3, [l1_inst,l2_inst], target=targ_inst)
    assert result.value == 14

@pytest.mark.linkl
def test_instantiate_eval_define_values():
    l1 = make_linklet("(linklet () ((x x15)) (define-values (x) 4) (+ x x))")
    targ = make_linklet("(linklet () ((x x16)) (define-values (x) 1000))")

    targ_inst = inst(targ)
    l1_inst = inst(l1, [], target=targ_inst)
    assert defines(targ_inst, "x15") and defines(targ_inst, "x16")
    assert not defines(targ_inst, "x")
    assert get_val(targ_inst, "x15").value == 4
    assert get_val(targ_inst, "x16").value == 1000

@pytest.mark.linkl
def test_instantiate_target_def_stays_the_same():
    # use the local var, don't change target's var if you don't export
    l1 = make_linklet("(linklet () () (define-values (x) 4) (+ x x))")
    targ = make_linklet("(linklet () () (define-values (x) 10))")
    targ2 = make_linklet("(linklet () (x) (define-values (x) 10))")

    targ_inst = inst(targ)
    targ2_inst = inst(targ2)
    result = inst(l1, [], target=targ_inst)
    result2 = inst(l1, [], target=targ2_inst)
    assert result.value == 8 and result2.value == 8
    assert get_val(targ_inst, "x").value == 10
    assert get_val(targ2_inst, "x").value == 10

@pytest.mark.linkl
def test_instantiate_closures_and_variables():
    l1 = make_linklet("(linklet () (x) (define-values (x) 4))")
    l1_inst = inst(l1)
    l2 = make_linklet("(linklet ((x)) (g) (define-values (g) (lambda (y) x)))")
    targ = make_linklet("(linklet () ())")
    targ_inst = inst(targ)

    result = inst(l2, [l1_inst], target=targ_inst)
    assert defines(targ_inst, "g")
    assert not defines(targ_inst, "x")

    # use the modified target
    l3 = make_linklet("(linklet () (g) (g 5))")
    result2 = inst(l3, [], target=targ_inst)
    assert result2.value == 4

    # import the closure
    l4 = make_linklet("(linklet ((g)) () (g 3))")
    l2_inst = inst(l2, [l1_inst])
    targ2 = make_linklet("(linklet () ())")
    targ2_inst = inst(targ2)
    result3 = inst(l4, [l2_inst], target=targ2_inst)
    assert result3.value == 4

@pytest.mark.linklh
def test_instantiate_set_bang():
    # mutating an imported variable is a compilation error
    with pytest.raises(SchemeException) as e:
        make_linklet("(linklet ((x)) () (set! x 5) (+ x x))")
    assert "cannot mutate imported variable" in str(e.value)

    l1 = make_linklet("(linklet () () (define-values (x) 3) (set! x 5) (+ x x))")
    targ = make_linklet("(linklet () ())")
    targ_inst = inst(targ)
    result = inst(l1, [], target=targ_inst)
    assert defines(targ_inst, "x")
    assert get_val(targ_inst, "x").value == 5
    assert result.value == 10

    l2 = make_linklet("(linklet () (x) (set! x 5) (+ x x))")
    targ2 = make_linklet("(linklet () () (define-values (x) 3))")
    targ2_inst = inst(targ2)
    result2 = inst(l2, [], target=targ2_inst)
    assert get_val(targ2_inst, "x").value == 5
    assert result2.value == 10
