# -*- coding: utf-8 -*-
from rpython.rlib import jit


@jit.unroll_safe
def _splice(array, array_len, index, insertion, insertion_len):
    """
    We splice insertion into array at index:

    index = 1
    array = [a, b, c]
    array_len = 3
    insertion = [x, y]
    insertion_len = 2
    =>
    new_storage = [a, x, y, c]
    """
    new_len = array_len + insertion_len - 1
    assert new_len >= 0
    new_array = [None] * new_len

    for pre_index in range(index):
        new_array[pre_index] = array[pre_index]
    for insert_index in range(insertion_len):
        new_array[index + insert_index] = insertion[insert_index]
    for post_index in range(index + 1, array_len):
        new_array[post_index + insertion_len - 1] = array[post_index]

    return new_array


class _Tag(object):
    tags = {}
    _immutable_fields_ = ['name', '_arity', 'default_shape']

    def __init__(self, name, arity):
        # from lamb.expression import W_ConstructorCursor
        self.name = name
        assert arity >= 0
        self._arity = arity
        # self._cursor = W_ConstructorCursor(self)
        self.default_shape = default_shape(self, arity)

    def arity(self):
        return self._arity

    def instatiate(self, children):
        raise NotImplementedError("abstract method")
    #
    # Tags compare by identity, tests only.
    #
    def __eq__(self, other): #pragma: no cover
        "NOT_RPYTHON"
        return self is other

    @staticmethod
    def _reset_tags():
        "NOT_RPYTHON"
        _Tag.tags = {}

# def tag(name, arity, payload=None):
#     assert isinstance(name, str)
#     assert isinstance(arity, int)
#     tag_ = (name, arity, payload)
#     tag = _Tag.tags.get(tag_ , None)
#     if tag is None:
#         tag = _Tag(name, arity)
#         _Tag.tags[tag_] = tag

#     assert isinstance(tag, _Tag)
#     return tag

class StructTag(_Tag):

    _immutable_fields_ = ['_struct_type']
    def __init__(self, struct_type):
        name = struct_type.name.utf8value
        arity = struct_type.total_field_count
        _Tag.__init__(self, name, arity)
        self._struct_type = struct_type

    def struct_type(self):
        return self._struct_type

def struct_tag(name, arity, type=None):
    assert isinstance(name, str)
    assert isinstance(arity, int)
    tag_ = (name, arity, type)
    tag = _Tag.tags.get(tag_ , None)
    if tag is None:
        assert type is not None
        tag = StructTag(type)
        _Tag.tags[tag_] = tag

    assert isinstance(tag, _Tag)
    return tag

def get_struct_tag(type):
    name = type.name.utf8value
    arity = type.total_field_count
    return struct_tag(name, arity, type)

class Shape(object):

    _attrs_ = []

    def _init_children(self, w_c, children):
        pass

    def _update_child(self, new_children, children, index):
        pass

    def get_child(self, w_c, index):
        return self.extract_child(w_c, index)

    def get_children(self, w_c):
        raise NotImplementedError("abstract method")

    def get_number_of_direct_children(self):
        raise NotImplementedError("abstract method")

    def extract_child(self, w_c, index):
        raise NotImplementedError("abstract method")

    def record_shape(self, child, i):
        pass

    def record_shapes(self, storage):
        """NOT_RPYHON"""
        # test only
        for i, child in enumerate(storage):
            self.record_shape(child, i)

    def fusion(self, children):
        return (self, children)

    def instatiate(self, children):
        raise NotImplementedError("abstract method")

    #
    # Testing and Debug
    #
    def merge_point_string(self):
        return self.merge_point_string_seen([])

    def merge_point_string_seen(self, seen):
        return "(<some shape>)"



class ShapeConfig(object):

    def __init__(self,
                 substitution_threshold=17,
                 max_storage_width=8,
                 max_shape_depth=10):
        self.substitution_threshold = substitution_threshold
        self.max_storage_width = max_storage_width
        self.max_shape_depth = max_shape_depth
        self.log_transformations = False
        self._inhibit_recognition = False
        self._inhibit_all = False

class CompoundShape(Shape):

    _immutable_fields_ = ['_tag', '_structure[*]']

    _config = ShapeConfig()

    def __init__(self, tag, structure):
        self._structure = structure
        self._tag = tag
        self._hist = {}
        self.transformation_rules = {}

    @jit.unroll_safe
    def get_children(self, w_c):
        new_length = self.get_number_of_direct_children()
        return [self.get_child(w_c, index) for index in range(new_length)]


    def get_number_of_direct_children(self):
        return self._tag.arity()

    def extract_child(self, w_c, index):
        storage_index = self.structure_to_storage(index)
        subshape = self._structure[index]
        if subshape is in_storage_shape:
            return w_c._get_storage_at(storage_index)
        else:
            newlen = subshape.storage_width()
            endindex = storage_index + newlen
            assert endindex <= self.storage_width()
            new_storage = (w_c._get_storage())[storage_index:endindex]
            return subshape.build_child(new_storage)

    @jit.unroll_safe
    def structure_to_storage(self, index):
        offset = 0
        structure = self._structure
        for i in range(index):
            subshape = structure[i]
            offset += subshape.storage_width()
        return offset

    def get_storage(self, w_c):
        return w_c._get_storage()

    @jit.unroll_safe
    def storage_width(self):
        sum = 0
        for subshape in self._structure:
            sum += subshape.storage_width()
        return sum

    @jit.unroll_safe
    def shape_depth(self):
        depth = 0
        for subshape in self._structure:
            depth = max(subshape.shape_depth(), depth)
        return depth + 1

    def build_child(self, new_children):
        (shape, storage) = self.fusion(new_children)
        return shape.instantiate(storage)

    def replace(self, storage_index, new_shape):
        structure = self._structure[:]
        for i, child in enumerate(structure):
            if storage_index < child.storage_width():
                structure[i] = child.replace(storage_index, new_shape)
                return CompoundShape(self._tag, structure)
            storage_index -= child.storage_width()

    def record_shape(self, child, i):
        from pycket.values_struct import W_Struct
        if not isinstance(child, W_Struct):
            return
        shape = child.shape()
        key = (i, shape)
        count = self._hist[key] if key in self._hist else 0
        width = child._get_storage_width()
        depth = shape.shape_depth()
        if (key not in self.transformation_rules and
            depth < self._config.max_shape_depth and
            width <= self._config.max_storage_width and
            count <= self._config.substitution_threshold):
            # self._hist_keys.append(key)
            self._hist[key] = count + 1
            if self._hist[key] >= self._config.substitution_threshold:
                self.recognize_transformation(i, shape)


    def recognize_transformation(self, i, shape):
        new_shape = self.replace(i, shape)
        # self._transformation_rules_list.append((i, shape, new_shape))
        self.transformation_rules[i, shape] = new_shape
        if self._config.log_transformations:
            print "%s/%d\t(%d,%s)\n\t->%s" % (
                self._tag.name, self._tag.arity(),
                i, shape.merge_point_string(),
                new_shape.merge_point_string())

    def fusion(self, storage):

        if self._config._inhibit_all:
            return (self, storage)

        new_shape, new_storage = self.merge(storage)
        return (new_shape, new_storage)

    def instantiate(self, children):
        from pycket.values_struct import W_Struct
        return W_Struct.make_basic(children, self)


    #
    # shape merge/fusion
    #
    @jit.unroll_safe
    def merge(self, storage):
        u"""
        fusion ≔ Shape × [W_Object] → Shape' × [W_Object]'
        """
        from pycket.values_struct import W_Struct

        current_storage = storage
        index = 0
        shape = self
        storage_len = shape.storage_width()

        if storage_len < 1:
            # nothing to do
            return (self, storage)

        while index < storage_len:
            child = current_storage[index]
            subshape = child.shape()

            if not self._config._inhibit_recognition:
                # We do not record statistics in jitted code,
                # it should be stable beforehand
                if not jit.we_are_jitted():
                    shape.record_shape(child, index)

            new_shape = shape.get_transformation(index, subshape)
            if new_shape is not shape:
                # XXX [child] sometimes?
                assert isinstance(child, W_Struct)
                child_storage = child._get_storage()
                new_storage = _splice(current_storage, storage_len, index,
                                      child_storage, subshape.storage_width())

                current_storage = new_storage
                shape = new_shape
                storage_len = shape.storage_width()

                # rewind over new storage
                index = 0
            else:
                index += 1

        return (shape, current_storage)

    @jit.elidable
    def get_transformation(self, index, subshape):
        key = (index, subshape)
        if key not in self.transformation_rules:
            return self
        return self.transformation_rules[key]

    #
    # Testing and Debug
    #
    def merge_point_string_seen(self, seen):
        seen.append(self)
        res  = "(%s%d{" % (self._tag.name, self._tag.arity())
        first = True
        for subshape in self._structure:
            if first:
                first = False
            else:
                res += ", "
            res += subshape.merge_point_string_seen(seen) #if not subshape in seen else "."
        res += "})"
        return res

    def print_transforms(self):
        for (index, src), dest in self.transformation_rules.items():
            print "\t(%d,%s) -> %s" % (
                index, src.merge_point_string(), dest.merge_point_string())

    def print_hist(self):
        for (index, src), count in self._hist.items():
            print "\t%d: (%d,%s)" % (
                count, index, src.merge_point_string())

    @staticmethod
    def print_verbose():
        for shape in CompoundShape._shapes:
            print shape.merge_point_string()
            shape.print_hist()
            shape.print_transforms()

    def __eq__(self, other):
        return self is other or (
            self.__class__  == other.__class__ and
            self._tag       == other._tag and
            self._structure == other._structure)


class InStorageShape(Shape):

    def extract_child(self, w_c, index):
        return w_c._get_storage_at(index)

    def get_number_of_direct_children(self):
        return 0

    def storage_width(self):
        return 1

    def shape_depth(self):
        return 0

    def build_child(self, new_children):
        raise NotImplementedError()  #should not happen
        #return new_children[0]

    def get_storage(self, w_c):
        return [w_c]

    def replace(self, storage_index, new_shape):
        assert storage_index == 0
        return new_shape

    #
    # Testing and Debug
    #

    def merge_point_string_seen(self, seen):
        return "|"

in_storage_shape = InStorageShape()


def in_storage_shape_instance():
    return in_storage_shape


@jit.unroll_safe
def default_shape(tag, arity):
    shape = CompoundShape(tag, [in_storage_shape] * arity)
    return shape

class ShapeTuple(object):
    """
    I am a little bit like the python tuple but I can
    built up myself consecutively and still retain object identity.
    """

    _immutable_fields_ = ["shape", "parent"]

    def __init__(self, shape, parent):
        assert isinstance(shape, Shape) or shape is None
        self.shape = shape
        self.parent = parent
        self._route = {}

    @jit.elidable
    def tuple_for_shape(self, shape):
        tup = self._route.get(shape, None)
        if tup is None:
            tup = self.__class__(shape, self)
            self._route[shape] = tup
        return tup

    # #
    # # Testing and Debug
    # #
    # @uni
    # def to_repr(self, seen):
    #     return self.merge_point_string()


    def merge_point_string(self):
        res = ""
        if self.shape is None and self.parent is None:
            return res

        if self.parent is not None:
            res += self.parent.merge_point_string()
        if self.shape is not None:
            res += ".%s" % self.shape.merge_point_string()
        # else:
        #     res += "."
        return res

_empty_tuple = ShapeTuple(None, None)

@jit.unroll_safe
def find_shape_tuple(shape_list):
    tup = _empty_tuple
    for shape in shape_list:
        tup = tup.tuple_for_shape(jit.promote(shape))
    return tup
