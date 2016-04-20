
from operator                 import eq
from rpython.rlib             import jit, objectmodel
from rpython.rlib.rarithmetic import r_int, r_uint, intmask

MASK_32 = r_uint(0xFFFFFFFF)

def mask(hash, shift):
    return (hash >> shift) & 0x01f

def bitpos(hash, shift):
    return (1 << mask(hash, shift)) & MASK_32

def bit_count(i):
    # TODO: See about implementing this via the POPCNT instruction on
    # supporting architectures
    assert isinstance(i, r_uint)
    i = i - ((i >> 1) & r_uint(0x55555555))
    i = (i & r_uint(0x33333333)) + ((i >> 2) & r_uint(0x33333333))
    return (((i + (i >> 4) & r_uint(0xF0F0F0F)) * r_uint(0x1010101)) & r_uint(0xffffffff)) >> 24

def validate_persistent_hash(ht):
    "NOT RPYTHON"
    root = ht._root
    assert (root is None and ht._cnt == 0) or (root is not None and ht._cnt == root._size)
    if root is not None:
        validate_nodes(root)

def validate_nodes(root):
    "NOT RPYTHON"
    subnodes = root._subnodes()
    entries  = root._entries()
    subnode_count = sum((node._size for node in subnodes))
    total = subnode_count + len(entries)
    assert root._size == total
    for node in subnodes:
        validate_nodes(node)

class Box(object):
    _attrs_ = ['_val']
    _settled_ = True

    def __init__(self):
        self._val = False

    @objectmodel.always_inline
    def add_leaf(self):
        self._val = True

    @objectmodel.always_inline
    def adjust_size(self, size):
        return size + int(self._val)

def make_persistent_hash_type(
        super   = object,
        base    = None,
        keytype = None,
        valtype = None,
        name    = "PersistentHashMap",
        hashfun = hash,
        equal   = eq):

    if base is None:
        base = super

    if keytype is None:
        keytype = super
    if valtype is None:
        valtype = super

    @objectmodel.always_inline
    def restrict_key_type(k):
        if keytype is not None:
            assert k is None or isinstance(k, keytype)
        return k

    @objectmodel.always_inline
    def restrict_val_type(v):
        if valtype is not None:
            assert v is None or isinstance(v, valtype)
        return v

    @objectmodel.always_inline
    def restrict_types(k, v):
        return restrict_key_type(k), restrict_val_type(v)

    class Missing(valtype):
        def __init__(self):
            pass

    MISSING = Missing()

    class INode(super):

        _attrs_ = ['_size']
        _immutable_fields_ = ['_size']
        _settled_ = True

        def __init__(self, size):
            self._size = size

        def assoc_inode(self, shift, hash_val, key, val, added_leaf):
            pass

        def without(self, shift, hash, key):
            pass

        def _get_item_node(self, index):
            pass

        def _validate_node(self):
            "NOT RPYTHON"
            pass

        def _entries(self):
            "NOT RPYTHON"
            pass

        def _subnodes(self):
            "NOT RPYTHON"
            pass

    INode.__name__ = "INode(%s)" % name

    class BitmapIndexedNode(INode):

        _attrs_ = ['_bitmap', '_array']
        _immutable_fields_ = ['_bitmap', '_array[*]']
        _settled_ = True

        def __init__(self, bitmap, array, size):
            INode.__init__(self, size)
            self._bitmap = bitmap
            self._array = array

        def _entries(self):
            "NOT RPYTHON"
            entries = []
            for x in range(0, len(self._array), 2):
                key_or_none = self._array[x]
                val_or_node = self._array[x + 1]
                if key_or_none is not None or val_or_node is None:
                    entries.append((key_or_none, val_or_node))
            return entries

        def _subnodes(self):
            "NOT RPYTHON"
            subnodes = []
            for x in range(0, len(self._array), 2):
                key_or_none = self._array[x]
                val_or_node = self._array[x + 1]
                if key_or_none is None and val_or_node is not None:
                    assert isinstance(val_or_node, INode)
                    subnodes.append(val_or_node)
            return subnodes

        @objectmodel.always_inline
        def _get_item_node(self, index):
            for x in range(0, len(self._array), 2):
                key_or_none = self._array[x]
                val_or_node = self._array[x + 1]
                if key_or_none is None and val_or_node is not None:
                    assert isinstance(val_or_node, INode)
                    size = val_or_node._size
                    if index < size:
                        return index, key_or_none, val_or_node
                    index -= size
                else:
                    if index == 0:
                        return -1, key_or_none, val_or_node
                    index -= 1
            assert False

        def index(self, bit):
            return bit_count(self._bitmap & (bit - 1))

        @jit.dont_look_inside
        def assoc_inode(self, shift, hash_val, key, val, added_leaf):
            bit = bitpos(hash_val, shift)
            idx = self.index(bit)

            if (self._bitmap & bit) != 0:
                key_or_null = self._array[2 * idx]
                val_or_node = self._array[2 * idx + 1]

                if key_or_null is None:
                    assert isinstance(val_or_node, INode)
                    n = val_or_node.assoc_inode(shift + 5, hash_val & MASK_32, key, val, added_leaf)
                    if n is val_or_node:
                        return self
                    new_array = clone_and_set(self._array, 2 * idx + 1, n)
                    newsize = added_leaf.adjust_size(self._size)
                    return BitmapIndexedNode(self._bitmap, new_array, newsize)

                if equal(key, key_or_null):
                    # Entry already exists for this key
                    if val is val_or_node:
                        return self
                    new_array = clone_and_set(self._array, 2 * idx + 1, val)
                    return BitmapIndexedNode(self._bitmap, new_array, self._size)

                added_leaf.add_leaf()
                subnode  = create_node(shift + 5, key_or_null, val_or_node, hash_val, key, val)
                new_array = clone_and_set2(self._array, 2 * idx, None, 2 * idx + 1, subnode)
                return BitmapIndexedNode(self._bitmap, new_array, self._size + 1)
            else:
                n = bit_count(self._bitmap)
                if n >= 16:
                    nodes = [None] * 32
                    jdx = mask(hash_val, shift)
                    nodes[jdx] = BitmapIndexedNode_EMPTY.assoc_inode(shift + 5, hash_val, key, val, added_leaf)
                    j = 0

                    for i in range(32):
                        if (self._bitmap >> i) & 1 != 0:
                            if self._array[j] is None:
                                nodes[i] = self._array[j + 1]
                            else:
                                nodes[i] = BitmapIndexedNode_EMPTY.assoc_inode(shift + 5, hashfun(self._array[j]),
                                                                   self._array[j], self._array[j + 1], added_leaf)
                            j += 2

                    newsize = added_leaf.adjust_size(self._size)
                    return ArrayNode(n + 1, nodes, newsize)
                else:
                    new_array = [None] * (2 * (n + 1))
                    list_copy(self._array, 0, new_array, 0, 2 * idx)
                    new_array[2 * idx] = key
                    added_leaf.add_leaf()
                    new_array[2 * idx + 1] = val
                    list_copy(self._array, 2 * idx, new_array, 2 * (idx + 1), 2 * (n - idx))
                    return BitmapIndexedNode(self._bitmap | bit, new_array, self._size + 1)

        @objectmodel.always_inline
        def find_step(self, shift, hash_val, key, not_found):
            bit = bitpos(hash_val, shift)
            if (self._bitmap & bit) == 0:
                return not_found
            idx = self.index(bit)
            key_or_null = self._array[2 * idx]
            val_or_node = self._array[2 * idx + 1]
            if key_or_null is None:
                return val_or_node
            if equal(key, key_or_null):
                return val_or_node
            return not_found

        @jit.dont_look_inside
        def without_inode(self, shift, hash, key):
            bit = bitpos(hash, shift)
            if self._bitmap & bit == 0:
                return self

            idx = self.index(bit)
            key_or_none = self._array[2 * idx]
            val_or_node = self._array[2 * idx + 1]

            if key_or_none is None:
                assert isinstance(val_or_node, INode)
                n = val_or_node.without_inode(shift + 5, hash, key)
                if n is val_or_node:
                    return self
                if n is not None:
                    new_array = clone_and_set(self._array, 2 * idx + 1, n)
                    return BitmapIndexedNode(self._bitmap, new_array, self._size - 1)

                if self._bitmap == bit:
                    return None

                new_array = remove_pair(self._array, idx)
                return BitmapIndexedNode(self._bitmap ^ bit, new_array, self._size - 1)

            if equal(key, key_or_none):
                new_array = remove_pair(self._array, idx)
                return BitmapIndexedNode(self._bitmap ^ bit, new_array, self._size - 1)

            return self

    BitmapIndexedNode.__name__ = "BitmapIndexedNode(%s)" % name
    BitmapIndexedNode_EMPTY = BitmapIndexedNode(r_uint(0), [], 0)

    class ArrayNode(INode):

        _attrs_ = ['_cnt', '_array']
        _immutable_fields_ = ['_cnt', '_array[*]']
        _settled_ = True

        def __init__(self, cnt, array, size):
            INode.__init__(self, size)
            self._cnt = cnt
            self._array = array

        def _entries(self):
            "NOT RPYTHON"
            return []

        def _subnodes(self):
            "NOT RPYTHON"
            return [node for node in self._array if node is not None]

        @objectmodel.always_inline
        def _get_item_node(self, index):
            for node in self._array:
                if node is None:
                    continue
                assert isinstance(node, INode)
                if index < node._size:
                    return index, None, node
                index -= node._size
            assert False

        @jit.dont_look_inside
        def assoc_inode(self, shift, hash_val, key, val, added_leaf):
            idx = mask(hash_val, shift)
            node = self._array[idx]
            if node is None:
                subnode = BitmapIndexedNode_EMPTY.assoc_inode(shift + 5, hash_val, key, val, added_leaf)
                cloned  = clone_and_set(self._array, idx, subnode)
                newsize = added_leaf.adjust_size(self._size)
                return ArrayNode(self._cnt + 1, cloned, newsize)

            assert isinstance(node, INode)
            n = node.assoc_inode(shift + 5, hash_val, key, val, added_leaf)
            if n is node:
                return self
            new_array = clone_and_set(self._array, idx, n)
            newsize = added_leaf.adjust_size(self._size)
            return ArrayNode(self._cnt, new_array, newsize)

        @jit.dont_look_inside
        def without_inode(self, shift, hash_val, key):
            idx = r_uint(mask(hash_val, shift))
            node = self._array[idx]
            if node is None:
                return self
            n = node.without_inode(shift + 5, hash_val, key)
            if n is node:
                return self
            if n is None:
                if self._cnt <= 8:  # shrink
                    return self.pack(idx)
                new_array = clone_and_set(self._array, idx, n)
                return ArrayNode(self._cnt - 1, new_array, self._size - 1)
            else:
                new_array = clone_and_set(self._array, idx, n)
                return ArrayNode(self._cnt, new_array, self._size - 1)

        def pack(self, idx):
            new_array = [None] * (2 * (self._cnt - 1))
            j = r_uint(1)
            bitmap = r_uint(0)

            i = r_uint(0)
            while i < idx:
                if self._array[i] is not None:
                    new_array[j] = self._array[i]
                    bitmap |= r_uint(1) << i
                    j += 2

                i += 1

            i = r_uint(idx) + 1
            while i < len(self._array):
                if self._array[i] is not None:
                    new_array[j] = self._array[i]
                    bitmap |= r_uint(1) << i
                    j += 2

                i += 1

            return BitmapIndexedNode(bitmap, new_array, self._size - 1)

        @objectmodel.always_inline
        def find_step(self, shift, hash_val, key, not_found):
            idx = mask(hash_val, shift)
            node = self._array[idx]
            if node is None:
                return not_found
            return node

    ArrayNode.__name__ = "ArrayNode(%s)" % name

    class HashCollisionNode(INode):

        _attrs_ = ['_hash', '_array']
        _immutable_fields_ = ['_hash', '_array[*]']
        _settled_ = True

        def __init__(self, hash, array, size):
            INode.__init__(self, size)
            self._hash = hash
            self._array = array

        def _entries(self):
            "NOT RPYTHON"
            entries = []
            for x in range(0, len(self._array), 2):
                key_or_nil = self._array[x]
                if key_or_nil is None:
                    continue
                val = self._array[x + 1]
                entries.append((key_or_nil, val))
            return entries

        def _subnodes(self):
            "NOT RPYTHON"
            return []

        @objectmodel.always_inline
        def _get_item_node(self, index):
            for x in range(0, len(self._array), 2):
                key_or_nil = self._array[x]
                if key_or_nil is None:
                    continue
                if index == 0:
                    val_or_node = self._array[x + 1]
                    return -1, key_or_nil, val_or_node
                index -= 1
            assert False

        @jit.dont_look_inside
        def assoc_inode(self, shift, hash_val, key, val, added_leaf):
            if hash_val == self._hash:
                count = len(self._array)
                idx = self.find_index(key)
                if idx != -1:
                    if self._array[idx + 1] == val:
                        return self;

                    new_array = clone_and_set(self._array, r_uint(idx + 1), val)
                    return HashCollisionNode(hash_val, new_array, self._size)

                new_array = [None] * (count + 2)
                list_copy(self._array, 0, new_array, 0, count)
                new_array[count] = key
                added_leaf.add_leaf()
                new_array[count + 1] = val
                return HashCollisionNode(self._hash, new_array, self._size + 1)
            new_array = [None, self]
            edit = bitpos(self._hash, shift)
            return BitmapIndexedNode(edit, new_array, self._size) \
                                    .assoc_inode(shift, hash_val, key, val, added_leaf)

        @objectmodel.always_inline
        def find_step(self, shift, hash_val, key, not_found):
            for x in range(0, len(self._array), 2):
                key_or_nil = self._array[x]
                if key_or_nil is not None and equal(key_or_nil, key):
                    return self._array[x + 1]
            return not_found

        def find_index(self, key):
            i = r_int(0)
            while i < len(self._array):
                if equal(key, self._array[i]):
                    return i

                i += 2

            return r_int(-1)

        @jit.dont_look_inside
        def without_inode(self, shift, hash, key):
            idx = self.find_index(key)
            if idx == -1:
                return self

            if len(self._array) == 1:
                return None

            new_array = remove_pair(self._array, r_uint(idx) / 2)
            return HashCollisionNode(self._hash, new_array, self._size - 1)

    HashCollisionNode.__name__ = "HashCollisionNode(%s)" % name

    class PersistentHashMap(base):

        _attrs_ = ['_cnt', '_root']
        _immutable_fields_ = ['_cnt', '_root']
        _settled_ = True

        def __init__(self, cnt, root):
            assert root is None or isinstance(root, INode)
            self._cnt = cnt
            self._root = root

        def __len__(self):
            return self._cnt

        def haskey(self, key):
            return self.val_at(key, MISSING) is not MISSING

        __contains__ = haskey

        def union(self, other):
            """
            Performs a right biased union via iterated insertion. This could be
            made faster at the cost of me figuring out how to actually implement
            a proper union operation.
            This skews the asymptotics a little since the implementation of
            iteration is O(n lg n) as is insertion.
            """
            assert isinstance(other, PersistentHashMap)
            if not self._cnt:
                return other
            if not other._cnt:
                return self
            count = self._cnt
            root  = self._root
            assert root is not None

            for key, val in other.iteritems():
                added_leaf = Box()
                hash = hashfun(key) & MASK_32
                root = root.assoc_inode(r_uint(0), hash, key, val, added_leaf)
                count = added_leaf.adjust_size(count)
            if root is self._root:
                return self
            return PersistentHashMap(count, root)

        __add__ = union

        def iteritems(self):
            for i in range(self._cnt):
                yield self.get_item(i)

        def __iter__(self):
            for i in range(self._cnt):
                yield self.get_item(i)[0]

        def keys(self):
            keys = [None] * self._cnt
            for i in range(self._cnt):
                keys[i] = self.get_item(i)[0]
            return keys

        def vals(self):
            vals = [None] * self._cnt
            for i in range(self._cnt):
                vals[i] = self.get_item(i)[1]
            return vals

        @jit.dont_look_inside
        def assoc(self, key, val):
            key = restrict_key_type(key)
            val = restrict_val_type(val)
            added_leaf = Box()

            root = BitmapIndexedNode_EMPTY if self._root is None else self._root
            hash = hashfun(key) & MASK_32

            new_root = root.assoc_inode(r_uint(0), hash, key, val, added_leaf)

            if new_root is self._root:
                return self

            newcnt = added_leaf.adjust_size(self._cnt)
            return PersistentHashMap(newcnt, new_root)

        def val_at(self, key, not_found):
            key = restrict_key_type(key)
            not_found = restrict_val_type(not_found)

            if self._root is None:
                return not_found

            hashval = hashfun(key) & MASK_32
            shift = r_uint(0)
            val_or_node = self._root
            while True:
                t = type(val_or_node)
                if t is BitmapIndexedNode:
                    val_or_node = val_or_node.find_step(shift, hashval, key, not_found)
                elif t is ArrayNode:
                    val_or_node = val_or_node.find_step(shift, hashval, key, not_found)
                elif t is HashCollisionNode:
                    val_or_node = val_or_node.find_step(shift, hashval, key, not_found)
                else:
                    return restrict_val_type(val_or_node)
                shift += 5

        @jit.dont_look_inside
        def without(self, key):
            key = restrict_key_type(key)
            if self._root is None:
                return self
            new_root = self._root.without_inode(0, hashfun(key) & MASK_32, key)
            if new_root is self._root:
                return self
            return PersistentHashMap(self._cnt - 1, new_root)

        @jit.dont_look_inside
        def without_many(self, keys):
            root  = self._root
            count = self._cnt
            if root is None:
                return self
            for key in keys:
                if root is None:
                    return PersistentHashMap.EMPTY
                key = restrict_key_type(key)
                new_root = root.without_inode(0, hashfun(key) & MASK_32, key)
                if new_root is not root:
                    root = new_root
                    count -= 1
            return PersistentHashMap(count, root)

        def get_item(self, index):
            return self._elidable_get_item(index)

        @jit.elidable
        def _elidable_get_item(self, index):
            if not (0 <= index < self._cnt):
                raise IndexError

            assert self._root is not None

            key_or_none = None
            val_or_node = self._root
            while index != -1:
                t = type(val_or_node)
                if t is BitmapIndexedNode:
                    index, key_or_none, val_or_node = val_or_node._get_item_node(index)
                elif t is ArrayNode:
                    index, key_or_none, val_or_node = val_or_node._get_item_node(index)
                elif t is HashCollisionNode:
                    index, key_or_none, val_or_node = val_or_node._get_item_node(index)
                else:
                    assert False

            assert key_or_none is not None
            assert not isinstance(val_or_node, INode)
            return restrict_types(key_or_none, val_or_node)

        @staticmethod
        def singleton(key, val=None):
            return PersistentHashMap.EMPTY.assoc(key, val)

        def make_copy(self):
            return PersistentHashMap(self._cnt, self._root)

    PersistentHashMap.__name__ = name
    PersistentHashMap.INode = INode
    PersistentHashMap.BitmapIndexedNode = BitmapIndexedNode
    PersistentHashMap.ArrayNode = ArrayNode
    PersistentHashMap.HashCollisionNode = HashCollisionNode

    PersistentHashMap.EMPTY = PersistentHashMap(0, None)

    def create_node(shift, key1, val1, key2hash, key2, val2):
        key1hash = hashfun(key1) & MASK_32
        if key1hash == key2hash:
            return HashCollisionNode(key1hash, [key1, val1, key2, val2], 2)
        added_leaf = Box()
        return BitmapIndexedNode_EMPTY.assoc_inode(shift, key1hash, key1, val1, added_leaf) \
                                      .assoc_inode(shift, key2hash, key2, val2, added_leaf)

    def list_copy(from_lst, from_loc, to_list, to_loc, count):
        from_loc = r_uint(from_loc)
        to_loc = r_uint(to_loc)
        count = r_uint(count)
        i = r_uint(0)
        while i < count:
            to_list[to_loc + i] = from_lst[from_loc+i]
            i += 1
        return to_list

    def clone_and_set(array, i, a):
        clone = [None] * len(array)

        idx = r_uint(0)
        while idx < len(array):
            clone[idx] = array[idx]
            idx += 1

        clone[i] = a
        return clone

    def clone_and_set2(array, i, a, j, b):
        clone = [None] * len(array)

        idx = r_uint(0)
        while idx < len(array):
            clone[idx] = array[idx]
            idx += 1

        clone[i] = a
        clone[j] = b
        return clone

    def remove_pair(array, i):
        new_array = [None] * (len(array) - 2)
        list_copy(array, 0, new_array, 0, 2 * i)
        list_copy(array, 2 * (i + 1), new_array, 2 * i, len(new_array) - (2 * i))
        return new_array

    return PersistentHashMap

