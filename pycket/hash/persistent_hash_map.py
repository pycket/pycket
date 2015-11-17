
from operator                 import eq
from rpython.rlib             import jit
from rpython.rlib.rarithmetic import r_int, r_uint, intmask

MASK_32 = r_uint(0xFFFFFFFF)

def make_persistent_hash_type(super=object, name="PersistentHashMap", hashfun=hash, equal=eq):

    class Box(object):
        def __init__(self):
            self._val = None

    class PersistentHashMap(super):

        _attrs_ = ['_cnt', '_root']
        _immutable_fields_ = ['_cnt', '_root']
        _settled_ = True

        def __init__(self, cnt, root):
            self._cnt = cnt
            self._root = root

        def __iter__(self):
            root = BitmapIndexedNode_EMPTY if self._root is None else self._root
            for item in root.iteritems():
                yield item

        def __len__(self):
            return self._cnt

        def iterkeys(self):
            for k, v in iter(self):
                yield k

        def itervals(self):
            for k, v in iter(self):
                yield v

        def assoc(self, key, val):
            added_leaf = Box()

            root = BitmapIndexedNode_EMPTY if self._root is None else self._root
            hash = hashfun(key) & MASK_32
            new_root = root.assoc_inode(r_uint(0), hash, key, val, added_leaf)

            if new_root is self._root:
                return self

            newcnt = self._cnt if added_leaf._val is None else self._cnt + 1
            return PersistentHashMap(newcnt, new_root)

        def val_at(self, key, not_found):
            if self._root is None:
                return not_found
            hashval = hashfun(key) & MASK_32
            result = self._root.find(r_uint(0), hashval, key, not_found)
            return result

        def without(self, key):
            if self._root is None:
                return self
            new_root = self._root.without_inode(0, hashfun(key) & MASK_32, key)
            if new_root is self._root:
                return self
            return PersistentHashMap(self._cnt - 1, new_root)

    PersistentHashMap.__name__ = name

    class INode(super):

        _attrs_ = []

        def iteritems(self):
            t = type(self)
            # XXX Hack for the type inferencer, since generator types cannot
            # be unified
            if t is BitmapIndexedNode:
                for item in self.iter():
                    yield item
            elif t is ArrayNode:
                for item in self.iter():
                    yield item
            elif t is HashCollisionNode:
                for item in self.iter():
                    yield item

        def assoc_inode(self, shift, hash_val, key, val, added_leaf):
            pass

        def find(self, shift, hash_val, key, not_found):
            pass

        def without(self, shift, hash, key):
            pass

    def mask(hash, shift):
        return (hash >> shift) & 0x01f

    def bitpos(hash, shift):
        return (1 << mask(hash, shift)) & MASK_32

    class BitmapIndexedNode(INode):

        _attrs_ = ['_edit', '_bitmap', '_array']
        _immutable_fields_ = ['_edit', '_bitmap', '_array[*]']

        def __init__(self, edit,  bitmap, array):
            self._edit = edit
            self._bitmap = bitmap
            self._array = array

        def iter(self):
            for x in range(0, len(self._array), 2):
                key_or_none = self._array[x]
                val_or_none = self._array[x + 1]
                if key_or_none is None and val_or_none is not None:
                    for x in val_or_none.iteritems():
                        yield x
                else:
                    yield key_or_none, val_or_none

        def index(self, bit):
            return bit_count(self._bitmap & (bit - 1))

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
                    return BitmapIndexedNode(None, self._bitmap, clone_and_set(self._array, 2 * idx + 1, n))


                if equal(key, key_or_null):
                    if val is val_or_node:
                        return self
                    return BitmapIndexedNode(None, self._bitmap, clone_and_set(self._array, 2 * idx + 1, val))

                added_leaf._val = added_leaf
                return BitmapIndexedNode(None, self._bitmap,
                    clone_and_set2(self._array,
                                   2 * idx, None,
                                   2 * idx + 1, create_node(shift + 5, key_or_null, val_or_node, hash_val, key, val)))
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

                    return ArrayNode(None, n + 1, nodes)
                else:
                    new_array = [None] * (2 * (n + 1))
                    list_copy(self._array, 0, new_array, 0, 2 * idx)
                    new_array[2 * idx] = key
                    added_leaf._val = added_leaf
                    new_array[2 * idx + 1] = val
                    list_copy(self._array, 2 * idx, new_array, 2 * (idx + 1), 2 * (n - idx))
                    return BitmapIndexedNode(None, self._bitmap | bit, new_array)

        @jit.elidable
        def find(self, shift, hash_val, key, not_found):
            bit = bitpos(hash_val, shift)
            if (self._bitmap & bit) == 0:
                return not_found
            idx = self.index(bit)
            key_or_null = self._array[2 * idx]
            val_or_node = self._array[2 * idx + 1]
            if key_or_null is None:
                return val_or_node.find(shift + 5, hash_val, key, not_found)
            if equal(key, key_or_null):
                return val_or_node
            return not_found

        def without_inode(self, shift, hash, key):
            bit = bitpos(hash, shift)
            if self._bitmap & bit == 0:
                return self

            idx = self.index(bit)
            key_or_none = self._array[2 * idx]
            val_or_node = self._array[2 * idx + 1]

            if key_or_none is None:
                n = val_or_node.without_inode(shift + 5, hash, key)
                if n is val_or_node:
                    return self
                if n is not None:
                    return BitmapIndexedNode(None, self._bitmap, clone_and_set(self._array, 2 * idx + 1, n))

                if self._bitmap == bit:
                    return None

                return BitmapIndexedNode(None, self._bitmap ^ bit, remove_pair(self._array, idx))

            if equal(key, key_or_none):
                return BitmapIndexedNode(None, self._bitmap ^ bit, remove_pair(self._array, idx))

            return self

    BitmapIndexedNode_EMPTY = BitmapIndexedNode(None, r_uint(0), [])

    class ArrayNode(INode):

        _attrs_ = ['_cnt', '_edit', '_array']
        _immutable_fields_ = ['_cnt', '_edit', '_array[*]']

        def __init__(self, edit, cnt, array):
            self._cnt = cnt
            self._edit = edit
            self._array = array

        def iter(self):
            for node in self._array:
                if node is None:
                    continue
                for x in node.iteritems():
                    yield x

        def assoc_inode(self, shift, hash_val, key, val, added_leaf):
            idx = mask(hash_val, shift)
            node = self._array[idx]
            if node is None:
                subnode = BitmapIndexedNode_EMPTY.assoc_inode(shift + 5, hash_val, key, val, added_leaf)
                cloned  = clone_and_set(self._array, idx, subnode)
                return ArrayNode(None, self._cnt + 1, cloned)

            n = node.assoc_inode(shift + 5, hash_val, key, val, added_leaf)
            if n is node:
                return self
            return ArrayNode(None, self._cnt, clone_and_set(self._array, idx, n))

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
                return ArrayNode(None, self._cnt - 1, clone_and_set(self._array, idx, n))
            else:
                return ArrayNode(None, self._cnt, clone_and_set(self._array, idx, n))

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

            return BitmapIndexedNode(None, bitmap, new_array)

        @jit.elidable
        def find(self, shift, hash_val, key, not_found):
            idx = mask(hash_val, shift)
            node = self._array[idx]
            if node is None:
                return not_found
            return node.find(shift + 5, hash_val, key, not_found)

    class HashCollisionNode(INode):

        _attrs_ = ['_hash', '_edit', '_array']
        _immutable_fields_ = ['_hash', '_edit', '_array[*]']

        def __init__(self, edit, hash, array):
            self._hash = hash
            self._edit = edit
            self._array = array

        def iter(self):
            for x in range(0, len(self._array), 2):
                key_or_nil = self._array[x]
                if key_or_nil is None:
                    continue
                val = self._array[x + 1]
                yield key_or_nil, val

        def assoc_inode(self, shift, hash_val, key, val, added_leaf):
            if hash_val == self._hash:
                count = len(self._array)
                idx = self.find_index(key)
                if idx != -1:
                    if self._array[idx + 1] == val:
                        return self;
                    return HashCollisionNode(None, hash_val, clone_and_set(self._array, r_uint(idx + 1), val))

                new_array = [None] * (count + 2)
                list_copy(self._array, 0, new_array, 0, count)
                new_array[count] = key
                added_leaf._val = added_leaf
                new_array[count + 1] = val
                return HashCollisionNode(self._edit, self._hash, new_array)
            return BitmapIndexedNode(None, bitpos(self._hash, shift), [None, self]) \
                                    .assoc_inode(shift, hash_val, key, val, added_leaf)

        @jit.elidable
        def find(self, shift, hash_val, key, not_found):
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

        def without_inode(self, shift, hash, key):
            idx = self.find_index(key)
            if idx == -1:
                return self

            if len(self._array) == 1:
                return None

            return HashCollisionNode(None, self._hash, remove_pair(self._array, r_uint(idx) / 2))

    def create_node(shift, key1, val1, key2hash, key2, val2):
        key1hash = hashfun(key1) & MASK_32
        if key1hash == key2hash:
            return HashCollisionNode(None, key1hash, [key1, val1, key2, val2])
        added_leaf = Box()
        return BitmapIndexedNode_EMPTY.assoc_inode(shift, key1hash, key1, val1, added_leaf) \
                                      .assoc_inode(shift, key2hash, key2, val2, added_leaf)

    def bit_count(i):
        # TODO: See about implementing this via the POPCNT instruction on
        # supporting architectures
        assert isinstance(i, r_uint)
        i = i - ((i >> 1) & r_uint(0x55555555))
        i = (i & r_uint(0x33333333)) + ((i >> 2) & r_uint(0x33333333))
        return (((i + (i >> 4) & r_uint(0xF0F0F0F)) * r_uint(0x1010101)) & r_uint(0xffffffff)) >> 24

    @jit.unroll_safe
    def list_copy(from_lst, from_loc, to_list, to_loc, count):
        from_loc = r_uint(from_loc)
        to_loc = r_uint(to_loc)
        count = r_uint(count)

        i = r_uint(0)
        while i < count:
            to_list[to_loc + i] = from_lst[from_loc+i]
            i += 1
        return to_list

    @jit.unroll_safe
    def clone_and_set(array, i, a):
        clone = [None] * len(array)

        idx = r_uint(0)
        while idx < len(array):
            clone[idx] = array[idx]
            idx += 1

        clone[i] = a
        return clone

    @jit.unroll_safe
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

    PersistentHashMap.EMPTY = PersistentHashMap(0, None)

    return PersistentHashMap

def test_persistent_hash():
    hash_cls = make_persistent_hash_type(
            super=object,
            name="test-hash-table",
            hashfun=hash,
            equalfun=op.eq)

    empty = hash_cls(0, None)
    acc = empty
    for i in range(100000):
        acc = acc.assoc(i % 1000, (i + 1) % 1000)
    print len(acc)

if __name__ == '__main__':
    test_persistent_hash()
