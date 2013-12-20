
import pytest
from pycket.json import loads

def compare(json, expected):
    result = loads(json)
    assert result == expected

def test_simple():
    compare("1", 1)
    compare("\"abc\"", "abc")
    compare("1.2", 1.2)

def test_array():
    compare("[]", [])
    compare("[1]", [1])
    compare("[1, 2.0, 3.0, \"abc\", [10.0, \"def\"]]", [1, 2.0, 3.0, "abc", [10.0, "def"]])

def test_object():
    compare("{}", {})
    compare("{\"a\": 1}", {"a": 1})
    compare("{\"a\": 1, \"123\": \"ab\", \"subobj\": {\"d\": 12.0}, \"subarr\": [1]}", {"a": 1, "123": "ab", "subobj": {"d": 12.0}, "subarr": [1]})
