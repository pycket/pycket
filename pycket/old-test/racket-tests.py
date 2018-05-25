import pytest
import subprocess

def test_struct_test():
    return_code = subprocess.call("./racket-tests.sh pycket/test/struct-test.rkt", shell=True)  
    assert return_code == 0

@pytest.mark.xfail
def test_struct_test_object_name():
    return_code = subprocess.call("./racket-tests.sh pycket/test/struct-test-object-name.rkt", shell=True)  
    assert return_code == 0
