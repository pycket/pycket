import json
import subprocess
import os

fn = os.path.join(os.path.dirname(__file__), "expand_racket.rkt")

def expand(s):
    process = subprocess.Popen(
        "racket %s" % (fn, ),
        shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    (data, err) = process.communicate(s)
    if err:
        raise Exception("Racket produced an error")
    return json.loads(data)
    
