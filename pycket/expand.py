import json
import subprocess

def expand(s):
    process = subprocess.Popen(["/home/samth/bin/racket", "../expand_racket.rkt"], 
                               stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    (data, err) = process.communicate(s)
    return json.loads(data)
    
