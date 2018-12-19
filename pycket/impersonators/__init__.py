
from pycket import config

if config.hidden_classes:
    from pycket.impersonators.hidden_classes import *
else:
    from pycket.impersonators.baseline import *

