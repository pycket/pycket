
from pycket import config

if not config.hidden_classes:
    from pycket.impersonators.hidden_classes import *
else:
    from pycket.impersonators.baseline import *
