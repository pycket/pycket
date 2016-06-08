
from pycket import config

# Conditionally define the contents of this module to use the right implementation
if config.hidden_classes:
    from pycket.impersonators_hidden_classes import *
else:
    from pycket.impersonators_baseline import *

