import os

from rpython.translator.tool.cbuild import ExternalCompilationInfo

from pycket.rktio.types import *
from pycket.rktio._rktio_bootstrap import *

# Load the librktio.a
# TODO: make this absolute (pycket/rktio), instead of "this file"
RKTIO_DIR = os.path.dirname(os.path.abspath(__file__))
librktio_a = ExternalCompilationInfo(
    includes=['rktio.h'],
    include_dirs=[RKTIO_DIR],
    libraries=['rktio'],
    library_dirs=[RKTIO_DIR],
)


