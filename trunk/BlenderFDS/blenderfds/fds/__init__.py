"""BlenderFDS, FDS entities"""
# use me like this: from blenderfds.fds import *

# Define FDS entities, then update them
# The order is important
from . import props, props_geometry, namelists, props_update

# Expose collections
from .props import bf_props
from .namelists import bf_namelists

# Public symbols
__all__ = ["bf_props", "bf_namelists"]
