"""BlenderFDS, Blender handlers"""

import bpy
from blenderfds.lib import fds_surf

@bpy.app.handlers.persistent
def load_post(self):
    """This function is run each time a Blender file is loaded"""
    # Init FDS default materials
    if not fds_surf.has_predefined(): bpy.ops.material.bf_set_predefined()
    # Init metric units
    for scene in bpy.data.scenes:
        scene.unit_settings.system = 'METRIC'

@bpy.app.handlers.persistent
def save_post(self):
    """This function is run each time a Blender file is saved"""
    # FIXME Use this to set BlenderFDS version on saved file
