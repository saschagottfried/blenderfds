# ##### BEGIN GPL LICENSE BLOCK #####
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 3
#  of the License, or (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software Foundation,
#  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# ##### END GPL LICENSE BLOCK #####
"""BlenderFDS, an open tool for the NIST Fire Dynamics Simulator"""

import bpy
from . import bf_config

@bpy.app.handlers.persistent
def load_handler(self):
    """This function is run each time a Blender file is loaded"""
    # Init FDS default materials
    if not set(bf_config.predefined_material_names) <= set(bpy.data.materials.keys()):
        bpy.ops.material.bf_set_predefined()

@bpy.app.handlers.persistent
def save_handler(self):
    """This function is run each time a Blender file is saved"""
    # FIXME Use this to set BlenderFDS version
