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
from .bf_types import bf_namelists

### Scene panels

class SceneButtonsPanel():
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "scene"
    bl_label = "FDS Scene"
    nl = None

    def draw_header(self,context):
        layout = self.layout
        element = context.scene
        nl = type(self).nl # access Class variable
        self.bl_label = bf_namelists[nl].draw_header(context,element,layout)

    def draw(self,context):
        layout = self.layout
        element = context.scene
        nl = type(self).nl # access Class variable
        bf_namelists[nl].draw(context,element,layout)

class SCENE_PT_bf_HEAD(SceneButtonsPanel,bpy.types.Panel):
    nl = "HEAD"

class SCENE_PT_bf_TIME(SceneButtonsPanel,bpy.types.Panel):
    nl = "TIME"
    
class SCENE_PT_bf_MISC(SceneButtonsPanel,bpy.types.Panel):
    nl = "MISC"
    
class SCENE_PT_bf_REAC(SceneButtonsPanel,bpy.types.Panel):
    nl = "REAC"

class SCENE_PT_bf_DUMP(SceneButtonsPanel,bpy.types.Panel):
    nl = "DUMP"

### Object panels

class ObjectButtonsPanel():
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "object"
    
    @classmethod
    def poll(cls,context):
        ob = context.active_object
        return ob and ob.type == "MESH"

class OBJECT_PT_bf(ObjectButtonsPanel,bpy.types.Panel):
    bl_label = "FDS Object"

    def draw_header(self,context):
        layout = self.layout
        element = context.active_object
        nl = element.bf_namelist     
        self.bl_label = bf_namelists[nl].draw_header(context,element,layout)

    def draw(self,context):
        layout = self.layout
        element = context.active_object
        nl = element.bf_namelist
        bf_namelists[nl].draw(context,element,layout)

### Material panel

class MaterialButtonsPanel():
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "material"

    @classmethod    
    def poll(cls,context):
        ma = context.material
        ob = context.active_object
        return ma and ob and ob.type == "MESH" and "SURF_ID" in ob.get_bf_params() and not ob.bf_is_voxels

class MATERIAL_PT_bf(MaterialButtonsPanel,bpy.types.Panel):
    bl_label = "FDS Material"

    def draw_header(self,context):
        layout = self.layout
        element = context.material
        nl = element.bf_namelist
        self.bl_label = bf_namelists[nl].draw_header(context,element,layout)

    def draw(self,context):
        layout = self.layout
        element = context.material
        nl = element.bf_namelist
        bf_namelists[nl].draw(context,element,layout)
