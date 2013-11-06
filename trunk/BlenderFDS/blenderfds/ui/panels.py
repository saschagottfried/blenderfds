"""BlenderFDS, Blender panels"""

import bpy
from blenderfds.fds import * # get the reference to the BFNamelist collection
from blenderfds.lib import fds_surf

class SCENE_PT_BF(): # No bpy.types.Panel, so it's not registered
    bl_label = "FDS Scene"
    bl_space_type = 'PROPERTIES'
    bl_region_type = 'WINDOW'
    bl_context = "scene"

    def draw_header(self, context):
        layout = self.layout
        element = context.scene
        bf_namelist = bf_namelists[type(self).bf_namelist] # access Class variable and get self bf_namelist object
        self.bl_label = bf_namelist.draw_header(context, element, layout)

    def draw_extra(self, context, element, layout):
        pass

    def draw(self, context):
        layout = self.layout
        element = context.scene
        bf_namelist = bf_namelists[type(self).bf_namelist] # access Class variable and get self bf_namelist object
        bf_namelist.draw(context, element, layout)
        self.draw_extra(context, element, layout)
        
class SCENE_PT_BF1(SCENE_PT_BF, bpy.types.Panel):
    bf_namelist = "bf_head"

    def draw_extra(self, context, element, layout):
        row = layout.row()
        row.label(text="")
        row.operator("scene.bf_props_to_scene")

class SCENE_PT_BF2(SCENE_PT_BF, bpy.types.Panel):
    bf_namelist = "bf_time"

class SCENE_PT_BF3(SCENE_PT_BF, bpy.types.Panel):
    bf_namelist = "bf_dump"

class SCENE_PT_BF4(SCENE_PT_BF, bpy.types.Panel):
    bf_namelist = "bf_misc"

class SCENE_PT_BF5(SCENE_PT_BF, bpy.types.Panel):
    bf_namelist = "bf_reac"

class OBJECT_PT_BF(bpy.types.Panel):
    bl_label = "FDS Geometric Object"
    bl_idname = "OBJECT_PT_BF"
    bl_space_type = 'PROPERTIES'
    bl_region_type = 'WINDOW'
    bl_context = "object"
    
    @classmethod
    def poll(cls, context):
        ob = context.active_object
        return ob and ob.type in ("MESH", "EMPTY")

    def draw_header(self, context):
        layout = self.layout
        element = context.active_object
        self.bl_label = element.draw_header(context, element, layout)

    def draw(self, context):
        layout = self.layout
        element = context.active_object
        element.draw(context, element, layout)

class MATERIAL_PT_BF(bpy.types.Panel):
    bl_label = "FDS Boundary Condition"
    bl_idname = "MATERIAL_PT_BF"
    bl_space_type = 'PROPERTIES'
    bl_region_type = 'WINDOW'
    bl_context = "material"

    @classmethod
    def poll(cls,context):
        ma = context.material
        ob = context.active_object
        return ma and ob and ob.type == "MESH" and "bf_surf_id" in ob.descendants and not ob.bf_is_tmp

    def draw_header(self, context):
        layout = self.layout
        element = context.material
        # Header for normal element
        bf_namelist = bf_namelists[element.bf_namelist] # get self bf_namelist object from element
        self.bl_label = bf_namelist.draw_header(context, element, layout)
 
    def draw(self, context):
        layout = self.layout
        element = context.material
        # Panel for normal element
        # Static part of the panel
        split = layout.split(.7) # namelist
        split.prop(element, "bf_namelist", text="")
        row = split.row(align=True) # aspect
        row.prop(element, "diffuse_color", text="")
        row.prop(element, "alpha", text="")
        # Dynamic part of the panel
        bf_namelist = bf_namelists[element.bf_namelist] # get self bf_namelist object from element
        bf_namelist.draw(context, element, layout)
        # Static part of the panel
        row = layout.row()
        if fds_surf.has_predefined: row.label("")
        else: row.operator("material.bf_set_predefined", icon="WARNING")
        row.operator("material.bf_surf_to_sel_obs")        
