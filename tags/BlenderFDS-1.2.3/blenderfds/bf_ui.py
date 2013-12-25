# ##### BEGIN GPL LICENSE BLOCK #####
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 3
#  of the License, or (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software Foundation,
#  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# ##### END GPL LICENSE BLOCK #####

import bpy
from bpy.props import *
from . import bf_config, bf_geometry
from time import time

# Check input data and file format

def contains_quotes(string):
    """Check if string contains quotes"""
    return "'" in string or '"' in string
    
def contains_even_single_quotes(string):
    """Check if string contains an even number of single quotes only"""
    return string.count("'") % 2 == 0 and ('"' not in string)

def detect_old_file_format(context, layout):
    """Detect old file format and propose convertion"""
    try:
        tmp = context.scene['bf_type_of_header']
        row = layout.row()
        row.operator("wm.bf_convert_to_new_file_format", icon="ERROR")
    except:
        pass
    return

def detect_predefined(context, layout):
    """Detect predefined entities and propose creation"""
    # Check predefined materials
    if not set(bf_config.mas_predefined) <= set(bpy.data.materials.keys()):
        row = layout.row()
        row.operator("material.bf_create_predefined", icon="ERROR")

# UI: scene panel

class SceneButtonsPanel():
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "scene"

class SCENE_PT_bf(SceneButtonsPanel, bpy.types.Panel):
    '''Panel.'''
    bl_label = "FDS Case"
    
    def draw(self, context):
        layout = self.layout
        sc = context.scene

        # Detect old file and predefined entities
        detect_old_file_format(context, layout)
        detect_predefined(context, layout)
        
        # Case name
        row = layout.row()
        row.prop(sc, "name", text="CHID")
        if sc.name != bpy.path.clean_name(sc.name): 
            row.label(icon="ERROR")
        
        # Case title
        row = layout.row()
        row.prop(sc, "bf_title",)
        if contains_quotes(sc.bf_title): 
            row.label(icon="ERROR")
        
        # Case directory
        row = layout.row()
        row.prop(sc, "bf_directory")

        # Configuration
        row = layout.row()
        row.prop(sc, "bf_config_type", expand=True)
        if sc.bf_config_type == "FILE":
            row = layout.row()
            row.prop(sc, "bf_config_filepath")

        # Voxel size
        row = layout.row()
        row.prop(sc, "bf_voxel_size")        

# UI: object panel

class ObjectButtonsPanel():
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "object"
    
    @classmethod
    def poll(cls, context):
        ob = context.object
        return ob and ob.type == "MESH"

class OBJECT_PT_bf(ObjectButtonsPanel, bpy.types.Panel):
    '''Panel.'''
    bl_label = "FDS Object"

    def draw_header(self, context):
        layout = self.layout
        ob = context.object
        
        # Export object to FDS
        layout.prop(ob, "bf_export", text="")
        
        # Object namelist description to panel title
        bf_nl = ob.bf_nl
        self.bl_label = "FDS {} ({})".format(bf_nl, bf_config.nls.get(bf_nl, "Unknown")) 

    def draw(self, context):
        layout = self.layout
        ob = context.object
        layout.active = ob.bf_export

        # Detect old file and predefined entities
        detect_old_file_format(context, layout)
        detect_predefined(context, layout)

        # Init parameter choice from config and name
        bf_nl = ob.bf_nl
        nl_params = bf_config.nl_params.get(bf_nl,"")
        
        # Namelist
        row = layout.row()
        row.prop(ob, "bf_nl")
        if not bf_nl in bf_config.nls:
            row.label(icon="ERROR")
                    
        # ID
        row = layout.row()
        row.active = "ID" in nl_params
        row.prop(ob, "name", text="ID")
        if contains_quotes(ob.name): 
            row.label(icon="ERROR")

        # FYI
        row = layout.row()
        row.prop(ob, "bf_fyi")
        if contains_quotes(ob.bf_fyi): 
            row.label(icon="ERROR")

        # Display as
        row = layout.row()
        row.prop(ob, "draw_type", text="Display as:")
        row.prop(ob, "show_transparent", text="Show transparency")
                   
        # SURF_ID
        if "SURF_ID" in nl_params and ob.active_material and ob.active_material.bf_export:
            row = layout.row()
            row.label(text="SURF_ID='" + ob.active_material.name + "'")
            
        # SAWTOOTH
        if "SAWTOOTH" in nl_params:
            row = layout.row()
            row.prop(ob, "bf_sawtooth", text='SAWTOOTH=.False.')

        # IJK
        if "IJK" in  nl_params:
            row = layout.row()
            if ob.bf_ijk:
                mesh_cells = bf_geometry.get_mesh_cells(ob)
                text = "IJK={0[0]},{0[1]},{0[2]} ({1} cells)".format(mesh_cells["ijk"],mesh_cells["quantity"])
                row.prop(ob, "bf_ijk", text=text)
            else:
                row.prop(ob, "bf_ijk")
            
            row = layout.row()
            row.active = ob.bf_ijk
            row.label(text="Desired Cell Size:")
            row.operator("object.bf_adapt_cell_voxel")
            row = layout.row()
            row.active = ob.bf_ijk
            row.prop(ob, "bf_cell_size", expand=True, text="")
        
        # Geometry
        row = layout.row()
        col1, col2, col3 = row.column(), row.column(), row.column()
        
        # XB
        col1.active = "XB" in nl_params
        col1.label(text="XB:")
        col1.prop(ob, "bf_xb", text="")
        
        # XYZ
        col2.active = "XYZ" in nl_params
        col2.label(text="XYZ:")
        col2.prop(ob, "bf_xyz", text="")
        
        # PB
        col3.active = "PB" in nl_params
        col3.label(text="PB*:")
        col3.prop(ob, "bf_pb", text="")

        # Visualize Boxels FIXME
        # if ob.bf_xb=="VOXELS":
        #    row = layout.row()
        #    row.operator("object.bf_visualize_boxels")

        # Visualize Voxels FIXME
        # if ob.bf_xb=="VOXELS":
        #    row = layout.row()
        #    row.operator("object.bf_visualize_voxels")

        # Check XB and manifold
        if ob.bf_xb == "VOXELS" and (not bf_geometry.is_manifold(ob.data)):
            row = layout.row()
            row.label(text="Not manifold", icon="ERROR")
            row.operator("object.bf_select_non_manifold")
        
        # Check one only one multiple at a time
        if ob.bf_xb in ["VOXELS","FACES","EDGES"]:
            if ob.bf_xyz == "VERTS":
                row = layout.row()
                row.label(text="XB and XYZ conflicting", icon="ERROR")

            if ob.bf_pb  == "FACES":
                row = layout.row()
                row.label(text="XB and PB* conflicting", icon="ERROR")

        if ob.bf_xyz == "VERTS":
            if ob.bf_pb  == "FACES":
                row = layout.row()
                row.label(text="XYZ and PB* conflicting", icon="ERROR")

        # Custom param
        col = layout.column()
        col.label(text="Custom " + bf_nl + " Parameters:")
        col.prop(ob, "bf_custom_param", text="")
        if not contains_even_single_quotes(ob.bf_custom_param): 
            col.label(text="Use Matched Single Quotes", icon="ERROR")

        # Copy active object FDS properties to other selected objects  
        row = layout.row()
        row.label()
        row.operator("object.bf_fds_props_to_sel_obs")
        
# UI: material panel

class MaterialButtonsPanel():
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "material"

    @classmethod    
    def poll(cls, context):
        ma = context.material
        ob = context.object
        return ma and ob and ob.type == "MESH" and "SURF_ID" in bf_config.nl_params.get(ob.bf_nl,"")

class MATERIAL_PT_bf(MaterialButtonsPanel, bpy.types.Panel):
    '''Panel.'''
    bl_label = "FDS Material"

    def draw_header(self, context):
        layout = self.layout
        ma = context.material
        
        # Export material to FDS
        layout.prop(ma, "bf_export", text="")
        
        # Object namelist description to panel title
        bf_nl = ma.bf_nl
        self.bl_label = "FDS {} ({})".format(bf_nl, bf_config.nls.get(bf_nl, "Unknown")) 

    def draw(self, context):
        layout = self.layout
        ma = context.material
        ob = context.object
        layout.active = ma.bf_export

        # Init parameter choice from config and name
        bf_nl = ma.bf_nl
        nl_params = bf_config.nl_params.get(bf_nl,"")

        # Detect old file and predefined entities
        detect_old_file_format(context, layout)
        detect_predefined(context, layout)

        # ID
        row = layout.row()
        row.active = "ID" in nl_params
        row.prop(ma, "name", text="ID")
        if contains_quotes(ma.name): 
            row.label(icon="ERROR")
        
        # FYI
        row = layout.row()
        row.prop(ma, "bf_fyi")
        if contains_quotes(ma.bf_fyi):
            row.label(icon="ERROR")

        # RGB
        row = layout.row()
        row.active = "RGB" in nl_params
        row.prop(ma, "diffuse_color", text="RGB")
        
        # TRANSPARENCY
        row = layout.row()
        row.active = "TRANSPARENCY" in nl_params
        row.prop(ma, "use_transparency", text="TRANSPARENCY")
        row.prop(ma, "alpha")
        
        # Custom param
        col = layout.column()
        col.label(text="Custom " + bf_nl + " Parameters:")
        col.prop(ma, "bf_custom_param", text="")
        if not contains_even_single_quotes(ma.bf_custom_param): 
            col.label(text="Use Matched Single Quotes.", icon="ERROR")
            
