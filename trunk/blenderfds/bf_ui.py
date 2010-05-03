# ##### BEGIN GPL LICENSE BLOCK #####
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
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
from blenderfds import bf_export, bf_config, bf_geometry
from time import time

# Configuration

# UI: export menu

class FdsExporter(bpy.types.Operator):
    """Export Blender objects as FDS file."""
    bl_idname = "export.fds"
    bl_label = "Export as FDS file"
    
    path = StringProperty(name="File Path",
                          description="File path used for exporting the FDS file",
                          maxlen=1024)
                          
    filename = StringProperty(name="File Name",
                              description="Name of the file")
                              
    directory = StringProperty(name="Directory",
                               description="Directory of the file")
                               
    check_existing = BoolProperty(name="Check Existing",
                                  description="Check on overwriting existing files",
                                  default=True,
                                  options={'HIDDEN'})
                                  
    export_visible = BoolProperty(name="Visible Only",
                                  description="Export visible objects only",
                                  default=False)
    
    def execute(self, context):
        bf_export.export_fds(context,
                             self.properties.directory,
                             self.properties.filename,
                             self.properties.export_visible)
        return {'FINISHED'}

    def invoke(self, context, event):
        wm = context.manager
        wm.add_fileselect(self)
        return {'RUNNING_MODAL'}

# UI: edit mode tools panel

class View3DPanel(bpy.types.Panel):
    bl_space_type = 'VIEW_3D'
    bl_region_type = 'TOOLS'

class VIEW3D_PT_tools_meshedit_bf(View3DPanel):
    bl_context = "mesh_edit"
    bl_label = "FDS Tools"

    def draw(self, context):
        layout = self.layout

        col = layout.column(align=True)
        col.operator("mesh.select_non_manifold")

# UI: scene panel

class SceneButtonsPanel(bpy.types.Panel):
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "scene"

class SCENE_PT_bf(SceneButtonsPanel):
    '''Panel.'''
    bl_label = "FDS Case"
    
    def draw(self, context):
        layout = self.layout
        sc = context.scene

        # Check strings
        sc.name = bf_export.to_file_name(sc.name)
        sc.bf_case_title = bf_export.to_no_quotes(sc.bf_case_title)

        # Case name and title
        row = layout.row()
        row.prop(sc, "name", text="CHID")
        row = layout.row()
        row.prop(sc, "bf_case_title")
        
        # Case directory
        row = layout.row()
        row.prop(sc, "bf_case_dir")

        # Header
        row = layout.row()
        row.prop(sc, "bf_type_of_header", expand=True)
        if sc.bf_type_of_header == "file":
            row = layout.row()
            row.prop(sc, "bf_header_file_path")

        # Voxel size
        row = layout.row()
        row.label(text="Voxel Size:")
        row = layout.row()
        row.prop(sc, "bf_voxel_size", text="")        

# UI: object panel

class ObjectButtonsPanel(bpy.types.Panel):
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "object"
    
    def poll(self, context):
        ob = context.object
        return ob and ob.type == "MESH"

class OBJECT_PT_bf(ObjectButtonsPanel):
    '''Panel.'''
    bl_label = "FDS Object"

    def draw_header(self, context):
        layout = self.layout
        ob = context.object
        layout.prop(ob, "bf_nl_export", text="")
        self.bl_label = bf_config.get_nl_description(ob.bf_nl_name)

    def draw(self, context):
        layout = self.layout
        ob = context.object
        layout.enabled = ob.bf_nl_export
        
        # Init time
        t = int(time())
        
        # Check xb
        if ob.bf_xb == "voxels" and context.mode == "OBJECT" and not(bf_geometry.is_manifold(ob.data)):
            ob.bf_xb = "bbox"
            ob.bf_msg = "Object is not manifold."
            ob.bf_msg_timer = t
        
        # Check one only one multiple at a time
        if ob.bf_xb in ["voxels","faces","edges"]:
            if ob.bf_xyz == "verts":
                ob.bf_xyz = "none"
                ob.bf_msg = "XB and XYZ conflicting."
                ob.bf_msg_timer = t
            if ob.bf_pb  == "faces":
                ob.bf_pb = "none"
                ob.bf_msg = "XB and PB conflicting."
                ob.bf_msg_timer = t
        if ob.bf_xyz == "verts":
            if ob.bf_pb  == "faces":
                ob.bf_pb = "none"
                ob.bf_msg = "XYZ and PB conflicting."
                ob.bf_msg_timer = t

        # Check strings
        ob.name = bf_export.to_no_quotes(ob.name)
        ob.bf_nl_name = bf_export.to_no_quotes(ob.bf_nl_name)
        ob.bf_fyi = bf_export.to_no_quotes(ob.bf_fyi)
        ob.bf_custom_param = bf_export.to_single_quotes(ob.bf_custom_param)

        # Init parameter choice from config and name
        nl_params = bf_config.nl_params
        bf_nl_name = ob.bf_nl_name
        
        # Nl Name and FYI
        row = layout.row()
        row.prop(ob, "bf_nl_name")
        row = layout.row()
        row.prop(ob, "bf_fyi")

        # ID
        row = layout.row()
        row.prop(ob, "bf_id", text=bf_export.get_id(ob))

        # SURF_ID
        if bf_nl_name in nl_params["SURF_ID"] and ob.active_material: # must have a material, too!
            row = layout.row()
            row.prop(ob, "bf_surf_id", text=bf_export.get_surf_id(ob))
        else: ob.bf_surf_id = False  

        # SAWTOOTH
        if bf_nl_name in nl_params["SAWTOOTH"]:
            row = layout.row()
            row.prop(ob, "bf_sawtooth", text=bf_export.get_sawtooth(ob))
        else: ob.bf_sawtooth = False

        # IJK
        if bf_nl_name in nl_params["IJK"]:
            row = layout.row()
            if ob.bf_ijk:
                ijk = bf_export.get_ijk(ob)
                row.prop(ob, "bf_ijk", text=ijk[3]) # use dynamic text
                ob.bf_cell_size = ijk[2] # correct cell_size
                row = layout.row()
                row.label(text="Cell size:")
                row.operator("object.bf_set_compatible_voxel")
                row = layout.row()
                row.prop(ob, "bf_cell_size", expand=True, text="")
            else:
                row.prop(ob, "bf_ijk") 
        else: ob.bf_ijk = False
        
        # Geometry
        row = layout.row()
        col1, col2, col3 = row.column(), row.column(), row.column()
        
        # XB
        col1.label(text="XB:")
        col1.prop(ob, "bf_xb", text="")
        if not(bf_nl_name in nl_params["XB"]):
            ob.bf_xb = "none"
            col1.enabled = False
        
        # XYZ
        col2.label(text="XYZ:")
        col2.prop(ob, "bf_xyz", text="")
        if not(bf_nl_name in nl_params["XYZ"]):
            ob.bf_xyz = "none"
            col2.enabled = False  
        
        # PB
        col3.label(text="PB*:")
        col3.prop(ob, "bf_pb", text="")
        if not(bf_nl_name in nl_params["PB"]):
            ob.bf_pb = "none"
            col3.enabled = False

        # Error message
        if ob.bf_msg:
            if (t-ob.bf_msg_timer) < 2:
                row = layout.row()
                row.label(icon="ERROR", text=ob.bf_msg)
            else:
                ob.bf_msg = ""
            
        # Custom param
        col = layout.column()
        col.label(text="Custom parameters:")
        col.prop(ob, "bf_custom_param", text="")

# UI: material panel

class MaterialButtonsPanel(bpy.types.Panel):
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "material"
    
    def poll(self, context):
        ma = context.material
        return ma

class MATERIAL_PT_bf(MaterialButtonsPanel):
    '''Panel.'''
    bl_label = "FDS SURF (Boundary Condition)"
    
    def draw(self, context):
        layout = self.layout
        ma = context.material

        # Check strings
        ma.name = bf_export.to_no_quotes(ma.name)
        ma.bf_fyi = bf_export.to_no_quotes(ma.bf_fyi)
        ma.bf_custom_param = bf_export.to_single_quotes(ma.bf_custom_param)

        # FYI
        row = layout.row()
        row.prop(ma, "bf_fyi")
        
        # RGB
        row = layout.row()
        row.prop(ma, "bf_rgb")
        row.prop(ma, "diffuse_color", text="")
        
        # TRANSPARENCY
        row = layout.row()
        row.prop(ma, "bf_transparency")
        row.prop(ma, "alpha", text="")
        
        # Custom param and FYI
        col = layout.column()
        col.label(text="Custom parameters:")
        col.prop(ma, "bf_custom_param", text="")

# UI: operators
        
class OBJECT_OT_bf_set_compatible_voxel(bpy.types.Operator):
    bl_label = "Adapt Voxel"
    bl_idname = "object.bf_set_compatible_voxel"
    bl_description = "Set compatible voxel size in scene"

    def invoke(self, context, event):
        cell_size = bf_export.get_ijk(context.object)[2]
        context.scene.bf_voxel_size = (cell_size[0]/2, cell_size[1]/2, cell_size[2]/2)
        return{'FINISHED'}
        
