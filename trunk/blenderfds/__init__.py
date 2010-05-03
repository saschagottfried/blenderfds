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
"""BlenderFDS, an open tool for the NIST Fire Dynamics Simulator."""
    
bl_addon_info = {
    "name": "BlenderFDS",
    "author": "Emanuele Gissi",
    "blender": (2, 5, 2),
    "category": "Export",
    "location": "File > Export > FDS Case (.fds)",
    "description": "BlenderFDS, an open tool for the NIST Fire Dynamics Simulator",
    "url": "http://code.google.com/p/blender-fds/",
    "category": "Export",
}

# FIXME Check scene name

import bpy

def menu_export(self, context):
    from blenderfds import bf_ui
    sc = context.scene
    default_path = bpy.data.filename.replace(".blend", ".fds")
    if sc.name: default_path = bpy.app.home + "/" + sc.name + ".fds"
    if sc.bf_case_dir: default_path = sc.bf_case_dir + default_path
    self.layout.operator(bf_ui.FdsExporter.bl_idname, text="FDS Case (.fds)").path = default_path

def register():
    from blenderfds import bf_ui
    
    # scene properties

    bpy.types.Scene.StringProperty(attr="bf_case_dir",
                                   name="Case Dir",
                                   subtype="FILE_PATH",
                                   description="FDS case directory") # FIXME DIRECTORY_PATH
   
    bpy.types.Scene.StringProperty(attr="bf_case_title",
                                   name="TITLE",
                                   maxlen=32,
                                   description="Case title")
    
    bpy.types.Scene.FloatVectorProperty(attr="bf_voxel_size",
                                        name="Voxel Size",
                                        description="Voxel size",
                                        step=1, precision=3,
                                        subtype="XYZ",
                                        default=(.20, .20, .20), min=.001, max=10.)

    items_list = [("auto", "Auto Header", "Automatic header"),
                  ("file", "File", "Custom header file",)]
    bpy.types.Scene.EnumProperty(attr="bf_type_of_header",
                                 name="Type of Header",
                                 items=items_list,
                                 description="Type of header",
                                 default="automatic")
                                 
    bpy.types.Scene.StringProperty(attr="bf_header_file_path",
                                   name="Header File",
                                   subtype="FILE_PATH",
                                   description="Header file",
                                   default="header.fds")
                                   
    # object properties
    
    bpy.types.Object.BoolProperty(attr="bf_nl_export",
                                  name="Export",
                                  description="Export object to FDS",
                                  default=False)
   
    bpy.types.Object.StringProperty(attr="bf_nl_name",
                                    name="& Name",
                                    description="Namelist name",
                                    default="OBST")

    bpy.types.Object.BoolProperty(attr="bf_id",
                                  name="ID",
                                  description="Set ID parameter",
                                  default=True)
                                  
    items_list = [("none",   "None",   "None"),
                  ("voxels", "Voxels", "Voxelized solid"),
                  ("bbox",   "BBox",   "Bounding box"),
                  ("faces",  "Faces",  "Faces, one for each face of this object"),
                  ("edges",  "Edges",  "Segments, one for each edge of this object")]
    bpy.types.Object.EnumProperty(attr="bf_xb",
                                  name="XB",
                                  items=items_list,
                                  description="Set XB parameter",
                                  default="none")
    
    items_list = [("none",   "None",   "None"),
                  ("center", "Center", "Point, corresponding to center point of this object"),
                  ("verts",  "Verts",  "Points, one for each vertex of this object"),]
    bpy.types.Object.EnumProperty(attr="bf_xyz",
                                  name="XYZ",
                                  items=items_list,
                                  description="Set XYZ parameter",
                                  default="none")
    
    items_list = [("none",   "None",   "None"),
                  ("faces",  "Faces",  "Planes, one for each face of this object"),]
    bpy.types.Object.EnumProperty(attr="bf_pb",
                                  name="PB*",
                                  items=items_list,
                                  description="Set PBX, PBY, PBZ parameter",
                                  default="none")

    bpy.types.Object.FloatVectorProperty(attr="bf_cell_size",
                                         name="Cell Size",
                                         description="Cell size",
                                         step=1, precision=3,
                                         subtype="XYZ",
                                         default=(.20, .20, .20), min=.001, max=10.)
    
    bpy.types.Object.BoolProperty(attr="bf_surf_id",
                                  name="SURF_ID",
                                  description="Set SURF_ID parameter to corresponding Blender material",
                                  default=False)
    
    bpy.types.Object.BoolProperty(attr="bf_sawtooth",
                                  name="SAWTOOTH",
                                  description="Set SAWTOOTH=.FALSE. parameter",
                                  default=False)
    
    bpy.types.Object.BoolProperty(attr="bf_ijk",
                                  name="IJK",
                                  description="Set IJK parameter optimized for voxel size",
                                  default=False)
    
    bpy.types.Object.StringProperty(attr="bf_fyi",
                                    name="FIY",
                                    maxlen=32,
                                    description="Set FIY parameter")
    
    bpy.types.Object.StringProperty(attr="bf_custom_param",
                                    name="Custom Parameters",
                                    description="Set custom parameters (exported verbatim)")
    
    bpy.types.Object.StringProperty(attr="bf_msg", name="Msg") # error message
    bpy.types.Object.IntProperty(attr="bf_msg_timer", name="Timer") # timer for error message
        
    # material properties
    
    bpy.types.Material.BoolProperty(attr="bf_rgb",
                                    name="RGB",
                                    description="Set RGB parameter to corresponding Blender material diffuse color",
                                    default=True)

    bpy.types.Material.BoolProperty(attr="bf_transparency",
                                    name="TRANSPARENCY",
                                    description="Set TRANSPARENCY parameter to corresponding Blender material alpha",
                                    default=False)
    
    bpy.types.Material.StringProperty(attr="bf_fyi",
                                      name="FIY",
                                      maxlen=32,
                                      description="Set FIY parameter")
                                       
    bpy.types.Material.StringProperty(attr="bf_custom_param",
                                      name="Custom Parameters",
                                      description="Set custom parameters (exported verbatim)")
     
    # panels
    bpy.types.register(bf_ui.SCENE_PT_bf)
    bpy.types.register(bf_ui.OBJECT_PT_bf)
    bpy.types.register(bf_ui.MATERIAL_PT_bf)
    
    # export menu
    bpy.types.register(bf_ui.FdsExporter)
    bpy.types.INFO_MT_file_export.prepend(menu_export)
    
    # tools
    bpy.types.register(bf_ui.VIEW3D_PT_tools_meshedit_bf)
    
    # ops
    bpy.types.register(bf_ui.OBJECT_OT_bf_set_compatible_voxel)

def unregister():
    from blenderfds import bf_ui
    
    # panels
    bpy.types.unregister(bf_ui.SCENE_PT_bf)
    bpy.types.unregister(bf_ui.OBJECT_PT_bf)
    bpy.types.unregister(bf_ui.MATERIAL_PT_bf)

    # export menu
    bpy.types.unregister(bf_ui.FdsExporter)
    bpy.types.INFO_MT_file_export.remove(menu_export)

    # tools
    bpy.types.unregister(bf_ui.VIEW3D_PT_tools_meshedit_bf)

    # ops
    bpy.types.unregister(bf_ui.OBJECT_OT_bf_set_compatible_voxel)
    
if __name__ == "__main__":
    register()
