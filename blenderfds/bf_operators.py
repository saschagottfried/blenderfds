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
from mathutils import Vector, geometry
from . import bf_config, bf_geometry
from random import random

def convert_to_new_file_format():
    '''Convert to new file format'''
    
    # Scenes
    for sc in bpy.data.scenes:
        print("BlenderFDS: scene:", sc.name)
        # bf_case_title -> bf_title
        try:
            sc.bf_title = sc['bf_case_title']
            print("BlenderFDS: converted bf_case_title -> bf_title")
            del sc['bf_case_title']
        except: pass
        # bf_case_dir -> bf_directory
        try:
            sc.bf_directory = sc['bf_case_dir']
            print("BlenderFDS: converted bf_case_dir -> bf_directory")
            del sc['bf_case_dir']
        except: pass
        # bf_type_of_header -> bf_config_type
        try:
            items_list = [("AUTO", "Auto Config", "Automatic basic configuration for non-geometric parameters"),
                  ("FILE", "Config File", "Use external configuration file to describe non-geometric parameters",)]
            sc.bf_config_type = items_list[sc['bf_type_of_header']][0]
            print("BlenderFDS: converted bf_type_of_header -> bf_config_type")
            del sc['bf_type_of_header']
        except: pass
        # bf_header_file_path -> bf_config_filepath
        try:
            sc.bf_config_filepath = sc['bf_header_file_path']
            print("BlenderFDS: converted bf_header_file_path -> bf_config_filepath")
            del sc['bf_header_file_path']
        except: pass
        # bf_header_file -> none
        try:
            del sc['bf_header_file']
        except: pass
           
    # Objects
    for ob in bpy.data.objects:
        print("BlenderFDS: object:", ob.name)
        # bf_nl_export -> bf_export
        try:
            ob.bf_export = ob['bf_nl_export']
            print("BlenderFDS: converted bf_nl_export -> bf_export")
            del ob['bf_nl_export']
        except: pass
        # bf_nl_name -> bf_nl
        try:
            ob.bf_nl = ob['bf_nl_name']
            print("BlenderFDS: converted bf_nl_name -> bf_nl")
            del ob['bf_nl_name']
        except: pass
        # bf_surf_id -> active_material.bf_export
        try:
            ob.active_material.bf_export = ob['bf_surf_id']
            print("BlenderFDS: converted bf_surf_id -> active_material.bf_export")
            del ob['bf_surf_id']
        except: pass
        # bf_msg -> none
        try:
            del ob['bf_msg']
        except: pass
        # bf_msg_timer -> none
        try:
            del ob['bf_msg_timer']
        except: pass
                                    
    # Materials
    for ma in bpy.data.materials:
        print("BlenderFDS: material:", ma.name)        
        # bf_transparency -> use_transparency
        try:
            ma.use_transparency = ma['bf_transparency']
            print("BlenderFDS: converted bf_transparency -> use_transparency")
            del ma['bf_transparency']
        except: pass    

class WM_OT_bf_convert_to_new_file_format(bpy.types.Operator):
    bl_label = "Convert to new file format"
    bl_idname = "wm.bf_convert_to_new_file_format"
    bl_description = "Convert current file to new BlenderFDS file format"

    def execute(self, context):
        convert_to_new_file_format()
        return{'FINISHED'}

class OBJECT_OT_bf_select_non_manifold(bpy.types.Operator):
    bl_label = "Select non manifold"
    bl_idname = "object.bf_select_non_manifold"
    bl_description = "Go to edit mode and select non manifold edges"

    def invoke(self, context, event):
        bpy.ops.object.mode_set(mode='EDIT', toggle=False)
        bpy.context.scene.tool_settings.mesh_select_mode = False, True, False # Vertices, edges, faces
        bpy.ops.mesh.remove_doubles()
        bpy.ops.mesh.select_all(action="DESELECT")
        bpy.ops.mesh.select_non_manifold()
        return{'FINISHED'}


class OBJECT_OT_bf_adapt_cell_voxel(bpy.types.Operator):
    bl_label = "Adapt Cell/Voxel"
    bl_idname = "object.bf_adapt_cell_voxel"
    bl_description = "Adapt voxel size to real cell size"

    def invoke(self, context, event):
        ob = context.object
        ob.bf_cell_size = bf_geometry.get_mesh_cells(ob)["cell_size"]
        context.scene.bf_voxel_size = (ob.bf_cell_size[0]/2., ob.bf_cell_size[1]/2., ob.bf_cell_size[2]/2.)
        return{'FINISHED'}

class OBJECT_OT_bf_copy_active_FDS_properties_to_sel_obs(bpy.types.Operator):
    bl_label = "To Selected Objects"
    bl_idname = "object.bf_fds_props_to_sel_obs"
    bl_description = "Copy these FDS properties to other selected objects"

    def invoke(self, context, event):
        ob1 = context.active_object
        obs = {ob for ob in context.selected_objects if ob.type == "MESH"}
        print("BlenderFDS: Copying active object FDS properties to other selected objects")
        for ob in obs:
            print("BlenderFDS:     ", ob1.name, "->", ob.name)
            # do not copy ID!
            ob.bf_export = ob1.bf_export
            ob.bf_nl = ob1.bf_nl
            ob.bf_xb = ob1.bf_xb
            ob.bf_xyz = ob1.bf_xyz
            ob.bf_pb = ob1.bf_pb
            ob.bf_cell_size = ob1.bf_cell_size
            ob.bf_sawtooth = ob1.bf_sawtooth
            ob.bf_ijk = ob1.bf_ijk
            ob.bf_fyi = ob1.bf_fyi
            ob.bf_custom_param = ob1.bf_custom_param
            ob.draw_type = ob1.draw_type
            # copy active material only if it exists
            if ob1.active_material:
                ob.active_material = ob1.active_material
        print("BlenderFDS: Copying completed.")
        return{'FINISHED'}
        
class OBJECT_OT_bf_visualize_boxels(bpy.types.Operator):
    bl_label = "Visualize Boxels"
    bl_idname = "object.bf_visualize_boxels"
    bl_description = "Visualize boxels"

    def execute(self, context):
        ob = context.active_object
        print("BlenderFDS: visualize boxels:", ob.name)
        # Check
        if ob.type != "MESH" and not is_manifold(ob.data):
            print("BlenderFDS: ob not mesh or not manifold:", ob.name)
            return{"CANCELLED"}
        # Boxelization
        solid = True
        voxel_size = context.scene.bf_voxel_size
        boxels = bf_geometry.get_boxels(ob, voxel_size, solid)
        # Deselect all objects
        bpy.ops.object.select_all(action='DESELECT')
        for index, box in enumerate(boxels):
            name = "{}_boxel{}".format(ob.name,index)
            me_new = bpy.data.meshes.new(name)
            edges = list()
            x0,x1,y0,y1,z0,z1 = box
            verts = ((x0,y0,z0),(x1,y0,z0),(x1,y1,z0),(x0,y1,z0),(x0,y0,z1),(x1,y0,z1),(x1,y1,z1),(x0,y1,z1))
            faces = ([0,3,2,1],[0,1,5,4],[0,4,7,3],[6,5,1,2],[6,2,3,7],[6,7,4,5])
            me_new.from_pydata(verts, edges, faces)
            # Create a new object, link the new mesh, link the object to the scene, same layers, select it
            ob_new = bpy.data.objects.new(name, me_new)
            bpy.context.scene.objects.link(ob_new)
            ob_new.layers = ob.layers
            ob_new.select = True
            # Assign a random material
            ma = bpy.data.materials.new(name)
            ma.diffuse_color = (random(),random(),random())
            ob_new.active_material = ma
            # Group them
        bpy.ops.group.create(name="Boxels")
        print("BlenderFDS: visualize boxels done.")
        return{'FINISHED'} 

class OBJECT_OT_bf_visualize_voxels(bpy.types.Operator):
    bl_label = "Visualize Voxels"
    bl_idname = "object.bf_visualize_voxels"
    bl_description = "Visualize voxels"

    def execute(self, context):
        ob = context.active_object
        print("BlenderFDS: visualize voxels:", ob.name)
        # Check
        if ob.type != "MESH" and not is_manifold(ob.data):
            print("BlenderFDS: ob not mesh or not manifold:", ob.name)
            return{"CANCELLED"}
        # Voxelization
        solid = True
        voxel_size = context.scene.bf_voxel_size
        voxels = bf_geometry._voxelize(ob,voxel_size,solid)
        # Create a new mesh
        name = "{}_voxel".format(ob.name)
        me_new = bpy.data.meshes.new(name)
        # Prepare mesh data
        verts = list()
        edges = list()
        faces = list()
        i = 0
        x = 0.5 * Vector((voxel_size[0],0,0))
        y = 0.5 * Vector((0,voxel_size[1],0))
        z = 0.5 * Vector((0,0,voxel_size[2]))
        for voxel in voxels:
            c =  Vector((voxel[0] * voxel_size[0], voxel[1] * voxel_size[1], voxel[2] * voxel_size[2]))
            v0 = c-x-y-z
            v1 = c+x-y-z
            v2 = c+x+y-z
            v3 = c-x+y-z
            v4 = c-x-y+z
            v5 = c+x-y+z
            v6 = c+x+y+z
            v7 = c-x+y+z
            verts += (v0,v1,v2,v3,v4,v5,v6,v7)
            faces += ([0+i,3+i,2+i,1+i],[0+i,1+i,5+i,4+i],[0+i,4+i,7+i,3+i],[6+i,5+i,1+i,2+i],[6+i,2+i,3+i,7+i],[6+i,7+i,4+i,5+i])
            i += 8
        me_new.from_pydata(verts, edges, faces)
        # Create a new object, link the new mesh, link the object to the scene
        ob_new = bpy.data.objects.new(name, me_new)
        ob_new.layers = ob.layers
        scene = context.scene
        scene.objects.link(ob_new)
        scene.objects.active = ob_new
        print("BlenderFDS: visualize voxels done.")
        return{'FINISHED'} 

def create_material(name):
    '''Create material and return it'''
    if name not in bpy.data.materials:
        bpy.data.materials.new(name)
    return bpy.data.materials[name]
        
class MATERIAL_OT_bf_create_predefined(bpy.types.Operator):
    bl_label = "Create predefined SURFs"
    bl_idname = "material.bf_create_predefined"
    bl_description = "Create predefined SURF boundary conditions"

    def invoke(self, context, event):
        # Create OPEN
        ma = create_material('OPEN')
        ma.type = 'SURFACE'
        ma.diffuse_color = (.2,.8,.8)
        ma.use_transparency = True
        ma.alpha = .2
        ma.use_fake_user = True
        ma.bf_fyi = "This is a predefined SURF"
        
        # Create MIRROR
        ma = create_material('MIRROR')
        ma.type = 'SURFACE'
        ma.diffuse_color = (.2,.2,.8)
        ma.use_transparency = False
        ma.alpha = 1.
        ma.use_fake_user = True
        ma.bf_fyi = "This is a predefined SURF"

        # Create INERT
        ma = create_material('INERT')
        ma.type = 'SURFACE'
        ma.diffuse_color = (.8,.8,.2)
        ma.use_transparency = False
        ma.alpha = 1.
        ma.use_fake_user = True
        ma.bf_fyi = "This is a predefined SURF"

        return{'FINISHED'}
