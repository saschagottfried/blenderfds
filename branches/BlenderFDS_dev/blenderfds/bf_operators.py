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
"""BlenderFDS, operators"""

from . import bf_geometry
from .bf_osd import bf_osd
import bpy

# TODO clean up
import bgl
from bpy_extras.view3d_utils import region_2d_to_origin_3d, location_3d_to_region_2d
length = 0.0

class OBJECT_OT_bf_correct_ijk(bpy.types.Operator):
    bl_label = "Correct IJK"
    bl_idname = "object.bf_correct_ijk"
    bl_description = "Correct IJK for FDS Poisson solver"

    def execute(self,context):
        ob = context.active_object
        ob.bf_mesh_ijk = bf_geometry.get_good_ijk(ob)
        self.report({"INFO"}, "IJK corrected")
        return{'FINISHED'}

# FIXME loop on BProps except ID?
# FIXME create UI
class OBJECT_OT_bf_copy_active_FDS_properties_to_sel_obs(bpy.types.Operator):
    bl_label = "Copy To Selected Objects"
    bl_idname = "object.bf_fds_props_to_sel_obs"
    bl_description = "Copy these FDS properties to other selected objects"

    def execute(self,context):
        bpy.ops.object.mode_set(mode='OBJECT', toggle=False)
        ob_active = context.active_object
        obs = {ob for ob in context.selected_objects if ob.type == "MESH"}
        for ob in obs:
            # do not copy ID!
            ob.bf_export = ob_active.bf_export
            ob.bf_nl = ob_active.bf_nl
            ob.bf_xb = ob_active.bf_xb
            ob.bf_xyz = ob_active.bf_xyz
            ob.bf_pb = ob_active.bf_pb
            ob.bf_voxel_size = ob_active.bf_voxel_size
            ob.bf_sawtooth = ob_active.bf_sawtooth
            ob.bf_ijk_n = ob_active.bf_ijk_n
            ob.bf_fyi = ob_active.bf_fyi
            ob.bf_custom_param = ob_active.bf_custom_param
            ob.draw_type = ob_active.draw_type
            ob.show_transparent = ob_active.show_transparent
            ob.bf_is_voxels = ob_active.bf_is_voxels
            ob.bf_has_voxels_shown = ob_active.bf_has_voxels_shown
            # copy active material only if it exists
            if ob_active.active_material:
                ob.active_material = ob_active.active_material
        self.report({"INFO"}, "FDS properties copied")
        return{'FINISHED'}

def _create_material(name):
    '''Create material and return it'''
    if name not in bpy.data.materials:
        bpy.data.materials.new(name)
    return bpy.data.materials[name]

class MATERIAL_OT_bf_set_predefined(bpy.types.Operator):
    bl_label = "Set INERT, OPEN, MIRROR"
    bl_idname = "material.bf_set_predefined"
    bl_description = "Set INERT, OPEN, MIRROR predefined SURFs"

    def execute(self,context):
        # Set INERT
        if "INERT" not in bpy.data.materials.keys(): ma = _create_material('INERT')
        else: ma = bpy.data.materials['INERT']
        ma.type = 'SURFACE'
        ma.diffuse_color = (.8,.8,.2)
        ma.use_transparency = False
        ma.alpha = 1.
        ma.use_fake_user = True
        ma.bf_fyi = "This is a predefined SURF"
        ma.bf_namelist_export = True
        # Set OPEN
        if "OPEN" not in bpy.data.materials.keys(): ma = _create_material('OPEN')
        else: ma = bpy.data.materials['OPEN']
        ma.type = 'SURFACE'
        ma.diffuse_color = (.2,.8,.8)
        ma.use_transparency = True
        ma.alpha = .2
        ma.use_fake_user = True
        ma.bf_fyi = "This is a predefined SURF"
        ma.bf_namelist_export = True
        # Set MIRROR
        if "MIRROR" not in bpy.data.materials.keys(): ma = _create_material('MIRROR')
        else: ma = bpy.data.materials['MIRROR']
        ma.type = 'SURFACE'
        ma.diffuse_color = (.2,.2,.8)
        ma.use_transparency = False
        ma.alpha = 1.
        ma.use_fake_user = True
        ma.bf_fyi = "This is a predefined SURF"
        ma.bf_namelist_export = True
        # Info
        self.report({"INFO"}, "Predefined SURFs ok")
        return{'FINISHED'}

# FIXME currently not used as the new voxelization algorithm
# is much more robust towards non manifold objects
class OBJECT_OT_bf_select_non_manifold(bpy.types.Operator):
    bl_label = "Select Non-Manifold Edges"
    bl_idname = "object.bf_select_non_manifold"
    bl_description = "Go to edit mode and select non-manifold edges"

    def execute(self,context):
        bpy.ops.object.mode_set(mode='EDIT', toggle=False)
        context.scene.tool_settings.mesh_select_mode = False, True, False # Vertices, edges, faces
        bpy.ops.mesh.remove_doubles()
        bpy.ops.mesh.select_all(action="DESELECT")
        bpy.ops.mesh.select_non_manifold()
        self.report({"INFO"}, "Non-manifold edges selected")
        return{'FINISHED'}

class OBJECT_OT_bf_show_voxels(bpy.types.Operator):
    bl_label = "Show Voxels"
    bl_idname = "object.bf_show_voxels"
    bl_description = "Show object voxels"

    def execute(self, context):
        # Init
        sc = context.scene
        ob = context.active_object
        if ob.mode == 'EDIT':
            bpy.ops.object.mode_set(mode='OBJECT', toggle=False)
        ob.hide = True
        ob.bf_has_voxels_shown = True
        # Create voxels, put them in a new mesh
        edges, verts, faces, = tuple(), tuple(), tuple()
        xbs, tt, dimension_too_large = bf_geometry.get_voxels(context, ob)
        for i, xb in enumerate(xbs):
            x0,x1,y0,y1,z0,z1 = xb
            j = i * 8
            verts += (x0,y0,z0),(x1,y0,z0),(x1,y1,z0),(x0,y1,z0),(x0,y0,z1),(x1,y0,z1),(x1,y1,z1),(x0,y1,z1),
            faces += (0+j,3+j,2+j,1+j),(0+j,1+j,5+j,4+j),(0+j,4+j,7+j,3+j),(6+j,5+j,1+j,2+j),(6+j,2+j,3+j,7+j),(6+j,7+j,4+j,5+j),
        me_new = bpy.data.meshes.new("tmp_voxels")
        me_new.from_pydata(verts, edges, faces)
        # Create a new temporary voxel object, link new mesh
        ob_new = bpy.data.objects.new("_tmp_object", me_new)
        ob_new.bf_namelist = "TMP"
        ob_new.bf_is_voxels = True
        ob_new.active_material = ob.active_material
        ob_new.layers = ob.layers
        ob_new.show_wire = True
        sc.objects.link(ob_new)
        sc.update()
        if dimension_too_large: self.report({"WARNING"}, "Object size too large, voxel size not guaranteed")
        else: self.report({"INFO"}, "Object voxels shown")
        return{'FINISHED'}

class OBJECT_OT_bf_hide_voxels(bpy.types.Operator):
    bl_label = "Hide All Voxels"
    bl_idname = "object.bf_hide_voxels"
    bl_description = "Hide all object voxels"

    def execute(self, context):
        # Init
        sc = context.scene
        # Get temporary Blender objects and delete them
        for ob in {ob for ob in bpy.data.objects if ob.bf_is_voxels}:
            sc.objects.unlink(ob)
            bpy.data.objects.remove(ob)
        # Get hidden original objects and show them
        for ob in {ob for ob in bpy.data.objects if ob.bf_has_voxels_shown}:
            ob.bf_has_voxels_shown = False
            ob.hide = False
        self.report({"INFO"}, "All object voxels hidden")
        return{'FINISHED'}

def update_voxels(self, context):
    bf_osd.show("BlenderFDS: Updating voxels")
    ob = context.active_object
    if ob.bf_has_voxels_shown:
        bpy.ops.object.bf_hide_voxels()
        bpy.ops.object.bf_show_voxels()
    bf_osd.clean()

# TODO analyze and clean up

def bg_center_draw_callback_px(self, context):
    if self.center_pt is None:
        return
    # 50% alpha, 2 pixel width line
    bgl.glEnable(bgl.GL_BLEND)
    bgl.glColor4f(0.0, 0.8, 0.0, 1.0)
    bgl.glLineWidth(2)
    bgl.glPointSize(2.0)
    bgl.glEnable(bgl.GL_POINT_SMOOTH);
    bgl.glHint(bgl.GL_POINT_SMOOTH_HINT, bgl.GL_NICEST);

    #bgl.glLineStipple(1,0xAAAA)
    #bgl.glEnable(bgl.GL_LINE_STIPPLE)
    
    x, y = location_3d_to_region_2d(bpy.context.region,bpy.context.region_data,self.center_pt)
    bgl.glBegin(bgl.GL_POINTS)
    bgl.glVertex2i(int(x), int(y))
    bgl.glVertex2i(int(x)+5, int(y)+5)
    bgl.glVertex2i(int(x)+5, int(y)-5)
    bgl.glVertex2i(int(x)-5, int(y)+5)
    bgl.glVertex2i(int(x)-5, int(y)-5)
    bgl.glEnd()
    
    bgl.glBegin(bgl.GL_LINE_STRIP)
    bgl.glVertex2i(int(x)-5, int(y)-5)
    bgl.glVertex2i(int(x)+5, int(y)+5)
    bgl.glEnd()

    bgl.glBegin(bgl.GL_LINE_STRIP)
    bgl.glVertex2i(int(x)+5, int(y)-5)
    bgl.glVertex2i(int(x)-5, int(y)+5)
    bgl.glEnd()

    # restore opengl defaults
    bgl.glLineWidth(1)
    bgl.glDisable(bgl.GL_BLEND)
    bgl.glDisable(bgl.GL_POINT_SMOOTH);
    #bgl.glDisable(bgl.GL_LINE_STIPPLE)
    bgl.glColor4f(0.0, 0.0, 0.0, 1.0)
    
def bg_scale_draw_callback_px(self, context):
    if len(self.scaling_pts) == 0:
        return
    # 50% alpha, 2 pixel width line
    bgl.glEnable(bgl.GL_BLEND)
    bgl.glColor4f(0.0, 0.8, 0.0, 1.0)
    bgl.glLineWidth(2)
    bgl.glPointSize(2.0)
    bgl.glEnable(bgl.GL_POINT_SMOOTH);
    bgl.glHint(bgl.GL_POINT_SMOOTH_HINT, bgl.GL_NICEST);

    #bgl.glLineStipple(1,0xAAAA)
    #bgl.glEnable(bgl.GL_LINE_STIPPLE)
    
    x, y = location_3d_to_region_2d(bpy.context.region,bpy.context.region_data,self.scaling_pts[0])
    bgl.glBegin(bgl.GL_POINTS)
    bgl.glVertex2i(int(x), int(y))
    bgl.glVertex2i(int(x)+5, int(y)+5)
    bgl.glVertex2i(int(x)+5, int(y)-5)
    bgl.glVertex2i(int(x)-5, int(y)+5)
    bgl.glVertex2i(int(x)-5, int(y)-5)
    bgl.glEnd()
    
    bgl.glBegin(bgl.GL_LINE_STRIP)
    bgl.glVertex2i(int(x)-5, int(y)-5)
    bgl.glVertex2i(int(x)+5, int(y)+5)
    bgl.glEnd()

    bgl.glBegin(bgl.GL_LINE_STRIP)
    bgl.glVertex2i(int(x)+5, int(y)-5)
    bgl.glVertex2i(int(x)-5, int(y)+5)
    bgl.glEnd()
    
    bgl.glBegin(bgl.GL_LINE_STRIP)
    for pt in self.scaling_pts:
        x, y = location_3d_to_region_2d(bpy.context.region,bpy.context.region_data,pt)
        bgl.glVertex2i(int(x), int(y))

    bgl.glEnd()
    
    if len(self.scaling_pts)>1:
        x, y = location_3d_to_region_2d(bpy.context.region,bpy.context.region_data,self.scaling_pts[-1])

        bgl.glBegin(bgl.GL_POINTS)
        bgl.glVertex2i(int(x), int(y))
        bgl.glVertex2i(int(x)+5, int(y)+5)
        bgl.glVertex2i(int(x)+5, int(y)-5)
        bgl.glVertex2i(int(x)-5, int(y)+5)
        bgl.glVertex2i(int(x)-5, int(y)-5)
        bgl.glEnd()
    
        bgl.glBegin(bgl.GL_LINE_STRIP)
        bgl.glVertex2i(int(x)-5, int(y)-5)
        bgl.glVertex2i(int(x)+5, int(y)+5)
        bgl.glEnd()
    
        bgl.glBegin(bgl.GL_LINE_STRIP)
        bgl.glVertex2i(int(x)+5, int(y)-5)
        bgl.glVertex2i(int(x)-5, int(y)+5)
        bgl.glEnd()

    # restore opengl defaults
    bgl.glLineWidth(1)
    bgl.glDisable(bgl.GL_BLEND)
    bgl.glDisable(bgl.GL_POINT_SMOOTH);
    #bgl.glDisable(bgl.GL_LINE_STIPPLE)
    bgl.glColor4f(0.0, 0.0, 0.0, 1.0)
    
class CenterBackgroundOperator(bpy.types.Operator):
    """Modal operator used to center a background image based on user input."""
    bl_idname = "background_image.center_background"
    bl_label = "Center Background Image"
    bg_index = bpy.props.IntProperty()
    
    def modal(self, context, event):
        context.area.tag_redraw()
        if event.type in {'MIDDLEMOUSE', 'WHEELUPMOUSE', 'WHEELDOWNMOUSE'}:
            # allow navigation
            return {'PASS_THROUGH'}
        
        if event.type == 'MOUSEMOVE':
            self.center_pt = region_2d_to_origin_3d(bpy.context.region,bpy.context.region_data,(event.mouse_region_x, event.mouse_region_y))

        elif event.type == 'LEFTMOUSE':
            print(self.center_pt)
            area = bpy.context.area
            bg_image = area.spaces[0].background_images[self.bg_index]
            aspect_ratio =  float(bg_image.image.size[0])/float(bg_image.image.size[1])
            bg_image.offset_x = bg_image.offset_x - self.center_pt[0]
            bg_image.offset_y = bg_image.offset_y - self.center_pt[1] * aspect_ratio
            context.region.callback_remove(self._handle)
            return {'FINISHED'}

        elif event.type in {'RIGHTMOUSE', 'ESC'}:
            context.region.callback_remove(self._handle)
            return {'CANCELLED'}

        return {'RUNNING_MODAL'}

    def invoke(self, context, event):
        if context.area.type == 'VIEW_3D':
            context.window_manager.modal_handler_add(self)
            # Add the region OpenGL drawing callback
            # draw in view space with 'POST_VIEW' and 'PRE_VIEW'
            self._handle = context.region.callback_add(bg_center_draw_callback_px, (self, context), 'POST_PIXEL')
            self.center_pt = None
            return {'RUNNING_MODAL'}
        else:
            self.report({'WARNING'}, "View3D not found, cannot run operator")
            return {'CANCELLED'}

class ScaleBackgroundOperator(bpy.types.Operator):
    """Modal operator used to scale a background image based on a dimension on the image."""
    bl_idname = "background_image.scale_background"
    bl_label = "Scale Background Image"
    bg_index = bpy.props.IntProperty()
    
    def modal(self, context, event):
        global length
        context.area.tag_redraw()

        if event.type in {'MIDDLEMOUSE', 'WHEELUPMOUSE', 'WHEELDOWNMOUSE'}:
            # allow navigation
            return {'PASS_THROUGH'}
        
        if event.type == 'MOUSEMOVE':
            if len(self.scaling_pts) == 0:
                return{'RUNNING_MODAL'}
            elif len(self.scaling_pts) < 2:
                self.scaling_pts.append(region_2d_to_origin_3d(bpy.context.region,bpy.context.region_data,(event.mouse_region_x, event.mouse_region_y)))
            else:
                self.scaling_pts[1] = region_2d_to_origin_3d(bpy.context.region,bpy.context.region_data,(event.mouse_region_x, event.mouse_region_y))

        elif event.type == 'LEFTMOUSE':
            if len(self.scaling_pts) == 0:
                self.scaling_pts.append(region_2d_to_origin_3d(bpy.context.region,bpy.context.region_data,(event.mouse_region_x, event.mouse_region_y)))
                return{'RUNNING_MODAL'}
            elif len(self.scaling_pts) == 2:
                length = (self.scaling_pts[1] - self.scaling_pts[0]).magnitude
                print(length)
                bpy.ops.background_image.scale_dialog('INVOKE_DEFAULT',bg_index = self.bg_index)
                context.region.callback_remove(self._handle)
                return {'FINISHED'}

        elif event.type in {'RIGHTMOUSE', 'ESC'}:
            context.region.callback_remove(self._handle)
            return {'CANCELLED'}

        return {'RUNNING_MODAL'}

    def invoke(self, context, event):
        if context.area.type == 'VIEW_3D':
            context.window_manager.modal_handler_add(self)

            # Add the region OpenGL drawing callback
            # draw in view space with 'POST_VIEW' and 'PRE_VIEW'
            self._handle = context.region.callback_add(bg_scale_draw_callback_px, (self, context), 'POST_PIXEL')

            self.scaling_pts = []

            return {'RUNNING_MODAL'}
        else:
            self.report({'WARNING'}, "View3D not found, cannot run operator")
            return {'CANCELLED'}

class ScaleBackgroundDialog(bpy.types.Operator):
    bl_idname = "background_image.scale_dialog"
    bl_label = "Background Scaling Tool"
    scale_dim = bpy.props.FloatProperty(name="Measurement [m]", min=0.0)
    bg_index = bpy.props.IntProperty()
 
    def execute(self, context):
        global length
        area = bpy.context.area
        area.spaces[0].background_images[self.bg_index].size = area.spaces[0].background_images[self.bg_index].size * self.scale_dim / length
        return{'FINISHED'}
 
    def draw(self,context):
        layout = self.layout
        row = layout.row()
        row.prop(self,'scale_dim')
        
    
    def invoke(self, context, event):
        self.scale_dim = 0.0
        return bpy.context.window_manager.invoke_props_dialog(self, width=300)
