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

from . import bf_geometry, bf_config, bf_objects
from .bf_types import bf_namelists, bf_props, BFError
from .bf_osd import bf_osd
import bpy

# TODO clean up
import bgl
from bpy_extras.view3d_utils import region_2d_to_origin_3d, location_3d_to_region_2d
length = 0.0

### MESH

class OBJECT_OT_bf_correct_ijk(bpy.types.Operator):
    bl_label = "Correct IJK"
    bl_idname = "object.bf_correct_ijk"
    bl_description = "Correct IJK for FDS Poisson solver"

    def execute(self,context):
        ob = context.active_object
        ob.bf_mesh_ijk = bf_geometry.get_good_ijk(ob.bf_mesh_ijk)
        self.report({"INFO"}, "IJK corrected")
        return{'FINISHED'}

### Utilities

class OBJECT_OT_bf_copy_FDS_properties_to_sel_obs(bpy.types.Operator):
    bl_label = "Copy To Selected Objects"
    bl_idname = "object.bf_props_to_sel_obs"
    bl_description = "Copy these properties to other selected objects"

    def execute(self,context):
        bpy.ops.object.mode_set(mode='OBJECT', toggle=False)
        # Get active (source) and selected objects (destination)
        ob_source = context.active_object
        obs_destination = set(ob for ob in context.selected_objects if ob.type == "MESH" and ob != ob_source)
        if not obs_destination:
            self.report({"WARNING"}, "No destination objects")
            return{'CANCELLED'}
        if not ob_source:
            self.report({"WARNING"}, "No source object")
            return{'CANCELLED'}        
        # Get properties to copy
        bf_props_to_copy = set(bf_props) - set((bf_props["ID"], bf_props["ID no export"])) # Do not overwrite IDs
        # Loop on properties and objects
        for bf_prop in bf_props_to_copy:
            for ob in obs_destination:
                bpy_name = bf_prop.bpy_name
                try: setattr(ob, bpy_name, getattr(ob_source, bpy_name))
                except AttributeError: pass
                else: print("BlenderFDS: Copy {} -> {}: {}".format(ob_source.name, ob.name, bf_prop.name))
        self.report({"INFO"}, "Copied to selected objects")
        return{'FINISHED'}
        
class MATERIAL_OT_bf_assign_BC_to_sel_obs(bpy.types.Operator):
    bl_label = "Assign To Selected Objects"
    bl_idname = "material.bf_surf_to_sel_obs"
    bl_description = "Assign this boundary condition to selected objects"

    def execute(self,context):
        bpy.ops.object.mode_set(mode='OBJECT', toggle=False)
        # Get active (source) and selected objects (destination)
        ob_source = context.active_object
        active_material = ob_source.active_material
        obs_destination = set(ob for ob in context.selected_objects if ob.type == "MESH" and ob != ob_source)
        if not obs_destination:
            self.report({"WARNING"}, "No destination objects")
            return{'CANCELLED'}
        if not ob_source:
            self.report({"WARNING"}, "No source object")
            return{'CANCELLED'}
        if not active_material:
            self.report({"WARNING"}, "No boundary condition to assign")
            return{'CANCELLED'}
        # Loop on objects
        for ob in obs_destination:
            ob.active_material = active_material
            print("BlenderFDS: Assign {} -> {}".format(active_material, ob.name))
        # Set myself exported
        active_material.bf_namelist_export = True
        # Return
        self.report({"INFO"}, "Assigned to selected objects")
        return{'FINISHED'}

def _create_material(name):
    '''Create material and return it'''
    if name not in bpy.data.materials:
        bpy.data.materials.new(name)
    return bpy.data.materials[name]

class MATERIAL_OT_bf_set_predefined(bpy.types.Operator):
    bl_label = "Set Predefined"
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

### Test and show exported geometry

class OBJECT_OT_bf_show_fds_geometries(bpy.types.Operator):
    bl_label = "Show FDS Geometries"
    bl_idname = "object.bf_show_fds_geometries"
    bl_description = "Show geometries as exported to FDS"

    def execute(self, context):
        # Init
        ob = context.object
        bf_xb, bf_xyz, bf_pb = None, None, None
        if ob.has_bf_prop_type(bf_objects.BFPropXB): bf_xb = ob.bf_xb
        if ob.has_bf_prop_type(bf_objects.BFPropXYZ): bf_xyz = ob.bf_xyz
        if ob.has_bf_prop_type(bf_objects.BFPropPB): bf_pb = ob.bf_pb
        actions = list()
        # XB
        if bf_xb == "VOXELS":
            actions.append((bf_geometry.get_voxels, bf_geometry.xbs_volumes_to_mesh, "Voxels shown"))
        elif bf_xb == "BBOX":
            actions.append((bf_geometry.get_bbox, bf_geometry.xbs_volumes_to_mesh, "Bounding box shown"))
        elif bf_xb == "FACES":
            actions.append((bf_geometry.get_faces, bf_geometry.xbs_faces_to_mesh, "Faces shown"))
        elif bf_xb == "EDGES":
            actions.append((bf_geometry.get_edges, bf_geometry.xbs_edges_to_mesh, "Edges shown"))
        # XYZ
        if bf_xyz == "VERTICES":
            actions.append((bf_geometry.get_vertices, bf_geometry.xyzs_to_mesh, "Vertices shown"))
        elif bf_xyz == "CENTER":
            actions.append((bf_geometry.get_center, bf_geometry.xyzs_to_mesh, "Center shown"))
        # PB
        if bf_pb == "PLANES":
            actions.append((bf_geometry.get_planes, bf_geometry.pbs_to_mesh, "Planes shown"))
        # No actions
        if not actions:
            self.report({'WARNING'}, "Nothing to show")
            return{'FINISHED'}
        # Actions
        for action in actions:
            try: geometries, tt = action[0](self, context, ob)
            except BFError as err:
                self.report({"ERROR"}, "; ".join(err.msgs))
                return{'FINISHED'}
            bf_geometry.create_tmp_object(context, ob, action[1](geometries))
        self.report({"INFO"}, "; ".join(action[2] for action in actions))
        return{'FINISHED'}

class SCENE_OT_bf_del_all_tmp_objects(bpy.types.Operator):
    bl_label = "Hide Temporary Objects"
    bl_idname = "scene.bf_del_all_tmp_objects"
    bl_description = "Delete all temporary objects"

    def execute(self, context):
        bf_geometry.del_all_tmp_objects(self, context)
        self.report({"INFO"}, "All temporary objects deleted")
        return{'FINISHED'}

# SURF burner

class MATERIAL_OT_bf_set_tau_q(bpy.types.Operator):
    bl_label = "Set t² Ramp"
    bl_idname = "material.set_tau_q"
    bl_description = "Set t² ramp and HRRPUA"

    bf_burner_area = bpy.props.FloatProperty(name="Burner area [m²]", min=0., precision = 1, step=1000)
    bf_hrr_max = bpy.props.FloatProperty(name="HRR max [kW]", min=0., precision = 1, step=1000)
    bf_growth_rate = bpy.props.EnumProperty(
        name = "Growth rate",
        items = (
            ("SLOW", "Slow (600\")", "Slow growth rate (600\")"),
            ("MEDIUM", "Medium (300\")", "Medium growth rate (300\")"),
            ("FAST", "Fast (150\")", "Fast growth rate (150\")"),
            ("ULTRA-FAST", "Ultra fast (75\")", "Ultra fast growth rate (75\")"),
            ),
        )
    bf_reference_hrr = bpy.props.EnumProperty(
        name = "Reference HRR",
        items = (
            ("US", "US, 1000 BTU/s (1055 kW)", "US, 1000 BTU/s (1055 kw)"),
            ("EU", "Eurocode, 1000 kW", "Eurocode, 1000 kW"),
            ),
        )
    bf_set_fyi = bpy.props.BoolProperty(name = "Set FYI")

    def execute(self, context):
        ma = context.object.active_material
        reference_hrr = self.bf_reference_hrr == "US" and 1055. or 1000.
        time = {"SLOW":600., "MEDIUM":300., "FAST":150., "ULTRA-FAST":75.}[self.bf_growth_rate]
        ma.bf_surf_tau_q = -time * (self.bf_hrr_max / reference_hrr) ** .5
        ma.bf_surf_hrrpua = self.bf_hrr_max / self.bf_burner_area
        if self.bf_set_fyi:
            ma.bf_fyi = "Area {} m², HRR max {:.0f} kW, {} t² ramp ({})".format(
                self.bf_burner_area,
                self.bf_hrr_max,
                self.bf_growth_rate.lower(),
                self.bf_reference_hrr
                )
        self.report({'INFO'}, "TAU_Q and HRRPUA set")
        return {'FINISHED'}

    def invoke(self, context, event):
        ma = context.object.active_material
        # Calc burner area
        burner_area = 0.
        obs = (ob for ob in context.scene.objects \
            if ob.type == "MESH" and ob.bf_namelist_export \
            and ob.active_material == ma and ob.bf_namelist in ("OBST","VENT"))
        polygons = (polygon for ob in obs for polygon in ob.data.polygons)
        for polygon in polygons: burner_area += polygon.area
        # Set defaults to estimated values
        self.bf_burner_area = burner_area
        self.bf_hrr_max = ma.bf_surf_hrrpua * burner_area
        self.bf_growth_rate = "FAST"
        self.bf_reference_hrr = "US"
        self.bf_set_fyi = False # The user must choose and understand implications
        # Call dialog
        wm = context.window_manager
        return wm.invoke_props_dialog(self)

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
            bpy.types.SpaceView3D.draw_handler_remove(self._handle, 'WINDOW')
            return {'FINISHED'}

        elif event.type in {'RIGHTMOUSE', 'ESC'}:
            bpy.types.SpaceView3D.draw_handler_remove(self._handle, 'WINDOW')
            return {'CANCELLED'}

        return {'RUNNING_MODAL'}

    def invoke(self, context, event):
        if context.area.type == 'VIEW_3D':
            context.window_manager.modal_handler_add(self)
            # Add the region OpenGL drawing callback
            # draw in view space with 'POST_VIEW' and 'PRE_VIEW'
            self._handle = bpy.types.SpaceView3D.draw_handler_add(bg_center_draw_callback_px, (self, context), 'WINDOW', 'POST_PIXEL')
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
                bpy.types.SpaceView3D.draw_handler_remove(self._handle, 'WINDOW')
                return {'FINISHED'}

        elif event.type in {'RIGHTMOUSE', 'ESC'}:
            bpy.types.SpaceView3D.draw_handler_remove(self._handle, 'WINDOW')
            return {'CANCELLED'}

        return {'RUNNING_MODAL'}

    def invoke(self, context, event):
        if context.area.type == 'VIEW_3D':
            context.window_manager.modal_handler_add(self)

            # Add the region OpenGL drawing callback
            # draw in view space with 'POST_VIEW' and 'PRE_VIEW'
            self._handle = bpy.types.SpaceView3D.draw_handler_add(bg_scale_draw_callback_px, (self, context), 'WINDOW', 'POST_PIXEL')
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
