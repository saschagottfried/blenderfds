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


bl_info = {
    "name": "BlenderFDS",
    "author": "Emanuele Gissi",
    "version": (9, 9, 9),
    "blender": (2, 6, 4),
    "api": 35622,
    "location": "File > Export > FDS Case (.fds)",
    "description": "BlenderFDS, an open graphical editor for the NIST Fire Dynamics Simulator",
    "warning": "",
    "wiki_url": "http://www.blenderfds.org/",
    "tracker_url": "http://code.google.com/p/blenderfds/issues/list",
    "support": "COMMUNITY",
    "category": "Import-Export",
}

# Reload if changed
if "bpy" in locals():
    import imp
    imp.reload(bf_export)
    imp.reload(bf_operators)
    imp.reload(bf_handlers)
    imp.reload(bf_types)
    imp.reload(bf_objects)
else:
    import bpy
    from . import bf_export, bf_operators, bf_handlers, bf_types, bf_objects

### Registration/Unregistration

def register():
    """Register Blender types"""
    bpy.utils.register_module(__name__)
    # Register namelists, their properties, and the panels
    for bf_namelist in bf_types.bf_namelists: bf_namelist.register()
    # Register menu and handlers
    bpy.types.INFO_MT_file_export.append(bf_export.export_fds_menu)
    bpy.app.handlers.load_post.append(bf_handlers.load_handler)
    bpy.app.handlers.save_post.append(bf_handlers.save_handler)
    # Update bf_params["Namelist"] items
    # This must be done now, after registering all other objects
    bf_types.bf_props["Namelist"].update_bf_namelist_items()
    # TODO analyze and clean up
    bpy.types.VIEW3D_PT_background_image.draw = bg_panel_draw

def unregister():
    """Unregister Blender types"""
    bpy.utils.unregister_module(__name__)
    # Unegister namelists, their properties, and the panels
    for bf_namelist in bf_types.bf_namelists: bf_namelist.unregister()
    # Unregister menu and handlers
    bpy.types.INFO_MT_file_export.remove(bf_export.export_fds_menu)
    bpy.app.handlers.load_post.remove(bf_handlers.load_handler)
    bpy.app.handlers.save_post.remove(bf_handlers.save_handler)

# TODO Analyze and clean up

### UI: Background panel draw override

def bg_panel_draw(self, context):
    '''Override the default background panel in the properties area.
       Used to add in the scaling tool UI elements'''
    layout = self.layout

    view = context.space_data
    col = layout.column()
    col.operator("view3d.background_image_add", text="Add Image")

    for i, bg in enumerate(view.background_images):
        layout.active = view.show_background_images
        box = layout.box()
        row = box.row(align=True)
        row.prop(bg, "show_expanded", text="", emboss=False)
        if bg.source == 'IMAGE' and bg.image:
            row.prop(bg.image, "name", text="", emboss=False)
        elif bg.source == 'MOVIE_CLIP' and bg.clip:
            row.prop(bg.clip, "name", text="", emboss=False)
        else:
            row.label(text="Not Set")

        if bg.show_background_image:
            row.prop(bg, "show_background_image", text="", emboss=False, icon='RESTRICT_VIEW_OFF')
        else:
            row.prop(bg, "show_background_image", text="", emboss=False, icon='RESTRICT_VIEW_ON')

        row.operator("view3d.background_image_remove", text="", emboss=False, icon='X').index = i

        box.prop(bg, "view_axis", text="Axis")

        if bg.show_expanded:
            row = box.row()
            row.prop(bg, "source", expand=True)

            has_bg = False
            if bg.source == 'IMAGE':
                row = box.row()
                row.template_ID(bg, "image", open="image.open")
                if (bg.image):
                    box.template_image(bg, "image", bg.image_user, compact=True)
                    has_bg = True

            elif bg.source == 'MOVIE_CLIP':
                box.prop(bg, "use_camera_clip")

                column = box.column()
                column.active = not bg.use_camera_clip
                column.template_ID(bg, "clip", open="clip.open")

                if bg.clip:
                    column.template_movieclip(bg, "clip", compact=True)

                if bg.use_camera_clip or bg.clip:
                    has_bg = True

                column = box.column()
                column.active = has_bg
                column.prop(bg.clip_user, "proxy_render_size", text="")
                column.prop(bg.clip_user, "use_render_undistorted")

            if has_bg:
                col = box.column()
                col.prop(bg, "opacity", slider=True)

                rowsub = col.row()
                rowsub.prop(bg, "draw_depth", expand=True)

                if bg.view_axis in {'CAMERA', 'ALL'}:
                    rowsub = col.row()
                    rowsub.prop(bg, "frame_method", expand=True)

                row = col.row(align=True)
                row.prop(bg, "offset_x", text="X")
                row.prop(bg, "offset_y", text="Y")

                if bg.view_axis != 'CAMERA':
                    col.prop(bg, "size")
                
                box = box.box()
                row = box.row()
                row.label(text="Image Scale & Location")
                row = box.row()
                row.operator("background_image.scale_background", text="Re-Scale").bg_index = i
                row = box.row()
                row.operator("background_image.center_background", text="Relocate Origin").bg_index = i
               

if __name__ == "__main__":
    register()
