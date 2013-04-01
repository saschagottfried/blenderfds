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
    "category": "Export",
    "location": "File > Export > FDS Case (.fds)",
    "description": "BlenderFDS, an open graphical editor for the NIST Fire Dynamics Simulator",
    "warning": "",
    "wiki_url": "http://www.blenderfds.org/",
    "tracker_url": "http://code.google.com/p/blenderfds/issues/list",
    "support": "COMMUNITY",
    "category": "Import-Export",
}

# Reload if changed FIXME does not work
if "bpy" in locals():
    import imp
    imp.reload(bf_operators)
    imp.reload(bf_ui)  
else:
    from .bf_operators import * # Define operators
    from .bf_handlers import * # Define handlers
    from .bf_ui import *  # Define UI
    import bpy

### Registration/Unregistration

def register():
    bpy.utils.register_module(__name__)
    for bf_namelist in bf_namelists: bf_namelist.register()
    bpy.types.INFO_MT_file_export.append(export_fds_menu)
    bpy.app.handlers.load_post.append(load_handler)
    bpy.app.handlers.save_post.append(save_handler)
    # Here is a workaround to execute handlers on startup, see bf_handlers.py: FIXME
    # bpy.app.handlers.scene_update_pre.append(call_load_handlers)
    ### Update bf_namelist menus FIXME

    items = list((bf_namelist.name,"{} ({})".format(bf_namelist.name,bf_namelist.description),bf_namelist.description,) for bf_namelist in bf_namelists if bf_namelist.bpy_type == bpy.types.Object)
    items.sort()
    print("items:",items)
    bpy.types.Object.bf_namelist = bpy.props.EnumProperty(
        name="Namelist",
        description="Description",
        items=items,
        default="OBST",
        )
    items = list((bf_namelist.name,"{} ({})".format(bf_namelist.name,bf_namelist.description),bf_namelist.description,) for bf_namelist in bf_namelists if bf_namelist.bpy_type == bpy.types.Material)
    items.sort()
    print("items:",items)
    bpy.types.Material.bf_namelist = bpy.props.EnumProperty(
        name="Namelist",
        description="Description",
        items=items,
        default="SURF",
        )


    
def unregister():
    bpy.utils.unregister_module(__name__)
    for bf_namelist in bf_namelists: bf_namelist.unregister()
    bpy.types.INFO_MT_file_export.remove(export_fds_menu)
    bpy.app.handlers.load_post.remove(load_handler)
    bpy.app.handlers.save_post.remove(save_handler)
    # Here is a workaround to execute handlers on startup, see bf_handlers.py: FIXME
    # bpy.app.handlers.scene_update_pre.remove(call_load_handlers)

if __name__ == "__main__":
    register()
