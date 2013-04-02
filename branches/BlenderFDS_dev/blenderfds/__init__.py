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

# Reload if changed
if "bpy" in locals():
    import imp
    imp.reload(bf_ui)
    imp.reload(bf_export)
    imp.reload(bf_operators)
    imp.reload(bf_handlers)
    imp.reload(bf_types)
    imp.reload(bf_objects)
else:
    import bpy
    from . import bf_ui, bf_export, bf_operators, bf_handlers, bf_types, bf_objects

### Registration/Unregistration

def register():
    bpy.utils.register_module(__name__)
    for bf_namelist in bf_types.bf_namelists: bf_namelist.register()
    bpy.types.INFO_MT_file_export.append(bf_export.export_fds_menu)
    bpy.app.handlers.load_post.append(bf_handlers.load_handler)
    bpy.app.handlers.save_post.append(bf_handlers.save_handler)
    ### Update bf_namelist menus FIXME improve
    bf_objects.update_menu()

def unregister():
    bpy.utils.unregister_module(__name__)
    for bf_namelist in bf_types.bf_namelists: bf_namelist.unregister()
    bpy.types.INFO_MT_file_export.remove(bf_export.export_fds_menu)
    bpy.app.handlers.load_post.remove(bf_handlers.load_handler)
    bpy.app.handlers.save_post.remove(bf_handlers.save_handler)

if __name__ == "__main__":
    register()
