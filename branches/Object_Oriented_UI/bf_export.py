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

from .bf_types import BFError, BFResult, bf_file

from os import path
import bpy
from bpy_extras.io_utils import ExportHelper
from .bf_objects import bf_namelists, bf_params

### Export to .fds menu

def export_fds_menu(self,context):
    # Prepare default filepath
    filepath ="{0}.fds".format(path.splitext(bpy.data.filepath)[0])
    directory = path.dirname(filepath)
    basename = path.basename(filepath)
    # If the context scene contains path and basename, use them
    sc = context.scene # FIXME no BFError management!
    if sc.bf_case_directory: directory = sc.bf_case_directory
    if sc.name: basename = "{0}.fds".format(bpy.path.clean_name(sc.name))
    # Call the exporter operator
    filepath = "{0}/{1}".format(directory, basename)
    self.layout.operator(ExportFDS.bl_idname, text="Fire Dynamics Simulator Case (.fds)").filepath = filepath

class ExportFDS(bpy.types.Operator,ExportHelper):
    bl_label = "Export scene as FDS case"
    bl_idname = "export_scene.nist_fds"
    bl_description = "Export current Blender Scene as an FDS file"
    filename_ext = ".fds"
    filter_glob = bpy.props.StringProperty(default="*.fds", options={'HIDDEN'})

    def execute(self, context):
        return save(self,context,**self.as_keywords(ignore=("check_existing", "filter_glob")))

def save(operator, context, filepath=""):
    """Export current Blender Scene to an FDS case file"""
    print("\nBlenderFDS: save(): Start exporting current scene to FDS case file:", context.scene.name)

    # FIXME predefined materials and case file version
        
    # Prepare file name
    if not filepath.lower().endswith('.fds'): filepath += '.fds'
    # Check output file is writable
    print("BlenderFDS: save(): Check if the file is writable")
    try:
        with open(filepath, "w") as out_file:
            out_file.write("Test")
    except IOError:
        operator.report({"ERROR"}, "Output file not writable, cannot export")
        return {'CANCELLED'}
    # Get results and check
    print("BlenderFDS: save(): Get results")
    try: result = bf_file.to_fds(context)
    except BFError:
        operator.report({"ERROR"}, "Untrapped errors reported, cannot export")
        return {'CANCELLED'}
    if not result or not result.value:
        operator.report({"ERROR"}, "Nothing to export")
        return {'CANCELLED'}
    # FIXME use file msgs???
    # Write to file
    print("BlenderFDS: save(): Write to path:", filepath)
    try:
        with open(filepath, "w") as out_file:
            out_file.write(result.value)
    except IOError:
        operator.report({"ERROR"}, "Output file not writable, cannot export")
        return {'CANCELLED'}
    # End
    print("BlenderFDS: save(): End.")
    return {'FINISHED'}
