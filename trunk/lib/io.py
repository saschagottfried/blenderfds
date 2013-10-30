"""BlenderFDS, input/output routines"""

import bpy, os
from bpy_extras.io_utils import ExportHelper
from blenderfds.types import *
from blenderfds.config import *

def save(operator, context, filepath=""):
    """Export current Blender Scene to an FDS case file"""
    if INFO or DEBUG: print("BFDS: io.save: Exporting current scene to FDS case file: {}".format(context.scene.name))
    # Prepare file name
    if not filepath.lower().endswith('.fds'): filepath += '.fds'
    # Check output file is writable
    if DEBUG: print("BFDS: io.save: Checking writable file: {}".format(filepath))
    filepath = bpy.path.abspath(filepath)
    try:
        with open(filepath, "w") as out_file:
            out_file.write("Test")
    except IOError:
        operator.report({"ERROR"}, "Output file not writable, cannot export")
        return {'CANCELLED'}
    # Get results or errors
    if DEBUG: print("BFDS: io.save: Getting results")
    to_fds_error = False
    try: fds_file = context.scene.to_fds(context=context)
    except BFException as err:
        fds_file = "".join(("ERROR: {}\n".format(msg) for msg in err.labels))
        to_fds_error = True
    # Check existence
    if fds_file is None:
        operator.report({"ERROR"}, "Nothing to export")
        return {'CANCELLED'}
    # Write to file
    if DEBUG: print("BFDS: io.save: Writing to path:".format(filepath))
    try:
        with open(filepath, "w") as out_file: out_file.write(fds_file)
    except IOError:
        operator.report({"ERROR"}, "Output file not writable, cannot export")
        return {'CANCELLED'}
    # Check errors
    if to_fds_error:
        operator.report({"ERROR"}, "Errors reported, check exported file")
        return {'CANCELLED'}
    # End
    if INFO or DEBUG: print("BFDS: io.save: End.")
    operator.report({"INFO"}, "FDS File exported")
    return {'FINISHED'}

def load(operator, context, filepath=""):
    """Import FDS file to new Blender Scene"""
    # Read file to Text Editor
    if INFO or DEBUG: print("BFDS: io.load: loading:", filepath)
    try: bpy.data.texts.load(filepath, internal=True)
    except:
        operator.report({"ERROR"}, "FDS file not readable, cannot import")
        return {'CANCELLED'}
    bpy.data.texts[-1].name = "Original FDS file"
    # Import to current scene
    try: context.scene.from_fds(context=context, value=bpy.data.texts[-1].as_string(), progress=True)
    except BFException as err:
        operator.report({"ERROR"}, "Errors reported, check custom text file")
        return {'CANCELLED'}
    # End
    if INFO or DEBUG: print("BFDS: io.load: End.")
    operator.report({"INFO"}, "FDS File imported")
    return {'FINISHED'} 
