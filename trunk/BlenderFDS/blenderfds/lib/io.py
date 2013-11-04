"""BlenderFDS, input/output routines"""

import bpy, os
from bpy_extras.io_utils import ExportHelper
from blenderfds.types import *
from blenderfds.types.flags import *
from blenderfds.lib import utilities

DEBUG = False

def save(operator, context, filepath=""):
    """Export current Blender Scene to an FDS case file"""

    # Init
    to_fds_error = False
    to_ge1_error = False
    if not filepath.lower().endswith('.fds'): filepath += '.fds'
    filepath = bpy.path.abspath(filepath)
    
    # Prepare FDS filepath
    print("BFDS: io.save: Exporting current scene to FDS case file: {}".format(context.scene.name))
    if not utilities.is_writable(filepath):
        operator.report({"ERROR"}, "FDS file not writable, cannot export")
        return {'CANCELLED'}

    # Prepare FDS file
    try: fds_file = context.scene.to_fds(context=context)
    except BFException as err:
        fds_file = "".join(("ERROR: {}\n".format(msg) for msg in err.labels))
        to_fds_error = True

    # Write FDS file
    if not utilities.write_to_file(filepath, fds_file):
        operator.report({"ERROR"}, "FDS file not writable, cannot export")
        return {'CANCELLED'}
        
    # Prepare GE1 filepath (always export!)
    print("BFDS: io.save: Exporting current scene to GE1 render file: {}".format(context.scene.name))
    filepath = filepath[:-4] + '.GE1'
    if not utilities.is_writable(filepath):
        operator.report({"ERROR"}, "GE1 file not writable, cannot export")
        return {'CANCELLED'}
        
    # Prepare GE1 file
    try: ge1_file = context.scene.to_ge1(context=context)
    except BFException as err:
        ge1_file = "".join(("ERROR: {}\n".format(msg) for msg in err.labels))
        to_ge1_error = True

    # Write GE1 file
    if not utilities.write_to_file(filepath, ge1_file):
        operator.report({"ERROR"}, "GE1 file not writable, cannot export")
        return {'CANCELLED'}
                
    # Check errors
    if to_fds_error:
        operator.report({"ERROR"}, "Errors reported, check exported FDS file")
        return {'CANCELLED'}
    if to_ge1_error:
        operator.report({"ERROR"}, "Errors reported, check exported GE1 file")
        return {'CANCELLED'}

    # End
    print("BFDS: io.save: End.")
    operator.report({"INFO"}, "FDS File exported")
    return {'FINISHED'}

def load(operator, context, filepath=""):
    """Import FDS file to new Blender Scene"""

    # Read file to Text Editor
    print("BFDS: io.load: Importing:", filepath)
    try: bpy.data.texts.load(filepath, internal=True)
    except:
        operator.report({"ERROR"}, "FDS file not readable, cannot import")
        return {'CANCELLED'}
    bpy.data.texts[-1].name = "Original FDS file"

    # Import to current scene
    if DEBUG: print("BFDS: io.load: importing.")
    try: context.scene.from_fds(context=context, value=bpy.data.texts[-1].as_string(), progress=True)
    except BFException as err:
        operator.report({"ERROR"}, "Errors reported, check custom text file")
        return {'CANCELLED'}

    # End
    print("BFDS: io.load: End.")
    operator.report({"INFO"}, "FDS File imported")
    return {'FINISHED'} 