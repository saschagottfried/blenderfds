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

from . import bf_geometry, bf_config
import bpy
from bpy.path import abspath, clean_name
from time import time, strftime, localtime
from sys import modules

# Remember:
# a = ("OBST",("ID","example"),)
# a += (("SAWTOOTH",True),)
# a
# ('OBST', ('ID', 'example'), ('SAWTOOTH', True))

### Helper functions: format parameters and namelists, check strings

def pa(arg):
    """Format parameter.
    arg[0]: the parameter name
    arg[1]: the parameter value"""
    if len(arg) == 1: return arg[0]
    name, value = arg[0], arg[1]
    if isinstance(value,bool):
        if value:
            return "{0}={1}".format(name,".TRUE.")
        else:
            return "{0}={1}".format(name,".FALSE.")
    if isinstance(value,tuple) or isinstance(value,list) or isinstance(value,set):
        lenght = len(value)
        if isinstance(value[0],float):
            if lenght == 3:
                return "{0}={1[0]:.3f},{1[1]:.3f},{1[2]:.3f}".format(name,value)
            if lenght == 6:
                return "{0}={1[0]:.3f},{1[1]:.3f},{1[2]:.3f},{1[3]:.3f},{1[4]:.3f},{1[5]:.3f}".format(name,value)
        if isinstance(value[0],int):
            if lenght == 3:
                return "{0}={1[0]},{1[1]},{1[2]}".format(name,value)
            if lenght == 6:
                return "{0}={1[0]},{1[1]},{1[2]},{1[3]},{1[4]},{1[5]}".format(name,value)
    if isinstance(value,float):
        return "{0}={1:.3f}".format(name,value)
    if isinstance(value,str):
        return "{0}='{1}'".format(name,value)
    if isinstance(value,int):
        return "{0}={1}".format(name,value)
    # Error management
    text = "BlenderFDS: ERROR: pa() error while formatting parameter: {0}, {1}".format(name, value)
    raise TypeError(text)

def nl(args):
    """Format full namelist and manages carriage returns.
    args[0]: namelist name
    args[1:]: parameters as lists, eg. ("ID","example")
    """
    if len(args) == 1:
        return "&{0} /\n".format(args[0])
    else:
        result = "&{0} {1}".format(args[0], pa(args[1]))
        col_max = bf_config.col_max
        row = 0 # start row
        for arg in args[2:]:
            pa_tmp = pa(arg)
            if (len(result) + len(pa_tmp)) // col_max > row: # exceeds max row columns
                row += 1
                result += ",\n      {0}".format(pa_tmp) # new line
            else:
                result += ", {0}".format(pa_tmp) # append parameter
        return "{0}, /\n".format(result)

def h1(value):
    """Format heading1"""
    return "\n!!! {}\n\n".format(value,)

def h2(value):
    """Format description"""
    return "! {}\n".format(value,)

def sp():
    """Format separator"""
    return "\n"

def has_quotes(string):
    """Check if string contains quotes"""
    return "'" in string or '"' in string or "`" in string or "“" in string or "”" in string or "‘" in string or "’‌" in string

def has_unmatched_quotes(string):
    """Check if string does not contain an even number of single quotes only"""
    return string.count("'") % 2 != 0 or '"' in string or "`" in string or "“" in string or "”" in string or "‘" in string or "’‌" in string

### Helper functions: build object and material namelists

def _include_element(context, el):
    """Build element namelist"""
    # Init
    el_name = el.name
    print("BlenderFDS:     ", el_name)
    out = tuple() # ( ("OBST",("ID", "Cube"),("XB", (1.,2.,3.,4.,5.,6.)), ...), other namelist )
    nl_params = bf_config.nl_params.get(el.bf_nl,"")
    # Prepare comment and group name
    comment = el_name
    gr = (el.bf_nl,) # ("OBST", )
    # Prepare single parameters
    pas = tuple() # ( ("FYI", "for your info"), ("SAWTOOTH", False), other parameter )
    if "FYI" in nl_params and el.bf_fyi and not has_quotes(el.bf_fyi):
        pas += (("FYI",el.bf_fyi),)
    if "SURF_ID" in nl_params and el.active_material and el.active_material.bf_export:
        pas += (("SURF_ID",el.active_material.name),)
    if "SAWTOOTH" in nl_params and el.bf_sawtooth:
        pas += (("SAWTOOTH",False),)
    if "THICKEN" in nl_params and el.bf_thicken:
        pas += (("THICKEN",True),)
    if "IJK" in nl_params:
        comment += ", {0} cells, cell size is {1[0]:.3f} x {1[1]:.3f} x {1[2]:.3f}".format(bf_geometry.get_cell_number(el), bf_geometry.get_cell_size(el))
        pas += (("IJK",(el.bf_ijk_n[0],el.bf_ijk_n[1],el.bf_ijk_n[2])),)
    if "RGB" in nl_params:
        pas += (("RGB",(int(el.diffuse_color[0]*255.),int(el.diffuse_color[1]*255.),int(el.diffuse_color[2]*255.)) ),)
    if "HRRPUA" in nl_params and el.bf_useHRR:
        pas += (("HRRPUA", el.bf_hrrpua),)
    if "TAU_Q" in nl_params and el.bf_UseHRRRamp:
        pas += (("TAU_Q", el.bf_tau_q),)
    if "TRANSPARENCY" in nl_params and el.use_transparency:
        pas += (("TRANSPARENCY",el.alpha),)
    if "OUTLINE" in nl_params and el.draw_type == "Wire":
        pas += (("OUTLINE",True),)
    if el.bf_custom_param and not has_unmatched_quotes(el.bf_custom_param):
        pas += ((el.bf_custom_param,),)
    # Prepare geometric parameters:
    is_multiple = False # does a multiple param exist already?
    xbs, xyzs, pbs = False, False, False # tuples of coordinates
    ges_single = tuple() # single nl geom params: (("XB", (1.2,1.3,1.4,1.5,1.6)), ("XYZ", (1.2,1.3,1.4)), ... )
    ges_multiple = tuple() # multi nl geom params, eg. voxels: (("XB", (1.2,1.3,1.4,1.5,1.6)), ("XB", (1.2,1.3,1.4,1.5,1.6)), ...)
    # Is XB set? then is it VOXELS, BBOX, ... ?
    if "XB" in nl_params and el.bf_xb != "NONE":
        if   el.bf_xb == "VOXELS":
            is_multiple = True
            xbs, tt, dimension_too_large = bf_geometry.get_voxels(context, el)
            for xb in xbs: ges_multiple += (("XB",xb),)
            comment += ", from voxelization ({0} voxels, voxel size is {1:.3f}), in {2:.3f} s".format(len(xbs), el.bf_voxel_size, tt)
            if dimension_too_large:
                out += (h2("WARNING: {} object size too large, voxel size not guaranteed".format(el_name)), )
        elif el.bf_xb == "BBOX":
            xbs, tt = bf_geometry.get_bbox(context, el)
            ges_single += (("XB",xbs[0]),)
            comment += ", from bounding box, in {0:.3f} s".format(tt)
        elif el.bf_xb == "FACES":
            is_multiple = True
            xbs, tt = bf_geometry.get_faces(context, el)
            for xb in xbs: ges_multiple += (("XB",xb),)
            comment += ", from {0} faces, in {1:.3f} s".format(len(xbs), tt)
        elif el.bf_xb == "EDGES":
            is_multiple = True
            xbs, tt = bf_geometry.get_edges(context, el)
            for xb in xbs: ges_multiple += (("XB",xb),)
            comment += ", from {0} edges, in {1:.3f} s".format(len(xbs), tt)
    # Is XYZ set? then is it CENTER, VERTS and the only multiple?
    if "XYZ" in nl_params and el.bf_xyz != "NONE":
        if   el.bf_xyz == "CENTER":
            xyzs, tt = bf_geometry.get_center(context, el)
            ges_single += (("XYZ",xyzs[0]),)
            comment += ", from center point, in {0:.3f} s".format(tt)
        elif el.bf_xyz == "VERTS" and not is_multiple:
            is_multiple = True
            xyzs, tt = bf_geometry.get_vertices(context, el)
            for xyz in xyzs: ges_multiple += (("XYZ",xyz),)
            comment += ", from {0} vertices, in {1:.3f} s".format(len(xyzs), tt)
    # Is PB set to FACES and the only multiple?
    if "PB" in nl_params and el.bf_pb != "NONE":
        if   el.bf_pb == "FACES" and not is_multiple:
            is_multiple = True
            pbs, tt = bf_geometry.get_planes(context, el)
            for pb in pbs: ges_multiple += (("PB{}".format(pb[0]),pb[1]),)
            comment += ", from {0} faces, in {1:.3f} s".format(len(pbs), tt)
    # Send out the comment and the full namelist
    out += (h2(comment),)
    len_ges_multiple = len(ges_multiple)
    if len_ges_multiple > 1:
        if "ID" in nl_params and not has_quotes(el_name):
            for index, ge_multiple in enumerate(ges_multiple):
                id = (("ID", "{0}_{1}".format(el_name, index)),)
                line = gr + id + pas + ges_single + (ge_multiple,)
                # DEBUG print("m>1,id: {}".format(line))
                out += (nl(line),)
        else:
            for ge_multiple in ges_multiple:
                line = gr + pas + ges_single + (ge_multiple,)
                # DEBUG print("m>1: {}".format(line))
                out += (nl(line),)
    elif len_ges_multiple == 1:
        if "ID" in nl_params and not has_quotes(el_name):
            id = (("ID", el_name),)
            line = gr + id + pas + ges_single + (ges_multiple[0],)
            # DEBUG print("m=1,id: {}".format(line))
            out += (nl(line),)
        else:
            line = gr + pas + ges_single + (ges_multiple[0],)
            # DEBUG print("m=1: {}".format(line))
            out += (nl(line),)
    else: # len_ges_multiple == 0
        if "ID" in nl_params and not has_quotes(el_name):
            id = (("ID", el_name),)
            line = gr + id + pas + ges_single
            # DEBUG print("m=0,id: {}".format(line))
            out += (nl(line),)
        else:
            line = gr + pas + ges_single
            # DEBUG print("m=0: {}".format(line))
            out += (nl(line),)
    return out

def _include_elements(context, els):
    """Include Blender materials and Blender objects in FDS """
    out = tuple()
    for nl_group in bf_config.nl_groups:
        # group elements, remainings in last empty group
        if nl_group[1]:
            els_group = {el for el in els if el.bf_nl in nl_group[1]}
            els -= els_group
        else:
            els_group = els
        # sort grouped elements
        els_group = [(el.name, el) for el in els_group]
        els_group.sort()
        # include title and each object
        out += (h1(nl_group[0]),)
        for el in els_group:
            out += _include_element(context,el[-1])
    return out

### Main

def save(operator, context, filepath=""):
    """Export Blender current scene to FDS case file"""
    # Init, clean sc.name and sc.bf_title
    sc = context.scene
    sc_name = clean_name(sc.name)
    bf_title = sc.bf_title
    if has_quotes(bf_title): bf_title = False
    out = tuple()
    t0 = time()
    print("\nBlenderFDS: Start exporting to FDS case file, current scene:", sc_name)
    # Create FDS predefined entities, if not already done
    if not set(bf_config.mas_predefined) <= set(bpy.data.materials.keys()):
        bpy.ops.material.bf_create_predefined()
    # Update BlenderFDS version in scene
    sc.bf_version = modules.get("blenderfds").bl_info["version"]
    # Prepare file name
    if not filepath.lower().endswith('.fds'):
        filepath += '.fds'
    # Try to write to file
    print("BlenderFDS: Check if FDS case file is writable")
    try:
        with open(filepath, "w") as out_file:
            out_file.write("Test")
    except IOError:
        operator.report({"ERROR"}, "Output file not writable, cannot export")
        return {'CANCELLED'}
    # Include log message and HEAD
    print("BlenderFDS: Include log message and HEAD")
    text = "Generated by BlenderFDS {0} on Blender {1}".format(".".join(str(x) for x in modules.get("blenderfds").bl_info["version"]),bpy.app.version_string)
    out += (h1(text),
            h2("Blender file: {0}".format(bpy.data.filepath)),
            h2(strftime("Date: %a, %d %b %Y %H:%M:%S", localtime())),
            sp(),
            nl(("HEAD", ("CHID", sc_name),("TITLE", bf_title or sc_name))),
           )
    # Include config file
    if sc.bf_config_type == "FILE":
        print("BlenderFDS: Include external config file")
        try:
            with open(abspath(sc.bf_config_filepath)) as in_file:
                out += (h1("External config file"),)
                out += tuple([line for line in in_file])
        except IOError:
            operator.report({"ERROR"}, "External config file not readable, cannot export")
            return {'CANCELLED'}
    else: # bf_config_type == "AUTO"
        out += nl(("TIME",("T_END",0.0),)),
    # Select Blender objects and materials to be exported, then include them
    print("BlenderFDS: Export Blender materials and objects")
    obs = {ob for ob in sc.objects if ob.type == "MESH" and ob.bf_export and not ob.bf_is_voxels}
    mas = {ob.active_material for ob in obs if ob.bf_export and not ob.bf_is_voxels and ob.active_material and ob.active_material.bf_export and (ob.active_material.name not in bf_config.mas_predefined)}
    out += _include_elements(context, mas | obs)
    # Close FDS case
    print("BlenderFDS: Close FDS case")
    out += (sp(), nl(("TAIL",)),)
    out += (h2("Generated in {0:.3f} s.".format(time()-t0)),)
    # Write results to file
    print("BlenderFDS: Writing to FDS case file")
    try:
        with open(filepath, "w") as out_file:
            for line in out:
                out_file.write(line)
    except IOError:
        operator.report({"ERROR"}, "Output file not writable, cannot export")
        return {'CANCELLED'}
    # End
    print("BlenderFDS: Exporting to FDS case file completed.")
    return {'FINISHED'}
