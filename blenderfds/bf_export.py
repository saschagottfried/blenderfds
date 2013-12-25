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

from . import __init__, bf_geometry, bf_config
from time import time, strftime, localtime
from bpy.path import abspath
from bpy import app
from sys import modules

### Helper functions: format namelists
#
#

def pa(arg):
    """Format parameter.
    arg[0]: the parameter name
    arg[1]: the parameter value"""
    if len(arg) == 1:
        return arg[0]
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

    print("BlenderFDS: error while formatting parameter:", name, value)
    raise Exception

def nl(args):
    """Format full namelist and manages carriage returns.
    args[0]: namelist name
    args[1:]: parameters as lists, eg. ("ID","example")
    """
    if len(args) == 1:
        return "&" + args[0] + " /\n"
    else:
        result = "&" + args[0] + " " + pa(args[1])
        col_max = bf_config.col_max
        row = 0 # start row
        for arg in args[2:]:
            pa_tmp = pa(arg)
            if len(result + pa_tmp) // col_max > row: # exceeds max row columns
                row += 1
                result += ",\n      " + pa_tmp # new line
            else:
                result += ", " + pa_tmp # append parameter
        return result + ", /\n"

def h1(value):
    """Format heading1"""
    return "\n!!! {}\n\n".format(value,)

def h2(value):
    """Format description"""
    return "! {}\n".format(value,)

def sp():
    """Format separator"""
    return "\n"

### Helper functions: build object and material namelist
# Remember:
# pa = ("OBST",("ID", "Cube"),("XB", (1.,2.,3.,4.,5.,6.)),("NO", 3),)
# pa += (("NO",3),)

def include_element(context, el):
    """Build a namelist"""
    out = tuple()
    nl_params = bf_config.nl_params.get(el.bf_nl,"")
    print("BlenderFDS:     ", el.name)

    # Prepare single parameters
    pa = (el.bf_nl,)
    if "ID" in nl_params:
        pa += (("ID",el.name),)
        if el.bf_fyi: pa += (("FYI",el.bf_fyi),)
    else: # no ID, then put the element name into FYI
        if el.bf_fyi: pa += (("FYI","(" + el.name + ") " + el.bf_fyi),)
        else: pa += (("FYI","(" + el.name + ")"),)
    if "SURF_ID" in nl_params and el.active_material and el.active_material.bf_export: pa += (("SURF_ID",el.active_material.name),)
    if "SAWTOOTH" in nl_params and el.bf_sawtooth: pa += (("SAWTOOTH",False),)
    if "IJK" in nl_params and el.bf_ijk:
        mesh_cells = bf_geometry.get_mesh_cells(el)
        text = "{1} cells, size: {0[0]:.3f}, {0[1]:.3f}, {0[2]:.3f} m".format(mesh_cells["cell_size"], mesh_cells["quantity"])
        out += (h2(text),)
        pa += (("IJK",mesh_cells["ijk"]),)
    if "RGB" in nl_params: pa += (("RGB",(int(el.diffuse_color[0]*255.),int(el.diffuse_color[1]*255.),int(el.diffuse_color[2]*255.)) ),)
    if "TRANSPARENCY" in nl_params and el.use_transparency: pa += (("TRANSPARENCY",el.alpha),)
    if "OUTLINE" in nl_params and el.draw_type == "Wire": pa += (("OUTLINE",True),)

    if el.bf_custom_param: pa += ((el.bf_custom_param,),)

    # prepare multiple parameters
    multiple = False
    if "XB" in nl_params or "XYZ" in nl_params or "PB" in nl_params:
        xbs, xyzs, pbs = False, False, False

        if el.bf_xb != "NONE":
            if   el.bf_xb == "VOXELS" and bf_geometry.is_manifold(el.data):
                multiple = "XB"
                xbs = bf_geometry.get_boxels(el, context.scene.bf_voxel_size, solid=True)
            elif el.bf_xb == "BBOX":
                xbs = bf_geometry.get_bbox(el)
            elif el.bf_xb == "FACES":
                multiple = "XB"
                xbs = bf_geometry.get_faces(el)
            elif el.bf_xb == "EDGES":
                multiple = "XB"
                xbs = bf_geometry.get_edges(el)

        if el.bf_xyz != "NONE":
            if   el.bf_xyz == "CENTER":
                xyzs = bf_geometry.get_center(el)
            elif el.bf_xyz == "VERTS" and not multiple:
                multiple = "XYZ"
                xyzs = bf_geometry.get_vertices(el)

        if el.bf_pb == "FACES" and not multiple:
            multiple = "PB"
            pbs = bf_geometry.get_planes(el) # special treatment for PBX PBY PBZ

        geo_pa = tuple() # tuple of geometric parameter
        geo_multiple_pas = tuple() # tuple of tuples of geometric parameters

        if multiple == "XB":
            if xyzs: geo_pa += (("XYZ",xyzs[0]),)
            if pbs:  geo_pa += (("PB" + pbs[0][0],pbs[0][1]),)
            for xb in xbs:
                geo_multiple_pas += (geo_pa + (("XB",xb),),)
        elif multiple == "XYZ":
            if xbs:  geo_pa += (("XB",xbs[0]),)
            if pbs:  geo_pa += (("PB" + pbs[0][0],pbs[0][1]),)
            for xyz in xyzs:
                geo_multiple_pas += (geo_pa + (("XYZ",xyz),),)
        elif multiple == "PB":
            if xbs:  geo_pa += (("XB",xbs[0]),)
            if xyzs: geo_pa += (("XYZ",xyzs[0]),)
            for pb in pbs:
                geo_multiple_pas += (geo_pa + (("PB" + pb[0],pb[1]),),)
        else:
            if xbs:  geo_pa += (("XB",xbs[0]),)
            if xyzs: geo_pa += (("XYZ",xyzs[0]),)
            if pbs:  geo_pa += (("PB" + pbs[0][0],pbs[0][1]),)            
            geo_multiple_pas += (geo_pa,)

        # send out
        if multiple: out += (h2(el.name),)
        for geo_multiple_pa in geo_multiple_pas:
            out += (nl(pa + geo_multiple_pa),)
    else:
        out += (nl(pa),)
    return out

###
#
#

def include_config(context):
    """Include auto config or config file"""
    sc = context.scene
    out = tuple()
    # Log message and HEAD
    out += (h1("Generated by BlenderFDS " + '.'.join(str(x) for x in modules.get("blenderfds").bl_info["version"]) + " on Blender " + app.version_string),
            h2(strftime("%a, %d %b %Y %H:%M:%S", localtime()) ),
            h2("Voxel Size: {0[0]:.3f}, {0[1]:.3f}, {0[2]:.3f}".format(sc.bf_voxel_size) ),
            sp(),
            nl(("HEAD",("CHID",sc.name),("TITLE",sc.bf_title or sc.name))),
            )
    # External config file
    if sc.bf_config_type == "FILE":
        try:
            with open(abspath(sc.bf_config_filepath)) as in_file:
                out += (h1("External config file"),)
                out += tuple([line for line in in_file])
        except IOError:
            out += (h1("ERROR: Config file not found"),)
    else:
        out += nl(("TIME",("T_END",0.0),)),
    return out

def include_elements(context, els):
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
            out += include_element(context,el[-1])
    return out

###
#
#

def save(operator, context, filepath="", export_visible=False):
    """Export Blender data to FDS case file"""
    sc = context.scene
    out = tuple()
    t0 = time()
    print("BlenderFDS: Start exporting to FDS case file")
    
    # Include auto config or config file
    print("BlenderFDS: Include auto config or config file")
    out += include_config(context)

    # Select Blender objects to be exported
    print("BlenderFDS: Select Blender objects")
    if export_visible:
        obs = {ob for ob in sc.objects if ob.type == "MESH" and ob.bf_export and ob.is_visible(sc)}
    else:
        obs = {ob for ob in sc.objects if ob.type == "MESH" and ob.bf_export}

    # Select Blender materials to be exported, exclude predefined
    print("BlenderFDS: Select Blender materials")
    mas = {ob.active_material for ob in obs if ob.active_material and ob.bf_export and ob.active_material.bf_export and ob.active_material.name not in bf_config.mas_predefined}

    # Include Blender materials and objects
    print("BlenderFDS: Include Blender materials and objects")
    out += include_elements(context, mas | obs)
 
    # Close FDS case
    print("BlenderFDS: Close FDS case")
    out += (sp(), nl(("TAIL",)),)
    
    # Write to file
    print("BlenderFDS: Writing to FDS case file")
    if not filepath.lower().endswith('.fds'):
        filepath += '.fds'
    try:
        with open(filepath, "w") as out_file:
            for line in out:
                out_file.write(line)
    except IOError:
        print("BlenderFDS: ERROR - Could not write to output file", filepath)
        return {'CANCELLED'}
        
    # End
    print("BlenderFDS: Exporting to FDS case file completed in " + str(int(time()-t0)) + " s.")   
    return {'FINISHED'}
