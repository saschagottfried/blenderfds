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
from time import time, strftime, localtime
from bpy.path import abspath

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

    print("BlenderFDS: error while formatting parameter:",name, value)
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
            if len(result) // col_max > row: # exceeds max row columns
                row += 1
                result += ",\n      " + pa(arg) # new line
            else:
                result += ", " + pa(arg) # append parameter
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

### Helper functions: build object namelist
#
#

def include_object(context, ob):
    """Build object namelists"""
    out = tuple()
    nl_params = bf_config.nl_params
    bf_nl = ob.bf_nl
    print("BlenderFDS:     ", ob.name)
    
    # Remember:
    # pa = ("OBST",("ID", "Cube"),("XB", (1.,2.,3.,4.,5.,6.)),("NO", 3),)
    # pa += (("NO",3),)
    
    # Prepare single parameters
    pa = (bf_nl,)
    if bf_nl in nl_params["ID"]: pa += (("ID",ob.name),)
    if ob.bf_fyi: pa += (("FYI",ob.bf_fyi),)
    if bf_nl in nl_params["SURF_ID"] and ob.active_material and ob.active_material.bf_export: pa += (("SURF_ID",ob.active_material.name),)
    if bf_nl in nl_params["SAWTOOTH"] and ob.bf_sawtooth: pa += (("SAWTOOTH",False),)
    if bf_nl in nl_params["IJK"]: pa += (("IJK",bf_geometry.get_mesh_cells(ob)["ijk"]),)
    if ob.bf_custom_param: pa += ((ob.bf_custom_param,),)
        
    # prepare multiple parameters
    multiple = ""
    xbs = False
    bf_xb = ob.bf_xb
    if bf_nl in nl_params["XB"] and bf_xb != "NONE":
        if   bf_xb == "VOXELS" and bf_geometry.is_manifold(ob.data):
            multiple = "XB"
            xbs = bf_geometry.get_boxels(ob, context.scene.bf_voxel_size, solid=True)
        elif bf_xb == "BBOX":
            xbs = bf_geometry.get_bbox(ob)
        elif bf_xb == "FACES":
            multiple = "XB"
            xbs = bf_geometry.get_faces(ob)
        elif bf_xb == "EDGES":
            multiple = "XB"
            xbs = bf_geometry.get_edges(ob)

    xyzs = False
    bf_xyz = ob.bf_xyz
    if bf_nl in nl_params["XYZ"] and bf_xyz != "NONE":
        if   bf_xyz == "CENTER":
            xyzs = bf_geometry.get_center(ob)
        elif bf_xyz == "VERTS" and not multiple:
            multiple = "XYZ"
            xyzs = bf_geometry.get_vertices(ob)

    pbs = False
    if bf_nl in nl_params["PB"] and ob.bf_pb == "FACES" and not multiple:
        multiple = "PB"
        pbs = bf_geometry.get_planes(ob) # special treatment for PBX PBY PBZ

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
    if multiple: out += (h2(ob.name),)
    for geo_multiple_pa in geo_multiple_pas:
        out += (nl(pa + geo_multiple_pa),)
    return out

###
#
#

def include_config(context):
    """Include auto config or config file"""
    sc = context.scene
    out = tuple()
    # Log message and HEAD
    out += (h1("Generated by BlenderFDS"),
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
    return out

def include_materials(context, obs):
    """Include Blender materials as SURFs"""
    out = tuple()
    # Select Blender materials linked to objects, exclude predefined by FDS
    mas_predefined = bf_config.mas_predefined
    mas = {ob.active_material for ob in obs if ob.active_material and ob.active_material.bf_export and ob.active_material.name not in mas_predefined}
    # Include them
    out += (h1("Boundary condition definitions"),)
    for ma in mas:
        print("BlenderFDS:     ", ma.name)
        # Prepare parameters
        pa = ("SURF",("ID",ma.name),)
        if ma.bf_fyi: pa += (("FYI",ma.bf_fyi),)
        pa += (("RGB",(int(ma.diffuse_color[0]*255.),int(ma.diffuse_color[1]*255.),int(ma.diffuse_color[2]*255.)) ),)
        if ma.use_transparency: pa += (("TRANSPARENCY",ma.alpha),)
        if ma.bf_custom_param: pa += ((ma.bf_custom_param,),)
        out += (nl(pa),)
    return out

def include_objects(context, obs):
    """Include Blender objects as grouped namelists"""
    sc = context.scene
    out = tuple()
    # Group selected Blender objects by namelist group
    obs_groups = tuple() # ((ob1, ob2, ...), ...)
    nl_groups = bf_config.nl_groups # (("Computational domain", ("MESH", "INIT", "ZONE")), ...)
    for nl_group in nl_groups[:-1]: # -1 as the last row collects remainings
        obs_selection = {ob for ob in obs if ob.bf_nl in nl_group[1]}
        obs_groups += (obs_selection, )
        obs -= obs_selection # remove selected obs from the pile
    obs_groups += (obs, ) # remaining obs assigned to last group
    # Include Blender objects as FDS namelists
    for index, obs_group in enumerate(obs_groups):
        out += (h1(nl_groups[index][0]),) # insert group name 
        for ob in obs_group:
            out += include_object(context,ob)
    return out

###
#
#

def save(operator, context, filepath="", export_visible=False):
    """Export Blender data to FDS case file"""
    sc = context.scene
    out = tuple()
    print("BlenderFDS: Start exporting to FDS case file")
    
    # Include auto config or config file
    print("BlenderFDS: Include auto config or config file")
    out += include_config(context)

    # Select Blender objects
    print("BlenderFDS: Select Blender objects")
    if export_visible:
        obs = {ob for ob in sc.objects if ob.type == "MESH" and ob.bf_export and ob.is_visible(sc)}
    else:
        obs = {ob for ob in sc.objects if ob.type == "MESH" and ob.bf_export}
    
    # Include Blender materials as FDS SURFs
    print("BlenderFDS: Include Blender materials as FDS SURFs")
    out += include_materials(context, obs)

    # Include Blender objects as grouped FDS namelists
    print("BlenderFDS: Include Blender objects as grouped FDS namelists")
    out += include_objects(context, obs)
    
    # Close FDS case
    print("BlenderFDS: Close FDS case")
    out += (sp(), nl(["TAIL",]),)
    
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
    print("BlenderFDS: Exporting to FDS case file completed.")   
    return {'FINISHED'}
