# ##### BEGIN GPL LICENSE BLOCK #####
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
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

from blenderfds import bf_geometry, bf_config
from time import time, strftime, localtime

### Export formatting
#
#

def to_nl(nl_name, params):
    """Format full namelist and manages carriage returns"""
    col_max = bf_config.col_max
    row = 0
    result = "&{0} {1[0]}".format(nl_name, params)
    for param in params[1:]:
        result_tmp = "{0}, {1}".format(result, param)
        if len(result_tmp) // col_max > row: # exceeds max row columns
            result_tmp = "{0},\n      {1}".format(result, param)
            row += 1
        result = result_tmp
    return "{} /\n".format(result)

def to_head(content):
    """Format heading"""
    return "!!! {}\n\n".format(content, )

def to_desc(content):
    """Format description"""
    return "! {}\n".format(content, )

def to_foot():
    """Format footer"""
    return "\n"

def to_single_quotes(string):
    """Format to single quotes"""
    return string.replace('"',"'")
    
def to_no_quotes(string):
    """Format to no quotes"""
    return string.replace("'","_").replace('"',"_")
    
def to_file_name(string):
    """Format to file name"""
    string = ''.join(c for c in string if c in " abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789()-_")
    if string: return string
    else: return "ERROR"
   
### Building FDS parameters from objects with message
# input:  ob
# output: ( "PARAM=VALUE",  ) or
#         (("PARAM=VALUE", ))

def get_xbs(ob, voxel_size):
    """Get XB values"""
    ok = False
    choice = ob.bf_xb
    if not ok and choice == "voxels":
        # Import Psyco if available
        try:
            import psyco
            psyco.full()
        except ImportError:
            pass
        # Check and execute
        if not bf_geometry.is_manifold(ob.data): return (("XB='ERROR The object is not manifold'",), )
        xbs = bf_geometry.get_boxels(ob, voxel_size, solid=True)
        ok = True
    if not ok and choice == "bbox":
        xbs = bf_geometry.get_bb(ob)
        ok = True
    if not ok and choice == "faces":
        xbs = bf_geometry.get_faces(ob)
        ok = True
    if not ok and choice == "edges":
        xbs = bf_geometry.get_edges(ob)
        ok = True
    if ok:
        result = list()
        for xb in xbs:
            result.append("XB={0[0]:.3f},{0[1]:.3f},{0[2]:.3f},{0[3]:.3f},{0[4]:.3f},{0[5]:.3f}".format(xb))
        return (result, )
    return (("ERROR Choice '{}' is not implemented".format(choice, ),), )

def get_xyzs(ob):
    """Get XYZ values"""
    ok = False
    choice = ob.bf_xyz
    if not ok and choice == "verts":
        xyzs = bf_geometry.get_verts(ob)
        ok = True
    if not ok and choice == "center":
        xyzs = bf_geometry.get_center(ob)
        ok = True
    if ok:
        result = list()
        for xyz in xyzs:
            result.append("XYZ={0[0]:.3f},{0[1]:.3f},{0[2]:.3f}".format(xyz))
        return (result, )
    return (("ERROR Choice '{}' is not implemented".format(choice, ),), )
    
def get_pbs(ob):
    """Get PB* values"""
    result = list()
    choice = ob.bf_pb
    if choice == "faces":
        xbs = bf_geometry.get_faces(ob)
        for xb in xbs:
            if xb[1] - xb[0] < .000001:
                result.append("PBX={:.3f}".format(xb[0],))
                continue
            if xb[3] - xb[2] < .000001:
                result.append("PBY={:.3f}".format(xb[2],))
                continue
            if xb[5] - xb[4] < .000001:
                result.append("PBZ={:.3f}".format(xb[4],))
                continue
        result.sort()
        return (result, )
    return (("ERROR Choice '{}' is not implemented".format(choice, ),), )

def get_ijk(ob):
    """Get optimized and valid value for MESH IJK"""
    mesh_cells = bf_geometry.get_mesh_cells(ob)
    result = "IJK={0[0]},{0[1]},{0[2]}".format(mesh_cells[0])
    msg = "IJK={0[0]},{0[1]},{0[2]} ({1} cells, actual size {2[0]:.3f}, {2[1]:.3f}, {2[2]:.3f})".format(mesh_cells[0], mesh_cells[1], mesh_cells[2])
    cell_size = mesh_cells[2]
    ui = "IJK={0[0]},{0[1]},{0[2]} ({1} cells)".format(mesh_cells[0], mesh_cells[1])
    return (result, msg, cell_size, ui)

### Building FDS parameters from objects without message
# input:  ob
# output: "PARAM=VALUE" or
#         ("PARAM=VALUE", )

def get_id(ob, index=None):
    """Format ID from an object"""
    if index: return "ID='{}_{}'".format(ob.name, index)
    else: return "ID='{}'".format(ob.name, )

def get_fyi(ob):
    """Format FYI from an object"""
    return "FYI='{}'".format(ob.bf_fyi)

def get_surf_id(ob):
    """Format SURF_ID from an object"""
    return "SURF_ID='{}'".format(ob.active_material.name)

def get_rgb(ma):
    """Get RGB from a material"""
    return "RGB={:.0f},{:.0f},{:.0f}".format(ma.diffuse_color[0]*255.,
                             ma.diffuse_color[1]*255.,
                             ma.diffuse_color[2]*255. )

def get_transparency(ma):
    """Get TRANSPARENCY from a material"""
    return "TRANSPARENCY={:.3f}".format(ma.alpha)

def get_custom_param(ob):
    """Format custom param from an object"""
    return ob.bf_custom_param

def get_sawtooth(ob):
    """Format SAWTOOTH param from an object"""
    return "SAWTOOTH=.False."

### Helper routines
#
#

def include_header(context, filename):
    """Include header file"""
    out = list()
    sc = context.scene
    
    # Automatic message
    out.append(to_desc("Generated by BlenderFDS (Version: {})".format(bf_config.version)))
    out.append(to_desc(strftime("%a, %d %b %Y %H:%M:%S", localtime()) ))
    out.append(to_desc("Voxel Size: {0[0]:.3f}, {0[1]:.3f}, {0[2]:.3f}".format(context.scene.bf_voxel_size) ))
    out.append(to_foot())

    # HEAD
    out.append(to_nl("HEAD", ("CHID='{}', TITLE='{}'".format(sc.name, sc.bf_case_title or sc.name), )) )
    out.append(to_foot())
    
    # Include external header file
    if sc.bf_type_of_header == "file":
        out.append(to_desc("Start of header file"))
        try:
            with open(context.scene.bf_header_file_path) as in_file:
                for line in in_file: out.append(line)
        except IOError:
            out.append(to_desc("ERROR Header file not found."))
        out.append(to_desc("End of header file"))
        out.append(to_foot())
        
    return out

def include_materials(context, obs):
    """ Export Blender materials"""
    # Materials: only those referenced by a surf_id
    mas = {ob.active_material for ob in obs if ob.active_material and ob.bf_surf_id} # set() comprehension
    # Init
    out = list()
    out.append(to_head("Boundary conditions defs"))
    nl_name = "SURF"
    for ma in mas:
        if ma.name in bf_config.mas_predefined: continue # do not export predefined materials
        params = list()
        params.append(get_id(ma))
        if ma.bf_rgb: params.append(get_rgb(ma))
        if ma.bf_transparency: params.append(get_transparency(ma))
        if ma.bf_custom_param: params.append(get_custom_param(ma))
        if ma.bf_fyi: params.append(get_fyi(ma))
        out.append(to_nl(nl_name, params))
    out.append(to_foot())
    return out

def include_objects(context, obs):
    """ Export objects"""
    out = list()
    for ob in obs:
        nl_name = ob.bf_nl_name
        xbs, xyzs, pbs = list(), list(), list()
        if not ob.bf_xb  == "none":
            result = get_xbs(ob, context.scene.bf_voxel_size)
            xbs = result[0]
        if not ob.bf_xyz == "none":
            result = get_xyzs(ob)
            xyzs = result[0]
        if not ob.bf_pb  == "none":
            result = get_pbs(ob)
            pbs = result[0]
        # Single geometric params
        params = list()
        if len(xbs) == 1:  params.append(xbs[0])
        if len(xyzs) == 1: params.append(xyzs[0])
        if len(pbs) == 1:  params.append(pbs[0])
        # Other params
        if ob.bf_ijk:
            result = get_ijk(ob)
            params.append(result[0])
            out.append(to_desc(result[1]))
        if ob.bf_surf_id: # the active_material check is in the UI
            params.append(get_surf_id(ob))
        if ob.bf_sawtooth: params.append(get_sawtooth(ob))
        if ob.bf_custom_param: params.append(ob.bf_custom_param)
        if ob.bf_fyi: params.append(get_fyi(ob))
        # Check multiple lines for geometry
        geoms = []
        if len(xbs)  > 1: geoms = xbs
        if len(xyzs) > 1: geoms = xyzs
        if len(pbs) > 1: geoms = pbs
        # Write to out
        if geoms:
            out.append(to_desc("Start object"))
            for index, geom in enumerate(geoms):
                params_g = params[:] # copy, not reference
                if ob.bf_id: params_g.insert(0, get_id(ob, index))
                params_g.insert(1, geom)
                out.append(to_nl(nl_name, params_g))
            out.append(to_desc("End object"))
        else:
            if ob.bf_id: params.insert(0, get_id(ob))
            out.append(to_nl(nl_name, params))
    return out
     
def write_to_file(context, directory, filename, out):
    """Write to fds file"""
    if not filename.lower().endswith('.fds'):
        filename += '.fds'
    with open(directory + filename, "w") as out_file:
        for line in out:
            out_file.write(line)

### Main routine
#
#

def export_fds(context, directory, filename, export_visible):
    """Export Blender materials and objects to FDS file"""
    # Start timer, prepare tmp out list
    t0 = time()
    out = list()
    
    # Get objects: export_visible -> visibility, Blender MESH type, exported object
    if export_visible:
        obs = {ob for ob in context.scene.objects if ob.is_visible() and ob.type == "MESH" and ob.bf_nl_export}
    else: obs = {ob for ob in context.scene.objects if ob.type == "MESH" and ob.bf_nl_export}

    # Include header file
    out += include_header(context, filename)
    
    # Export Blender materials
    out += include_materials(context, obs)
        
    # Group objects
    obs_groups = list() # ({ob1, ob2, ...}, ...)
    nl_groups = bf_config.nl_groups # [("Computational domain", ("MESH", "INIT", "ZONE")),...]
    
    for nl_group in nl_groups[:-1]: # the last row collects remainings
        obs_tmp = {ob for ob in obs if ob.bf_nl_name in nl_group[1]}
        obs_groups.append(obs_tmp)
        obs -= obs_tmp
               
    obs_groups.append(obs) # remaining obs assigned to last group
        
    # Export Blender objects
    for index, nl_group in enumerate(nl_groups):
        out.append(to_head(nl_group[0]))
        out += include_objects(context, obs_groups[index])
        out.append(to_foot())

    # Close and write to file
    out.append(to_nl("TAIL", ("",)))
    out.append(to_desc("Generated in {:.3f} sec".format(time()-t0, )))
    write_to_file(context, directory, filename, out)

