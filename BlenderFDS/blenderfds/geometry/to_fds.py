"""BlenderFDS, translate Blender object geometry to FDS notation"""

import bpy
from time import time
from blenderfds.geometry.utilities import *
from blenderfds.geometry.voxelize import voxelize # FIXME

DEBUG = True

### to None

def ob_to_none(context, ob):
    return None, None

### to XB

# FIXME common code in pixels and voxels!!!

def ob_to_xbs_pixels(context, ob):
    """Return a list of object flat voxels XBs and a msg:
    ((x0,x0,y0,y1,z0,z1,), ...), "Message"."""
    print("BFDS: geometry.ob_to_xbs_pixels:", ob.name) 
    # Init
    t0 = time()
    voxel_size = ob.bf_xb_voxel_size
    ob_tmp = _get_absolute_tmp_object(context, ob)
    # Check and set location
    if not ob_tmp.data.vertices: return None, "Empty object. No pixel created."
    location = ob_tmp.data.vertices[0].co
    # Choose flat dimension or return None
    if   ob_tmp.dimensions[0] < epsilon: normal = "x" # the face is normal to x axis
    elif ob_tmp.dimensions[1] < epsilon: normal = "y" # ...to y axis
    elif ob_tmp.dimensions[2] < epsilon: normal = "z" # ...to z axis
    else: raise BFException(sender=ob, msg="Not flat and normal to axis, no pixels created.".format(ob.name))
    # Solidify and remesh
    _apply_solidify_modifier(context, ob_tmp, thickness=voxel_size/3.)
    _apply_remesh_modifier(context, ob_tmp, voxel_size)
    # Get absolute faces
    me_tmp = get_global_mesh(context, ob_tmp)
    tessfaces = get_tessfaces(context, me_tmp)
    if not tessfaces: return None, "No pixel created."
    # Sort tessfaces centers by face normal
    t1 = time()
    x_tessfaces, y_tessfaces, z_tessfaces = _sort_tessfaces_by_normal(tessfaces)
    # Choose procedure FIXME clean up
    t2 = time()
    choice = (len(x_tessfaces), len(y_tessfaces), len(z_tessfaces))
    sorted_choice = sorted(choice)
    choice0 = choice.index(sorted_choice[0]) # 1st min
    choice1 = choice.index(sorted_choice[1]) # 2nd 
    choice2 = choice.index(sorted_choice[2]) # 3nd max
    # Create boxes
    t3 = time()
    if   choice0 == 0:
        x_floors = _get_x_floors(x_tessfaces, voxel_size)
        boxes = _x_floors_to_boxes(x_floors)
    elif choice0 == 1:
        y_floors = _get_y_floors(y_tessfaces, voxel_size)
        boxes = _y_floors_to_boxes(y_floors)
    else:
        z_floors = _get_z_floors(z_tessfaces, voxel_size)
        boxes = _z_floors_to_boxes(z_floors)
    # Grow boxes along the other two directions
    t4 = time()
    if   choice1 == 0: boxes = _grow_boxes_along_x(boxes)
    elif choice1 == 1: boxes = _grow_boxes_along_y(boxes)
    else: boxes = _grow_boxes_along_z(boxes)
    t5 = time()
    if   choice2 == 0: boxes = _grow_boxes_along_x(boxes)
    elif choice2 == 1: boxes = _grow_boxes_along_y(boxes)
    else: boxes = _grow_boxes_along_z(boxes)
    t6 = time()
    # Prepare XBs
    xbs = _boxes_to_xbs(boxes, voxel_size, flat=True, normal=normal, location=location)
    # Clean unneeded tmp object and tmp mesh
    bpy.data.objects.remove(ob_tmp) # DEBUG comment out to leave tmp object
    bpy.data.meshes.remove(me_tmp) # DEBUG comment out to leave tmp object
    #bpy.context.scene.objects.link(ob_tmp) # DEBUG uncomment to leave temp object
    # Return
    msg = "{0} pixels, normal to {1} axis, resolution {2:.3f} m, in {3:.3f} s".format(len(xbs), normal, voxel_size, time()-t0) or None
    if DEBUG and msg: msg += " (s:{0:.0f} c:{1:.0f} fb:{2:.0f}, g1:{3:.0f}, g2:{4:.0f})".format(t2-t1, t3-t2, t4-t3, t5-t4, t6-t5)
    return xbs, msg    

def ob_to_xbs_voxels(context, ob):
    """Return a list of object voxels XBs and a msg:
    ((x0,x1,y0,y1,z0,z1,), ...), "Message"."""
    print("BFDS: geometry.ob_to_xbs_voxels:", ob.name)
    t0 = time()
    xbs = voxelize(context, ob, flat=False, normal=None, location=None)
    if not xbs: return None, "No voxel created"
    msg = "{0} voxels, resolution {1:.3f} m, in {2:.0f} s".format(len(xbs), ob.bf_xb_voxel_size, time()-t0) or None
    #if DEBUG and msg:
    #    msg += " (s:{0:.0f} c:{1:.0f} fb:{2:.0f}, g1:{3:.0f}, g2:{4:.0f})".format(t2-t1, t3-t2, t4-t3, t5-t4, t6-t5)
    return xbs, msg


# Other

def ob_to_xbs_bbox(context, ob):
    """Return a list of object bounding box XBs and a msg:
    ((x0,x1,y0,y1,z0,z1,), ...), "Message"."""
    # Init
    me_tmp = get_global_mesh(context, ob)
    # Check at least one vertex
    if not me_tmp.vertices:
        bpy.data.meshes.remove(me_tmp)
        location = ob.location
        return ((location[0], location[0], location[1], location[1], location[2], location[2],),), None
    # Calc the bounding box in global coordinates and clean
    bbminx, bbminy, bbminz = me_tmp.vertices[0].co
    bbmaxx, bbmaxy, bbmaxz = me_tmp.vertices[0].co
    for vertex in me_tmp.vertices:
        x, y, z = vertex.co
        bbminx, bbminy, bbminz = min(bbminx, x), min(bbminy, y), min(bbminz, z)
        bbmaxx, bbmaxy, bbmaxz = max(bbmaxx, x), max(bbmaxy, y), max(bbmaxz, z)
    # Clean up temporary mesh
    bpy.data.meshes.remove(me_tmp)
    # Return
    return [(bbminx, bbmaxx, bbminy, bbmaxy, bbminz, bbmaxz,),], None

def ob_to_xbs_faces(context, ob):
    """Return a list of object faces straightened bounding boxes XBs and a msg:
    ((x0,x0,y0,y1,z0,z1,), ...), "Message"."""
    # Init
    result = list()
    me = get_global_mesh(context, ob)
    tessfaces = get_tessfaces(context, me)
    # For each tessface...
    for tessface in tessfaces:
        vertices = [me.vertices[vertex] for vertex in tessface.vertices]
        # Calc the bounding box in global coordinates
        bbminx, bbminy, bbminz = vertices[0].co
        bbmaxx, bbmaxy, bbmaxz = vertices[0].co
        for vertex in vertices[1:]:
            x, y, z = vertex.co
            bbminx, bbminy, bbminz = min(bbminx, x), min(bbminy, y), min(bbminz, z)
            bbmaxx, bbmaxy, bbmaxz = max(bbmaxx, x), max(bbmaxy, y), max(bbmaxz, z)
        bbd = [(bbmaxx - bbminx, 2), (bbmaxy - bbminy, 1), (bbmaxz - bbminz, 0)]
        bbd.sort()
        if bbd[0][1] == 2: bbmaxx = bbminx = (bbminx+bbmaxx)/2
        if bbd[0][1] == 1: bbmaxy = bbminy = (bbminy+bbmaxy)/2
        if bbd[0][1] == 0: bbmaxz = bbminz = (bbminz+bbmaxz)/2
        result.append((bbminx, bbmaxx, bbminy, bbmaxy, bbminz, bbmaxz,),)
    result.sort()
    # Clean up
    bpy.data.meshes.remove(me)
    # Return
    msg = len(result) > 1 and "{0} faces".format(len(result)) or None
    return result, msg

def ob_to_xbs_edges(context, ob):
    """Return a list of object edges XBs and a msg:
    ((x0,x1,y0,y1,z0,z1,), ...), "Message"."""
    # Init
    result = list()
    me = get_global_mesh(context, ob)
    # For each edge...
    for edge in me.edges:
        pt0x, pt0y, pt0z = me.vertices[edge.vertices[0]].co
        pt1x, pt1y, pt1z = me.vertices[edge.vertices[1]].co
        result.append((pt0x, pt1x, pt0y, pt1y, pt0z, pt1z,),)
    result.sort()
    # Clean up
    bpy.data.meshes.remove(me)
    # Return
    msg = len(result) > 1 and "{0} edges".format(len(result)) or None
    return result, msg

# Caller function (ob.bf_xb)

choose_to_xbs = {
    "NONE"   : ob_to_none,
    "BBOX"   : ob_to_xbs_bbox,
    "VOXELS" : ob_to_xbs_voxels,
    "FACES"  : ob_to_xbs_faces,
    "PIXELS" : ob_to_xbs_pixels,
    "EDGES"  : ob_to_xbs_edges,
}

def ob_to_xbs(context, ob):
    """Blender object geometry according to ob.bf_xb to FDS notation"""
    return choose_to_xbs[ob.bf_xb](context, ob)

### XYZ

def ob_to_xyzs_vertices(context, ob):
    """Return a list of object vertices XYZs and a msg:
    ((x0,y0,z0,), ...), "Message"."""
    # Init
    result = list()
    me = get_global_mesh(context, ob)
    # For each vertex...
    for vertex in me.vertices:
        pt0x, pt0y, pt0z = vertex.co
        result.append((pt0x, pt0y, pt0z,),)
    result.sort()
    # Clean up
    bpy.data.meshes.remove(me)
    # Return
    msg = len(result) > 1 and "{0} vertices".format(len(result)) or None
    return result, msg

def ob_to_xyzs_center(context, ob):
    """Return a list of one object center XYZ and a msg:
    ((x0,y0,z0,), ...), "Message"."""
    return [(ob.location[0], ob.location[1], ob.location[2],),], None

# Caller function (ob.bf_xyz)

choose_to_xyzs = {
    "NONE"     : ob_to_none,
    "CENTER"   : ob_to_xyzs_center,
    "VERTICES" : ob_to_xyzs_vertices,
}

def ob_to_xyzs(context, ob):
    """Blender object geometry according to ob.bf_xyz to FDS notation"""
    return choose_to_xyzs[ob.bf_xyz](context, ob)

### PB

def ob_to_pbs_planes(context, ob):
    """Return a list of object planes with orientation and coordinate for PB and a msg:
    (("X",x3,), ("X",x7,), ("Y",y9,), ...), "Message"."""
    # Init
    result = list()
    xbs, msg = ob_to_xbs_faces(context, ob)
    # For each face build a plane...
    for xb in xbs:
        if   abs(xb[1] - xb[0]) < epsilon: result.append(("X", xb[0],),)
        elif abs(xb[3] - xb[2]) < epsilon: result.append(("Y", xb[2],),)
        elif abs(xb[5] - xb[4]) < epsilon: result.append(("Z", xb[4],),)
        else: raise ValueError("BFDS: Building planes impossible, problem in ob_to_xbs_faces.")
    result.sort()
    # Nothing to clean up, return
    msg = len(result) > 1 and "{0} planes".format(len(result)) or None
    return result, msg

# Caller function (ob.bf_pb)

choose_to_pbs = {
    "NONE"   : ob_to_none,
    "PLANES" : ob_to_pbs_planes,
}

def ob_to_pbs(context, ob):
    """Blender object geometry according to ob.bf_pb to FDS notation"""
    return choose_to_pbs[ob.bf_pb](context, ob)

