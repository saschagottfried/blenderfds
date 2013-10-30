"""BlenderFDS, Blender geometry routines"""

import bpy, bmesh
from mathutils import Vector
from time import time
from blenderfds.types import *
from blenderfds.types.flags import *

### Constants

epsilon = .0001

### Working on Blender objects

def get_global_mesh(context, ob):
    """Return object mesh modified and transformed in global coordinates."""
    me = ob.to_mesh(context.scene, True, "PREVIEW") # apply modifiers
    me.transform(ob.matrix_world) # transform mesh in global coordinates, apply scale, rotation, and location
    return me

def set_global_mesh(context, ob, me):
    """Set object mesh from mesh in global coordinates."""
    try: me.transform(ob.matrix_world.inverted()) # transform global mesh to local coordinates, apply scale, rotation, and location
    except ValueError: pass
    ob.data = me

def get_global_dimensions(context, ob):
    """Get object dimensions in global coordinates."""
    current_xbs, msg = ob_to_xbs_bbox(context, ob)
    x0, x1, y0, y1, z0, z1 = current_xbs[0]
    return abs(x1-x0), abs(y1-y0), abs(z1-z0)

def get_global_area(context, ob):
    """Get area of object in global coordinates."""
    area = 0.
    me = get_global_mesh(context, ob) # Apply modifiers and scales
    for polygon in me.polygons: area += polygon.area
    return area

def get_new_object(context, name="object", me=None):
    """Create new object, named name, set mesh me if any."""
    if not me: me = bpy.data.meshes.new("mesh") # dummy mesh
    ob = bpy.data.objects.new(name, me)
    context.scene.objects.link(ob)
    return ob

def get_object(context, name="Object"):
    """Get or create an object and return it"""
    if name not in bpy.data.objects:
        return get_new_object(context, name)
    return bpy.data.objects[name]

def set_balanced_center_position(context, ob):
    """Set object center position"""
    if context.mode != 'OBJECT': bpy.ops.object.mode_set(mode='OBJECT', toggle=False)
    # origin_set works on currently selected objects
    # unselect all, select ob, set origin, and try to revert selections to original
    active_ob = context.active_object
    bpy.ops.object.select_all(action='DESELECT')
    ob.select = True
    bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY')
    active_ob.select = True

### Working on Blender materials

def get_new_material(name="Material"):
    """Create new material, named name."""
    return bpy.data.materials.new(name)

def get_material(name="Material"):
    """Get or create material and return it"""
    if name not in bpy.data.materials:
        return get_new_material(name)
    return bpy.data.materials[name]

### Working on Blender meshes

def get_tessfaces(context, me):
    """Get bmesh tessfaces."""
    me.update(calc_tessface=True)
    return me.tessfaces

def is_manifold(context, me):
    """Check if mesh me is manifold."""
    bm = bmesh.new()
    bm.from_mesh(me)
    for edge in bm.edges:
        if not edge.is_manifold:
            bm.free()
            return False
    for vert in bm.verts:
        if not vert.is_manifold:
            bm.free()
            return False
    bm.free()
    return True

### Temporary object management

def del_all_tmp_objects(context):
    """Restore all original obs and delete all tmp objects."""
    if context.mode != 'OBJECT': bpy.ops.object.mode_set(mode='OBJECT', toggle=False)
    sc = context.scene
    for ob in sc.objects:
        # Restore original object
        if ob.bf_has_tmp: ob.bf_has_tmp, ob.hide = False, False
        # Unlink and delete temporary object
        if ob.bf_is_tmp:
            sc.objects.unlink(ob)
            bpy.data.objects.remove(ob)
            continue

def set_tmp_object(context, ob, ob_tmp):
    """Link ob_tmp as temporary object of ob."""
    # Set original object
    ob.bf_has_tmp = True
    ob.hide = True
    # Set temporary object
    ob_tmp.name = "{}_tmp".format(ob.name)
    ob_tmp.bf_is_tmp = True
    ob_tmp.active_material = ob.active_material
    ob_tmp.layers = ob.layers
    ob_tmp.show_wire = True
    ob_tmp.select = True

### Translate Blender object geometry to FDS notation

def ob_to_none(context, ob):
    return None, None

def ob_to_xbs_voxels(context, ob):
    """Return a list of object voxels XBs and a msg:
    ((x0,x1,y0,y1,z0,z1,), ...), "Message"."""
    print("BFDS: geometry.ob_to_xbs_voxels:", ob.name) 
    # Init
    t0 = time()
    voxel_size = ob.bf_xb_voxel_size
    progress = True
    if progress:
        wm = context.window_manager
        wm.progress_begin(0, 100)
    # Create a new tmp object from original object, link it to the scene and update the scene
    # The new object mesh has modifiers, scale, rotation, and location applied
    me_tmp = get_global_mesh(context, ob)
    ob_tmp = bpy.data.objects.new("{}_tmp".format(ob.name), me_tmp)
    ob_tmp.bf_is_tmp = True
    # Add new Remesh modifier to the tmp object
    dimension = max(ob_tmp.dimensions) # before appling the modifier
    mo = ob_tmp.modifiers.new('voxels_tmp','REMESH')
    mo.octree_depth, mo.scale, voxel_size, dimension_too_large = _calc_remesh(context, dimension, voxel_size)
    if dimension_too_large: raise BFException(ob, msg="Object '{}' too large for desired voxel size, split it in parts.".format(ob.name))
    mo.mode = 'BLOCKS'
    mo.use_remove_disconnected = False
    # Extract tessfaces from the object mesh as modified by Remesh (modifiers applied)
    if progress: wm.progress_update(20)
    me_tmp = get_global_mesh(context, ob_tmp)
    tessfaces = get_tessfaces(context, me_tmp)
    # Clean unneeded tmp object
    bpy.data.objects.remove(ob_tmp)
    # Classify tessfaces
    if progress: wm.progress_update(30)
    voxel_size_half = voxel_size / 2.
    origin = me_tmp.vertices[0].co
    facezs = dict()
    for tessface in tessfaces:
        center_loc = tessface.center - origin
        if abs(tessface.normal[2]) >.9:  # tessface is normal to z axis
            ix = round((center_loc[0] - voxel_size_half) / voxel_size)
            iy = round((center_loc[1] - voxel_size_half) / voxel_size)
            iz = round( center_loc[2] / voxel_size)
            if (ix, iy) in facezs: facezs[(ix, iy)].append(iz)
            else: facezs[(ix, iy)] = [iz,]
    # Create boxes along z axis, then grow them in x and y direction
    if progress: wm.progress_update(60)
    boxes = set()
    while facezs:
        key, values = facezs.popitem()
        values.sort()
        while values:
            end = values.pop()
            start = values.pop()
            boxes.add((key[0], key[0] + 1, key[1], key[1] + 1, start, end,))
    boxes = _grow_boxes_along_y(_grow_boxes_along_x(boxes))
    # Prepare XBs
    if progress: wm.progress_update(80)
    result = list()
    for box in boxes:
        a = origin + Vector((box[0], box[2], box[4])) * voxel_size
        b = origin + Vector((box[1], box[3], box[5])) * voxel_size
        result.append((a[0]-epsilon, b[0]+epsilon, a[1]-epsilon, b[1]+epsilon, a[2]-epsilon, b[2]+epsilon),)
    # Clean up temporary mesh
    bpy.data.meshes.remove(me_tmp)
    # Return
    if progress: wm.progress_end()
    msg = len(result) > 1 and "{0} voxels of size {1:.3f} m, in {2:.3f} s".format(len(result), voxel_size, time() - t0) or None
    return result, msg
    
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
        else: raise Exception("Building a plane is impossible, problem with received faces")
    result.sort()
    # Nothing to clean up, return
    msg = len(result) > 1 and "{0} planes".format(len(result)) or None
    return result, msg

### Translate geometry in FDS notation to a Blender mesh

def none_to_mesh(value=None, me=None):
    return me or bpy.data.meshes.new("none")

def xbs_edges_to_mesh(xbs, me=None):
    """Translate XB edges ((x0,x1,y0,y1,z0,z1,), ...) to Blender mesh."""
    if not me: me = bpy.data.meshes.new("xbs_edges")
    verts, edges, faces = list(), list(), list()
    for i, xb in enumerate(xbs):
        x0, x1, y0, y1, z0, z1 = xb
        j = i * 2
        verts.extend(((x0,y0,z0), (x1,y1,z1)))
        edges.append((0+j,1+j))
    me.from_pydata(verts, edges, faces)
    me.update(calc_edges=True)
    return me

def xbs_faces_to_mesh(xbs, me=None):
    """Translate XB faces ((x0,x1,y0,y1,z0,z1,), ...) to Blender mesh."""
    if not me: me = bpy.data.meshes.new("xbs_faces")
    verts, edges, faces = list(), list(), list()
    for i, xb in enumerate(xbs):
        x0, x1, y0, y1, z0, z1 = xb
        j = i * 4
        if   abs(x1 - x0) < epsilon: verts.extend(((x0,y0,z0), (x0,y1,z0), (x0,y1,z1), (x0,y0,z1)))
        elif abs(y1 - y0) < epsilon: verts.extend(((x0,y0,z0), (x1,y0,z0), (x1,y0,z1), (x0,y0,z1)))
        elif abs(z1 - z0) < epsilon: verts.extend(((x0,y0,z0), (x0,y1,z0), (x1,y1,z0), (x1,y0,z0)))
        else:
            print("BFDS: geometry.xbs_faces_to_ob: this XB is not a face:", xb)
            continue
        faces.append((0+j,1+j,2+j,3+j))
    me.from_pydata(verts, edges, faces)
    me.update(calc_edges=True)
    return me

def xbs_bbox_to_mesh(xbs, me=None):
    """Translate XB bbox ((x0,x1,y0,y1,z0,z1,), ...) to Blender mesh."""
    if not me: me = bpy.data.meshes.new("xbs_bbox")
    verts, edges, faces = list(), list(), list()
    for i, xb in enumerate(xbs):
        x0, x1, y0, y1, z0, z1 = xb
        j = i * 8
        verts.extend(((x0,y0,z0), (x1,y0,z0), (x1,y1,z0), (x0,y1,z0), (x0,y0,z1), (x1,y0,z1), (x1,y1,z1), (x0,y1,z1)))
        faces.extend(((0+j,3+j,2+j,1+j), (0+j,1+j,5+j,4+j), (0+j,4+j,7+j,3+j), (6+j,5+j,1+j,2+j), (6+j,2+j,3+j,7+j), (6+j,7+j,4+j,5+j)))
    me.from_pydata(verts, edges, faces)
    #me.update(calc_edges=True)
    return me

def xyzs_vertices_to_mesh(xyzs, me=None):
    """Translate XYZ vertices ((x0,y0,z0,), ...) to Blender mesh."""
    if not me: me = bpy.data.meshes.new("xyzs_vertices")
    verts, edges, faces = xyzs, list(), list()
    me.from_pydata(verts, edges, faces)
    me.update(calc_edges=True)
    return me

def pbs_planes_to_mesh(pbs, me=None):
    """Translate PB* planes (("X",x3,), ("X",x7,), ("Y",y9,), ...) to Blender mesh."""
    # Prepare xbs
    xbs = list()
    for i, pb in enumerate(pbs):
        if pb[0] == "X": xbs.append((pb[1], pb[1], -1., +1., -1., +1.))
        elif pb[0] == "Y": xbs.append((-1., +1., pb[1], pb[1], -1., +1.))
        elif pb[0] == "Z": xbs.append((-1., +1., -1., +1., pb[1], pb[1]))
        else:
            print("BFDS: geometry.pbs_planes_to_ob: unrecognized PB*:", pb)
            continue
    # Call companion function
    return xbs_faces_to_mesh(xbs, me)

### Auxiliary functions for ob_to_xbs_voxels()

def _calc_remesh(context, dimension, voxel_size):
    """Calc Remesh modifier parameters."""
    for octree_depth in range(1,11):
        scale = dimension / voxel_size / 2 ** octree_depth
        if 0.010 < scale < 0.990:
            dimension_too_large = False
            break
    if dimension_too_large: scale = 0.990
    voxel_size = dimension / scale / 2 ** octree_depth
    return octree_depth, scale, voxel_size, dimension_too_large

# Grow boxes for voxels->boxels

def _grow_boxes_along_x(boxes):
    """Grow boxes along x axis."""
    boxes_grown = set()
    while boxes:
        box = list(boxes.pop())
        while True: # fatten into +x direction
            box_desired = (box[1], box[1] + 1, box[2], box[3], box[4], box[5],)
            if box_desired not in boxes: break
            boxes.remove(box_desired)
            box[1] += 1
        while True: # fatten into -x direction
            box_desired = (box[0] - 1, box[0], box[2], box[3], box[4], box[5],)
            if box_desired not in boxes: break
            boxes.remove(box_desired)
            box[0] -= 1
        boxes_grown.add(tuple(box))
    return boxes_grown

def _grow_boxes_along_y(boxes):
    """Grow boxes along y axis."""
    boxes_grown = set()
    while boxes:
        box = list(boxes.pop())
        while True: # fatten into +y direction
            box_desired = (box[0], box[1], box[3], box[3] + 1, box[4], box[5],)
            if box_desired not in boxes: break
            boxes.remove(box_desired)
            box[3] += 1
        while True: # fatten into -y direction
            box_desired = (box[0], box[1], box[2] - 1, box[2], box[4], box[5],)
            if box_desired not in boxes: break
            boxes.remove(box_desired)
            box[2] -= 1
        boxes_grown.add(tuple(box))
    return boxes_grown
    
def _grow_boxes_along_z(boxes): # currently not used
    """Grow boxes along z axis."""
    boxes_grown = set()
    while boxes:
        box = list(boxes.pop())
        while True: # fatten into +z direction
            box_desired = (box[0], box[1], box[2], box[3], box[5], box[5] + 1,)
            if box_desired not in boxes: break
            boxes.remove(box_desired)
            box[5] += 1
        while True: # fatten into -z direction
            box_desired = (box[0], box[1], box[2], box[3], box[4] - 1, box[4],)
            if box_desired not in boxes: break
            boxes.remove(box_desired)
            box[4] -= 1
        boxes_grown.add(tuple(box))
    return boxes_grown

### Geometry choice matrix for functions

choose_to_geometry = {
    "xbs": {
        "NONE"    : (ob_to_none, none_to_mesh,),
        "BBOX"    : (ob_to_xbs_bbox, xbs_bbox_to_mesh,),
        "VOXELS"  : (ob_to_xbs_voxels, xbs_bbox_to_mesh,),
        "FACES"   : (ob_to_xbs_faces, xbs_faces_to_mesh,),
        "EDGES"   : (ob_to_xbs_edges, xbs_edges_to_mesh,),
    },
    "xyzs": {
        "NONE"    : (ob_to_none, none_to_mesh,),
        "CENTER"  : (ob_to_xyzs_center, xyzs_vertices_to_mesh,),
        "VERTICES": (ob_to_xyzs_vertices, xyzs_vertices_to_mesh,),
    },
    "pbs": {
        "NONE"    : (ob_to_none, none_to_mesh,),
        "PLANES"  : (ob_to_pbs_planes, pbs_planes_to_mesh,),
    },
}

### Get Blender object geometry in FDS notation

def ob_to_xbs(context, ob):
    """Blender object geometry to FDS notation"""
    return choose_to_geometry["xbs"][ob.bf_xb][0](context, ob)

def ob_to_xyzs(context, ob):
    """Blender object geometry to FDS notation"""
    return choose_to_geometry["xyzs"][ob.bf_xyz][0](context, ob)

def ob_to_pbs(context, ob):
    """Blender object geometry to FDS notation"""
    return choose_to_geometry["pbs"][ob.bf_pb][0](context, ob)

### Set Blender object geometry from FDS notation
# If no ob, a new one (named name) is created and returned
# If no bf_xb, bf_xyz, bf_pb, a guess is made from data

def xbs_to_ob(xbs, context, ob=None, bf_xb="NONE", name="xbs_to_ob", update_center=True):
    """Geometry in FDS notation to Blender object"""
    # Choose bf_xb
    if bf_xb == "NONE":
        x0, x1, y0, y1, z0, z1 = xbs[0]
        if abs(x1-x0) < epsilon or abs(y1-y0) < epsilon or abs(z1-z0) < epsilon: bf_xb = "FACES"
        else: bf_xb = "BBOX"
    # Get mesh, set it, set properties and center position
    me = choose_to_geometry["xbs"][bf_xb][1](xbs)
    if ob: set_global_mesh(context, ob, me) # ob exists, set its mesh
    else: ob = get_new_object(context, name, me) # no ob, get a new one with proper mesh
    ob.bf_xb = bf_xb
    if update_center: set_balanced_center_position(context, ob)
    return ob

def xyzs_to_ob(xyzs, context, ob=None, bf_xyz="NONE", name="xyzs_to_ob", update_center=True):
    """Geometry in FDS notation to Blender object"""
    # Choose bf_xyz
    if bf_xyz == "NONE": bf_xyz = "VERTICES"
    # Get mesh, set it, set properties and center position
    me = choose_to_geometry["xyzs"][bf_xyz][1](xyzs)
    if ob: set_global_mesh(context, ob, me) # ob exists, set its mesh
    else: ob = get_new_object(context, name, me) # no ob, get a new one with proper mesh
    ob.bf_xyz = bf_xyz
    if update_center: set_balanced_center_position(context, ob)
    return ob
    
def pbs_to_ob(pbs, context, ob=None, bf_pb="NONE", name="pbs_to_ob", update_center=True):
    """Geometry in FDS notation to Blender object."""
    # Choose bf_pb
    if bf_pb == "NONE": bf_pb = "PLANES"
    # Get mesh, set it, set properties and center position
    me = choose_to_geometry["pbs"][bf_pb][1](pbs)
    if ob: set_global_mesh(context, ob, me) # ob exists, set its mesh
    else: ob = get_new_object(context, name, me) # no ob, get a new one with proper mesh
    ob.bf_pb = bf_pb
    if update_center: set_balanced_center_position(context, ob)
    return ob

### Show geometries

def show_ob_fds_geometries(context, ob):
    """Create temporary objects from object exported FDS geometries. Return report arguments."""
    # Init
    if context.mode != 'OBJECT': bpy.ops.object.mode_set(mode='OBJECT', toggle=False)
    msgs = list()
    # Manage XB: get coordinates, show them in a tmp object, prepare msg
    xbs = None
    try: xbs, msg  = ob_to_xbs(context, ob)
    except: print("BFDS: show_ob_fds_geometries: error in xbs for object '{}'".format(ob.name))
    if xbs:
        ob_tmp = xbs_to_ob(xbs, context, bf_xb=ob.bf_xb)
        set_tmp_object(context, ob, ob_tmp)
        if msg: msgs.append(msg)
    # Manage XYZ: get coordinates, show them in a tmp object, prepare msg
    xyzs = None
    try: xyzs, msg = ob_to_xyzs(context, ob)
    except: print("BFDS: show_ob_fds_geometries: error in xyzs for object '{}'".format(ob.name))
    if xyzs:
        ob_tmp = xyzs_to_ob(xyzs, context, bf_xyz=ob.bf_xyz)
        set_tmp_object(context, ob, ob_tmp)
        if msg: msgs.append(msg)
    # Manage PB*: get coordinates, show them in a tmp object, prepare msg
    pbs  = None        
    try: pbs, msg  = ob_to_pbs(context, ob)
    except: print("BFDS: show_ob_fds_geometries: error in pbs for object '{}'".format(ob.name))
    if pbs:
        ob_tmp = pbs_to_ob(pbs, context, bf_pb=ob.bf_pb)
        set_tmp_object(context, ob, ob_tmp)
        if msg: msgs.append(msg)
    # Return report
    if xbs or xyzs or pbs:
        if msgs: return {"INFO"}, ", ".join(msgs)
        else: return {"INFO"}, "FDS geometries shown"
    else: return {"WARNING"}, "Nothing to show"
