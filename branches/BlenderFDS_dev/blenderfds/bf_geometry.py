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

import bpy, bmesh
from mathutils import Vector
from time import time

### Working on Blender objects and meshes

def _get_global_mesh(context, ob):
    """Return object mesh modified and transformed in global coordinates."""
    me = ob.to_mesh(context.scene, True, "PREVIEW") # apply modifiers
    me.transform(ob.matrix_world) # transform mesh in global coordinates, apply scale, rotation, and location
    return me

def _get_tessfaces(context, me):
    """Get bmesh tessfaces"""
    me.update(calc_tessface=True)
    return me.tessfaces

def is_manifold(context, me):
    """Check if mesh me is manifold"""
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

def calc_remesh(context, dimension, voxel_size):
    """Calc Remesh modifier parameters"""
    dimension_too_large = True
    for octree_depth in range(1,11):
        scale = dimension / voxel_size / 2 ** octree_depth
        if 0.010 < scale < 0.990:
            dimension_too_large = False
            break
    if dimension_too_large: scale = 0.990
    voxel_size = dimension / scale / 2 ** octree_depth
    return octree_depth, scale, voxel_size, dimension_too_large

### Extracting coordinates from objects
# input:  context,ob
# output for XB:  ((x0,x1,y0,y1,z0,z1,), ...)
#              or  (x0,x1,y0,y1,z0,z1,)
# output for XYZ: ((x0,y0,z0,), ...)
#              or  (x0,y0,z0,)
# output for PB:  (("X",x0,), ("X",x1,), ("Y",y0,), ...)

def get_voxels(context, ob):
    """Return a list of object voxels XBs, time, and error"""
    # Init
    t0 = time()
    sc = context.scene
    voxel_size = ob.bf_voxel_size
    epsilon = 0.001
    # Create a new tmp object from original object, link it to the scene and update the scene
    # The new object mesh has modifiers, scale, rotation, and location applied
    me_new = _get_global_mesh(context, ob)
    ob_new = bpy.data.objects.new("{0}_tmp".format(ob.name), me_new)
    ob_new.bf_is_voxels = True
    sc.objects.link(ob_new)
    sc.update()
    # Add new Remesh modifier to the tmp object
    dimension = max(ob_new.dimensions) # before appling the modifier
    mo = ob_new.modifiers.new('voxels_tmp','REMESH')
    mo.octree_depth, mo.scale, voxel_size, dimension_too_large = calc_remesh(context, dimension, voxel_size)
    mo.mode = 'BLOCKS'
    mo.remove_disconnected_pieces = False
    # Extract tessfaces from the object mesh as modified by Remesh (modifiers applied)
    me_new = _get_global_mesh(context, ob_new)
    tessfaces = _get_tessfaces(context, me_new)
    # Clean unneeded tmp object
    ob_new.modifiers.remove(mo)
    sc.objects.unlink(ob_new)
    bpy.data.objects.remove(ob_new)
    # Classify tessfaces
    voxel_size_half = voxel_size / 2.
    origin = me_new.vertices[0].co
    facezs = dict()
    for tessface in tessfaces:
        center_loc = tessface.center - origin
        if abs(tessface.normal[2]) >.9:  # tessface is normal to z axis
            ix = round((center_loc[0] - voxel_size_half) / voxel_size)
            iy = round((center_loc[1] - voxel_size_half) / voxel_size)
            iz = round( center_loc[2] / voxel_size)
            if (ix, iy) in facezs:
                facezs[(ix, iy)].append(iz)
            else:
                facezs[(ix, iy)] = [iz,]
    # Create boxes along z axis, then grow them in x and y direction
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
    result = list()
    for box in boxes:
        a = origin + Vector((box[0], box[2], box[4])) * voxel_size
        b = origin + Vector((box[1], box[3], box[5])) * voxel_size
        result.append((a[0]-epsilon, b[0]+epsilon, a[1]-epsilon, b[1]+epsilon, a[2]-epsilon, b[2]+epsilon),)
    # Clean up temporary mesh
    bpy.data.meshes.remove(me_new)
    # Timing and return
    tt = time() - t0
    return result, tt, dimension_too_large

def get_bbox(context, ob):
    """Return a tuple of object bounding box XBs, and time"""
    # Init
    t0 = time()
    me = _get_global_mesh(context, ob)
    # Check at least one vertex
    if not me.vertices:
        bpy.data.meshes.remove(me)
        location = ob.location
        return [(location[0], location[0], location[1], location[1], location[2], location[2],), ]
    # Calc the bounding box in global coordinates and clean
    bbminx, bbminy, bbminz = me.vertices[0].co
    bbmaxx, bbmaxy, bbmaxz = me.vertices[0].co
    for vertex in me.vertices:
        x, y, z = vertex.co
        bbminx, bbminy, bbminz = min(bbminx, x), min(bbminy, y), min(bbminz, z)
        bbmaxx, bbmaxy, bbmaxz = max(bbmaxx, x), max(bbmaxy, y), max(bbmaxz, z)
    # Clean up and return
    bpy.data.meshes.remove(me)
    # Timing and return
    tt = time() - t0
    return ((bbminx, bbmaxx, bbminy, bbmaxy, bbminz, bbmaxz,),), tt

def get_faces(context, ob):
    """Return a list of object faces straightened bounding boxes XBs, and time"""
    # Init
    t0 = time()
    result = list()
    me = _get_global_mesh(context, ob)
    tessfaces = _get_tessfaces(context, me)
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
    # Timing and return
    tt = time() - t0
    return result, tt

def get_planes(context, ob):
    """Return a list of object planes with orientation and coordinate for PB, and time"""
    # Init
    t0 = time()
    result = list()
    xbs, tt = get_faces(context, ob)
    # For each face build a plane...
    for xb in xbs:
        if   xb[1] - xb[0] < .000001: result.append(("X", xb[0],),)
        elif xb[3] - xb[2] < .000001: result.append(("Y", xb[2],),)
        elif xb[5] - xb[4] < .000001: result.append(("Z", xb[4],),)
    result.sort()
    # Nothing to clean up
    # Timing and return
    tt = time() - t0
    return result, tt

def get_edges(context, ob):
    """Return a list of object edges XBs, and time"""
    # Init
    t0 = time()
    result = list()
    me = _get_global_mesh(context, ob)
    # For each edge...
    for edge in me.edges:
        pt0x, pt0y, pt0z = me.vertices[edge.vertices[0]].co
        pt1x, pt1y, pt1z = me.vertices[edge.vertices[1]].co
        result.append((pt0x, pt1x, pt0y, pt1y, pt0z, pt1z,),)
    result.sort()
    # Clean up
    bpy.data.meshes.remove(me)
    # Timing and return
    tt = time() - t0
    return result, tt

def get_vertices(context, ob):
    """Return a list of object vertices XYZs, and time"""
    # Init
    t0 = time()
    result = list()
    me = _get_global_mesh(context, ob)
    # For each vertex...
    for vertex in me.vertices:
        pt0x, pt0y, pt0z = vertex.co
        result.append((pt0x, pt0y, pt0z,),)
    result.sort()
    # Clean up
    bpy.data.meshes.remove(me)
    # Timing and return
    tt = time() - t0
    return result, tt

def get_center(context, ob):
    """Return a tuple of object center XYZ, and time"""
    return ((ob.location[0], ob.location[1], ob.location[2],),), 0.

### Grow boxes

def _grow_boxes_along_x(boxes):
    """Grow boxes along x axis"""
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
    """Grow boxes along y axis"""
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
    """Grow boxes along z axis"""
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

### Manage mesh cells for FDS MESH object

def _factor(n):
    """Generator for prime factors of n.
Many thanks Dhananjay Nene (http://dhananjaynene.com/)
for publishing this code"""
    yield 1  
    i = 2  
    limit = n**0.5  
    while i <= limit:  
        if n % i == 0:  
            yield i
            n = n / i
            limit = n**0.5  
        else:
            i += 1  
    if n > 1:  
        yield int(n)

def _n_for_poisson(n):
    """Get a good number for poisson solver at least bigger than n"""
    good = set((1, 2, 3, 5))
    while True:
        if [i for i in _factor(n) if i not in good]: n += 1
        else: break
    return n

def get_good_ijk(ob):
    """Get a good IJK near the desired"""
    return ob.bf_mesh_ijk[0], _n_for_poisson(ob.bf_mesh_ijk[1]), _n_for_poisson(ob.bf_mesh_ijk[2])

def get_cell_size(ob):
    """Calc cell size"""
    return (
        ob.dimensions[0] / ob.bf_mesh_ijk[0] or .001,
        ob.dimensions[1] / ob.bf_mesh_ijk[1] or .001,
        ob.dimensions[2] / ob.bf_mesh_ijk[2] or .001
       )

def get_cell_number(ob):
    """Calc cell number"""
    return ob.bf_mesh_ijk[0] * ob.bf_mesh_ijk[1] * ob.bf_mesh_ijk[2]
