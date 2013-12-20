"""BlenderFDS, voxelize algorithms."""

import bpy
from time import time
from blenderfds.geometry.utilities import *

DEBUG = True

def voxelize(context, ob, flat=False, normal=None, location=None):
    """FIXME"""
    print("BFDS: voxelize.voxelize:", ob.name)
    # Init
    t0 = time()
    voxel_size = ob.bf_xb_voxel_size
    ob_tmp = _get_absolute_tmp_object(context, ob)
    # FIXME
    if flat: pass
    # Apply remesh modifier
    _apply_remesh_modifier(context, ob_tmp, voxel_size)
    # Get absolute tessfaces
    me_tmp = get_global_mesh(context, ob_tmp)
    tessfaces = get_tessfaces(context, me_tmp)
    if not tessfaces: return None
    # Sort tessfaces centers by face normal
    t1 = time()
    x_tessfaces, y_tessfaces, z_tessfaces = _sort_tessfaces_by_normal(tessfaces)
    # Choose fastest procedure: less tessfaces => less time required
    t2 = time()
    choose = [ # Here you choose the procedure by hand FIXME
        (2, x_tessfaces, _x_tessfaces_to_boxes, _grow_boxes_along_x, _x_boxes_to_xbs),
        (1, y_tessfaces, _y_tessfaces_to_boxes, _grow_boxes_along_y, _y_boxes_to_xbs),
        (0, z_tessfaces, _z_tessfaces_to_boxes, _grow_boxes_along_z, _z_boxes_to_xbs),
    ]
#        (len(x_tessfaces), x_tessfaces, _x_tessfaces_to_boxes, _grow_boxes_along_x, _x_boxes_to_xbs),
#        (len(y_tessfaces), y_tessfaces, _y_tessfaces_to_boxes, _grow_boxes_along_y, _y_boxes_to_xbs),
#        (len(z_tessfaces), z_tessfaces, _z_tessfaces_to_boxes, _grow_boxes_along_z, _z_boxes_to_xbs),
#    ]
    choose.sort(key=lambda k:k[0]) # sort by len(tessfaces)
    # Build boxes along 1st axis
    t3 = time()
    boxes = choose[0][2](choose[0][1], voxel_size)
    # Grow boxes along 2nd axis
    t4 = time()
    boxes = choose[1][3](boxes)
    # Grow boxes along 3rd axis
    t5 = time()
    boxes = choose[2][3](boxes)
    t6 = time()
    # Prepare XBs
    xbs = choose[0][4](boxes, voxel_size) #FIXME double check this time!
    if flat: pass
    #xbs = _boxes_to_xbs(boxes, voxel_size)
    # Clean unneeded tmp object and tmp mesh
    if DEBUG: bpy.context.scene.objects.link(ob_tmp) # leave tmp object
    else: # del tmp object
        bpy.data.objects.remove(ob_tmp)
        bpy.data.meshes.remove(me_tmp)
    # Return
    return xbs

def _get_absolute_tmp_object(context, ob):
    """Get absolute tmp object of ob: modifiers, rotation, location and scale are applied."""
    ob_tmp = bpy.data.objects.new("{}_tmp".format(ob.name), get_global_mesh(context, ob))
    ob_tmp.bf_is_tmp = True
    return ob_tmp

def _apply_remesh_modifier(context, ob, voxel_size):
    """Apply remesh modifier for voxelization."""
    mo = ob.modifiers.new('voxels_tmp','REMESH') # apply modifier
    mo.octree_depth, mo.scale, voxel_size, dimension_too_large = _calc_remesh_parameters(context, ob.dimensions, voxel_size)
    if dimension_too_large: raise BFException(sender=ob, msg="Too large for desired voxel size, split it in parts.".format(ob.name))
    mo.mode, mo.use_remove_disconnected = 'BLOCKS', False

# When appling a remesh modifier, object max dimension is scaled by scale value
# and divided in 2 ** octree_depth voxels

def _calc_remesh_parameters(context, dimensions, voxel_size):
    """Calc Remesh modifier parameters for voxel_size."""
    # Fix voxel_size for Blender remesh algorithms
    # If dimension / voxel_size is "too integer" voxelization is not very good.
    broken = [True, True, True]
    while any(broken):
        for index, dimension in enumerate(dimensions):
            # dimension should not be 0 as in flat objects!
            if dimension and (dimension / voxel_size) - round(dimension / voxel_size) < 1E-6: voxel_size -= 1E-6
            else: broken[index] = False
    # Get max dimension and init flag
    dimension = max(dimensions)
    dimension_too_large = True
    # Find righ octree_depth and relative scale
    for octree_depth in range(1,11):
        scale = dimension / voxel_size / 2 ** octree_depth
        if 0.010 < scale < 0.990:
            dimension_too_large = False
            break
    if dimension_too_large: scale = 0.990
    # Recalc true voxel_size and return
    voxel_size = dimension / scale / 2 ** octree_depth
    return octree_depth, scale, voxel_size, dimension_too_large

def _apply_solidify_modifier(context, ob, thickness):
    """Apply solidify modifier with centered thickness."""
    mo = ob.modifiers.new('solid_tmp','SOLIDIFY') # apply modifier
    mo.thickness, mo.offset = thickness, thickness / 2. # Set centered thickness

# Sort tessfaces by normal

def _sort_tessfaces_by_normal(tessfaces):
    """Sort tessfaces: normal to x axis, y axis, z axis."""
    if DEBUG: print("BFDS: _sort_tessfaces_by_normal:", len(tessfaces))
    x_tessfaces, y_tessfaces, z_tessfaces = list(), list(), list()
    for tessface in tessfaces:
        normal = tessface.normal
        # Faces from Remesh modifier are perpendicular to axis, so not 0 is certainly 1. FIXME
        if   abs(normal[0]) > .9: x_tessfaces.append(tessface) # tessface is normal to x axis
        elif abs(normal[1]) > .9: y_tessfaces.append(tessface) # tessface is normal to y axis
        elif abs(normal[2]) > .9: z_tessfaces.append(tessface) # tessface is normal to z axis
        else: raise ValueError("BFDS: voxelize._sort_tessfaces_by_normal: abnormal face")
    return x_tessfaces, y_tessfaces, z_tessfaces

# (ix, iy, iz) : absolute int coordinates of center of a tessface normal to an axis
#   ix = round(center[0] / voxel_size)
#   iy = round(center[1] / voxel_size)
#   iz = round(center[2] / voxel_size)
# the absolute origin point (0, 0, 0) is used as reference,
# voxel_size is used as step.

# First, get x_tessfaces center coordinates in absolute int coordinates,
# and classify them in list of floors for each location:
# (iy, iz -> location int coordinates):
#    (ix0, ix1, ... -> list of floors int coordinates, an even number for closed geometry)
# Then create a list of minimal boxes from floors:
# [(ix0, ix1, iy, iy, iz, iz -> int coordinates), (...), ...]"""

# Use floor levels to detect solid volumes:
# Eg. at location of int coordinates (ix, iy), at z floor izs[0] go into solid, at izs[1] go out of solid, at izs[2] go into solid, ...
# If solid is manifold, len(izs) is an even number: go into solid at izs[0], get at last out of it at izs[-1].
# --> z axis, izs: |==solid==| void |==solid==|

def _x_tessfaces_to_boxes(x_tessfaces, voxel_size):
    """Transform _x_tessfaces into minimal boxes."""
    if DEBUG: print("BFDS: _x_tessfaces_to_boxes:", len(x_tessfaces))
    # Create floors
    floors = dict() # {(3,4):(3,4,15,25,), (3,5):(3,4,15,25), ...}
    for tessface in x_tessfaces:
        center = tuple(tessface.center)        
        ix = round(center[0] / voxel_size) # face index, round returns an int
        iy = round(center[1] / voxel_size)
        iz = round(center[2] / voxel_size)
        try: floors[(iy, iz)].append(ix) # append face ix to list of ixs
        except: floors[(iy, iz)] = [ix,] # or create new list of ixs from face ix
    # Create minimal boxes
    boxes = list()
    while floors:
        (iy, iz), ixs = floors.popitem()
        ixs.sort() # sort from bottom to top in +x direction
        while ixs:
            ix1 = ixs.pop() # pop from top to bottom in -x direction
            ix0 = ixs.pop()
            boxes.append((ix0, ix1, iy, iy, iz, iz,))
    return boxes


#    """Classify centers of tessfaces normal to y axis in absolute int coordinates:
#    (ix, iz -> location int coordinates):
#    (iy0, iy1, ... -> list of floors int coordinates, an even number for closed geometry)"""
#    """Create minimal boxes from y_floors:
#    [(ix, ix, iy0, iy1, iz, iz -> int coordinates), (...), ...]"""

def _y_tessfaces_to_boxes(y_tessfaces, voxel_size):
    """Transform _y_tessfaces into minimal boxes."""
    if DEBUG: print("BFDS: _y_tessfaces_to_boxes:", len(y_tessfaces))
    # Create floors
    floors = dict() # {(3,4):(3,4,15,25,), (3,5):(3,4,15,25), ...}
    for tessface in y_tessfaces:
        center = tuple(tessface.center)
        ix = round(center[0] / voxel_size) # face index, round returns an int
        iy = round(center[1] / voxel_size)
        iz = round(center[2] / voxel_size)
        try: floors[(ix, iz)].append(iy) # append face iy to existing list of iys
        except: floors[(ix, iz)] = [iy,] # or create new list of iys from face iy
    # Create minimal boxes
    boxes = list()
    while floors:
        (ix, iz), iys = floors.popitem()
        iys.sort() # sort from bottom to top in +y direction
        while iys:
            iy1 = iys.pop() # pop from top to bottom in -y direction
            iy0 = iys.pop()
            boxes.append((ix, ix, iy0, iy1, iz, iz,))
    return boxes

#    """Classify centers of tessfaces normal to z axis in absolute int coordinates:
#    (ix, iy -> location int coordinates):
#    (iz0, iz1, ... -> list of floors int coordinates, an even number for closed geometry)"""
#    """Create minimal boxes from z_floors:
#    [(ix, ix, iy, iy, iz0, iz1 -> int coordinates), (...), ...]"""

def _z_tessfaces_to_boxes(z_tessfaces, voxel_size):
    """Transform _z_tessfaces into minimal boxes."""
    if DEBUG: print("BFDS: _z_tessfaces_to_boxes:", len(z_tessfaces))
    # Create floors
    floors = dict() # {(3,4):(3,4,15,25,), (3,5):(3,4,15,25), ...}
    for tessface in z_tessfaces:
        center = tuple(tessface.center)
        ix = round(center[0] / voxel_size) # face index, round returns an int
        iy = round(center[1] / voxel_size)
        iz = round(center[2] / voxel_size)
        try: floors[(ix, iy)].append(iz) # append face iz to existing list of izs
        except: floors[(ix, iy)] = [iz,] # or create new list of izs from face iz
    # Create minimal boxes  
    boxes = list()
    while floors:
        (ix, iy), izs = floors.popitem()
        izs.sort() # sort from bottom to top in +z direction
        while izs:
            iz1 = izs.pop() # pop from top to bottom in -z direction
            iz0 = izs.pop()
            boxes.append((ix, ix, iy, iy, iz0, iz1,))
    return boxes

# Try to merge each solid box with other available boxes in axis direction

def _grow_boxes_along_x(boxes):
    """Grow boxes along x axis:
    [(ix0, ix1, iy0, iy1, iz0, iz1 -> int coordinates), (...), ...]"""
    if DEBUG: print("BFDS: _grow_boxes_along_x:", len(boxes))
    boxes_grown = list()
    while boxes:
        ix0, ix1, iy0, iy1, iz0, iz1 = boxes.pop()
        while True: # fatten into +x direction
            box_desired = (ix1 + 1, ix1 + 1, iy0, iy1, iz0, iz1,)
            try: boxes.remove(box_desired)
            except: break
            ix1 += 1
        while True: # fatten into -x direction
            box_desired = (ix0 - 1, ix0 - 1, iy0, iy1, iz0, iz1,)
            try: boxes.remove(box_desired)
            except: break
            ix0 -= 1
        boxes_grown.append((ix0, ix1, iy0, iy1, iz0, iz1))
    return boxes_grown

def _grow_boxes_along_y(boxes):
    """Grow boxes along y axis:
    [(ix0, ix1, iy0, iy1, iz0, iz1 -> int coordinates), (...), ...]"""
    if DEBUG: print("BFDS: _grow_boxes_along_y:", len(boxes))
    boxes_grown = list()
    while boxes:
        ix0, ix1, iy0, iy1, iz0, iz1 = boxes.pop()
        while True: # fatten into +y direction
            box_desired = (ix0, ix1, iy1 + 1, iy1 + 1, iz0, iz1)
            try: boxes.remove(box_desired)
            except: break
            iy1 += 1
        while True: # fatten into -y direction
            box_desired = (ix0, ix1, iy0 - 1, iy0 - 1, iz0, iz1)
            try: boxes.remove(box_desired)
            except: break
            iy0 -= 1
        boxes_grown.append((ix0, ix1, iy0, iy1, iz0, iz1))
    return boxes_grown

def _grow_boxes_along_z(boxes):
    """Grow boxes along z axis:
    [(ix0, ix1, iy0, iy1, iz0, iz1 -> int coordinates), (...), ...]"""
    if DEBUG: print("BFDS: _grow_boxes_along_z:", len(boxes))
    boxes_grown = list()
    while boxes:
        ix0, ix1, iy0, iy1, iz0, iz1 = boxes.pop()
        while True: # fatten into +z direction
            box_desired = (ix0, ix1, iy0, iy1, iz1 + 1, iz1 + 1)
            try: boxes.remove(box_desired)
            except: break
            iz1 += 1
        while True: # fatten into -z direction
            box_desired = (ix0, ix1, iy0, iy1, iz0 - 1, iz0 - 1)
            try: boxes.remove(box_desired)
            except: break
            iz0 -= 1
        boxes_grown.append((ix0, ix1, iy0, iy1, iz0, iz1))
    return boxes_grown

# Convert back int coordinates to real world coordinates
# FIXME comments


def _x_boxes_to_xbs(boxes, voxel_size):
    if DEBUG: print("BFDS: _x_boxes_to_xbs:", len(boxes))
    xbs = list()
    while boxes:
        ix0, ix1, iy0, iy1, iz0, iz1 = boxes.pop()
        x0, y0, z0 = ix0 * voxel_size, (iy0-.5) * voxel_size, (iz0-.5) * voxel_size
        x1, y1, z1 = ix1 * voxel_size, (iy1+.5) * voxel_size, (iz1+.5) * voxel_size  
        xbs.append([x0, x1, y0, y1, z0, z1],)
    return xbs

def _y_boxes_to_xbs(boxes, voxel_size):
    if DEBUG: print("BFDS: _y_boxes_to_xbs:", len(boxes))
    xbs = list()
    while boxes:
        ix0, ix1, iy0, iy1, iz0, iz1 = boxes.pop()
        x0, y0, z0 = (ix0-.5) * voxel_size, iy0 * voxel_size, (iz0-.5) * voxel_size
        x1, y1, z1 = (ix1+.5) * voxel_size, iy1 * voxel_size, (iz1+.5) * voxel_size  
        xbs.append([x0, x1, y0, y1, z0, z1],)
    return xbs

def _z_boxes_to_xbs(boxes, voxel_size):
    if DEBUG: print("BFDS: _z_boxes_to_xbs:", len(boxes))
    xbs = list()
    while boxes:
        ix0, ix1, iy0, iy1, iz0, iz1 = boxes.pop()
        x0, y0, z0 = (ix0-.5) * voxel_size, (iy0-.5) * voxel_size, iz0 * voxel_size
        x1, y1, z1 = (ix1+.5) * voxel_size, (iy1+.5) * voxel_size, iz1 * voxel_size  
        xbs.append([x0, x1, y0, y1, z0, z1],)
    return xbs



def _boxes_to_xbs(boxes, voxel_size, flat=False, normal="z", location=(0.,0.,0.)):
    """Transform boxes in int absolute coordinates to xbs in true absolute coordinates:
    [(x0, x1, y0, y1, z0, z1 -> true coordinates), (...), ...]"""
    if DEBUG: print("BFDS: _boxes_to_xbs:", len(boxes))
    xbs = list()
    while boxes:
        ix0, ix1, iy0, iy1, iz0, iz1 = boxes.pop()
        # Absolute int coordinates refer to the absolute origin point (0, 0, 0).
        # voxel_size is the step of int coordinates.
        # -.5 and +.5 are used to reach box corner from tessface center.
        x0, y0, z0 = (ix0 - .5) * voxel_size, (iy0 - .5) * voxel_size, (iz0 - .5) * voxel_size
        x1, y1, z1 = (ix1 + .5) * voxel_size, (iy1 + .5) * voxel_size, (iz1 + .5) * voxel_size  
        xbs.append([x0, x1, y0, y1, z0, z1],)
        if flat:
            # Flatten object to location, a flat object is required here!
            if normal == "z":
                for xb in xbs: xb[4] = xb[5] = location[2]
            elif normal == "y":
                for xb in xbs: xb[2] = xb[3] = location[1]
            elif normal == "x":
                for xb in xbs: xb[0] = xb[1] = location[0]
            else: raise ValueError("BFDS: Unrecognized normal, problem in _boxes_to_xbs.")
    return xbs

