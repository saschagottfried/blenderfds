"""BlenderFDS, Blender geometry routines"""

import bpy, bmesh
from mathutils import Vector
from time import time
from blenderfds.types import *
from blenderfds.types.flags import *

DEBUG = True

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
    if active_ob: active_ob.select = True

### Working on Blender materials

def get_new_material(context, name="Material"):
    """Create new material, named name."""
    return bpy.data.materials.new(name)

def get_material(context, name="Material"):
    """Get or create material and return it"""
    if name not in bpy.data.materials:
        return get_new_material(context, name)
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

# FIXME common code in pixels and voxels!!!

def ob_to_xbs_pixels(context, ob):
    """Return a list of object flat voxels XBs and a msg:
    ((x0,x0,y0,y1,z0,z1,), ...), "Message"."""
    print("BFDS: geometry.ob_to_xbs_flat_voxels:", ob.name) 
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
    # Init
    t0 = time()
    voxel_size = ob.bf_xb_voxel_size
    ob_tmp = _get_absolute_tmp_object(context, ob)
    # Apply remesh
    _apply_remesh_modifier(context, ob_tmp, voxel_size)
    # Get absolute tessfaces
    me_tmp = get_global_mesh(context, ob_tmp)
    tessfaces = get_tessfaces(context, me_tmp)
    if not tessfaces: return None, "No voxel created"
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
    xbs = _boxes_to_xbs(boxes, voxel_size)
    # Clean unneeded tmp object and tmp mesh
    bpy.data.objects.remove(ob_tmp) # DEBUG comment out to leave tmp object
    bpy.data.meshes.remove(me_tmp) # DEBUG comment out to leave tmp object
    #bpy.context.scene.objects.link(ob_tmp) # DEBUG uncomment to leave temp object
    # Return
    msg = "{0} voxels, resolution {1:.3f} m, in {2:.0f} s".format(len(xbs), voxel_size, time()-t0) or None
    if DEBUG and msg: msg += " (s:{0:.0f} c:{1:.0f} fb:{2:.0f}, g1:{3:.0f}, g2:{4:.0f})".format(t2-t1, t3-t2, t4-t3, t5-t4, t6-t5)
    return xbs, msg

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
    if DEBUG: print("BFDS: _calc_remesh_parameters: octree_depth, scale, voxel_size, dimension_too_large:\n", octree_depth, scale, voxel_size, dimension_too_large)
    return octree_depth, scale, voxel_size, dimension_too_large

def _apply_solidify_modifier(context, ob, thickness):
    """Apply solidify modifier with centered thickness."""
    mo = ob.modifiers.new('solid_tmp','SOLIDIFY') # apply modifier
    mo.thickness, mo.offset = thickness, thickness / 2. # Set centered thickness

# Sort tessfaces by normal

def _sort_tessfaces_by_normal(tessfaces):
    """Sort tessfaces: normal to x axis, y axis, z axis."""
    x_tessfaces, y_tessfaces, z_tessfaces = list(), list(), list()
    for tessface in tessfaces:
        normal = tessface.normal
        if   abs(normal[2]) > .9: z_tessfaces.append(tessface) # tessface is normal to z axis
        elif abs(normal[1]) > .9: y_tessfaces.append(tessface) # tessface is normal to y axis
        else: x_tessfaces.append(tessface)                     # tessface is normal to x axis
    return x_tessfaces, y_tessfaces, z_tessfaces

# (ix, iy, iz) : absolute int coordinates of center of a tessface normal to an axis
#   ix = round(center[0] / voxel_size)
#   iy = round(center[1] / voxel_size)
#   iz = round(center[2] / voxel_size)
# the absolute origin point (0, 0, 0) is used as reference,
# voxel_size is used as step.

def _get_x_floors(x_tessfaces, voxel_size):
    """Classify centers of tessfaces normal to x axis in absolute int coordinates:
    (iy, iz -> location int coordinates):
    (ix0, ix1, ... -> list of floors int coordinates, an even number for closed geometry)"""
    if DEBUG: print("BFDS: _get_x_floors:", len(x_tessfaces))
    x_floors = dict() # {(3,4):(3,4,15,25,), (3,5):(3,4,15,25), ...}
    for tessface in x_tessfaces:
        center = tuple(tessface.center)        
        ix = round(center[0] / voxel_size) # face index, round returns an int
        iy = round(center[1] / voxel_size)
        iz = round(center[2] / voxel_size)
        try: x_floors[(iy, iz)].append(ix) # append face ix to list of ixs
        except: x_floors[(iy, iz)] = [ix,] # or create new list of ixs from face ix
    return x_floors

def _get_y_floors(y_tessfaces, voxel_size):
    """Classify centers of tessfaces normal to y axis in absolute int coordinates:
    (ix, iz -> location int coordinates):
    (iy0, iy1, ... -> list of floors int coordinates, an even number for closed geometry)"""
    if DEBUG: print("BFDS: _get_x_floors:", len(y_tessfaces))
    y_floors = dict() # {(3,4):(3,4,15,25,), (3,5):(3,4,15,25), ...}
    for tessface in y_tessfaces:
        center = tuple(tessface.center)
        ix = round(center[0] / voxel_size) # face index, round returns an int
        iy = round(center[1] / voxel_size)
        iz = round(center[2] / voxel_size)
        try: y_floors[(ix, iz)].append(iy) # append face iy to existing list of iys
        except: y_floors[(ix, iz)] = [iy,] # or create new list of iys from face iy
    return y_floors

def _get_z_floors(z_tessfaces, voxel_size):
    """Classify centers of tessfaces normal to z axis in absolute int coordinates:
    (ix, iy -> location int coordinates):
    (iz0, iz1, ... -> list of floors int coordinates, an even number for closed geometry)"""
    if DEBUG: print("BFDS: _get_x_floors:", len(z_tessfaces))
    z_floors = dict() # {(3,4):(3,4,15,25,), (3,5):(3,4,15,25), ...}
    for tessface in z_tessfaces:
        center = tuple(tessface.center)
        ix = round(center[0] / voxel_size) # face index, round returns an int
        iy = round(center[1] / voxel_size)
        iz = round(center[2] / voxel_size)
        try: z_floors[(ix, iy)].append(iz) # append face iz to existing list of izs
        except: z_floors[(ix, iy)] = [iz,] # or create new list of izs from face iz
    return z_floors  

# Use floor levels to detect solid volumes:
# Eg. at location of int coordinates (ix, iy), at z floor izs[0] go into solid, at izs[1] go out of solid, at izs[2] go into solid, ...
# If solid is manifold, len(izs) is an even number: go into solid at izs[0], get at last out of it at izs[-1].
# --> z axis, izs: |==solid==| void |==solid==|

def _x_floors_to_boxes(x_floors):
    """Create minimal boxes from x_floors:
    [(ix0, ix1, iy, iy, iz, iz -> int coordinates), (...), ...]"""
    if DEBUG: print("BFDS: _x_floors_to_boxes:", len(x_floors))
    boxes = list()
    while x_floors:
        (iy, iz), ixs = x_floors.popitem()
        ixs.sort() # sort from bottom to top in +x direction
        while ixs:
            ix1, ix0 = ixs.pop(), ixs.pop() # pop from top to bottom in -x direction
            boxes.append((ix0, ix1, iy, iy, iz, iz,))
    return boxes
    
def _y_floors_to_boxes(y_floors):
    """Create minimal boxes from y_floors:
    [(ix, ix, iy0, iy1, iz, iz -> int coordinates), (...), ...]"""
    if DEBUG: print("BFDS: _y_floors_to_boxes:", len(y_floors))
    boxes = list()
    while y_floors:
        (ix, iz), iys = y_floors.popitem()
        iys.sort() # sort from bottom to top in +y direction
        while iys:
            iy1, iy0 = iys.pop(), iys.pop() # pop from top to bottom in -x direction
            boxes.append((ix, ix, iy0, iy1, iz, iz,))
    return boxes

def _z_floors_to_boxes(z_floors):
    """Create minimal boxes from z_floors:
    [(ix, ix, iy, iy, iz0, iz1 -> int coordinates), (...), ...]"""
    if DEBUG: print("BFDS: _z_floors_to_boxes:", len(z_floors))
    boxes = list()
    while z_floors:
        (ix, iy), izs = z_floors.popitem()
        izs.sort() # sort from bottom to top in +z direction
        while izs:
            iz1, iz0 = izs.pop(), izs.pop() # pop from top to bottom in -z direction
            boxes.append((ix, ix, iy, iy, iz0, iz1,))
    return boxes

# Try to merge each solid box with other available boxes on +x and -x direction

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

# Try to merge each solid box with other available boxes on +y and -y direction

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

# Try to merge each solid box with other available boxes on +z and -z direction
    
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
        else: raise ValueError("BFDS: Building planes impossible, problem in ob_to_xbs_faces.")
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

### Geometry choice matrix for functions

choose_to_geometry = {
    "xbs": {
        # name       to FDS      from FDS
        "NONE"    : (ob_to_none, none_to_mesh,),
        "BBOX"    : (ob_to_xbs_bbox, xbs_bbox_to_mesh,),
        "VOXELS"  : (ob_to_xbs_voxels, xbs_bbox_to_mesh,),
        "FACES"   : (ob_to_xbs_faces, xbs_faces_to_mesh,),
        "PIXELS"  : (ob_to_xbs_pixels, xbs_bbox_to_mesh,),
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
    msg = None    
    msgs = list()
    err_msgs = list()
    # Manage XB: get coordinates, show them in a tmp object, prepare msg
    xbs = None
    try:  xbs, msg  = ob_to_xbs(context, ob)
    except BFException as err: err_msgs.extend(err.labels)
    if msg: msgs.append(msg)
    if xbs:
        ob_tmp = xbs_to_ob(xbs, context, bf_xb=ob.bf_xb)
        set_tmp_object(context, ob, ob_tmp)
    # Manage XYZ: get coordinates, show them in a tmp object, prepare msg
    xyzs = None
    try: xyzs, msg = ob_to_xyzs(context, ob)
    except BFException as err: err_msgs.extend(err.labels)
    if msg: msgs.append(msg)
    if xyzs:
        ob_tmp = xyzs_to_ob(xyzs, context, bf_xyz=ob.bf_xyz)
        set_tmp_object(context, ob, ob_tmp)
    # Manage PB*: get coordinates, show them in a tmp object, prepare msg
    pbs  = None        
    try: pbs, msg  = ob_to_pbs(context, ob)
    except BFException as err: err_msgs.extend(err.labels)
    if msg: msgs.append(msg)
    if pbs:
        ob_tmp = pbs_to_ob(pbs, context, bf_pb=ob.bf_pb)
        set_tmp_object(context, ob, ob_tmp)
    # Return report
    if err_msgs: return {"ERROR"}, "; ".join(err_msgs)
    if msgs: return {"INFO"}, "; ".join(msgs)
    if xbs or xyzs or pbs: return {"INFO"}, "FDS geometries shown"
    return {"WARNING"}, "No geometry to show"
