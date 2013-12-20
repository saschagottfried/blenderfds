"""BlenderFDS, geometric utilities"""

import bpy, bmesh

### Constants

epsilon = .0001
DEBUG = True

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

