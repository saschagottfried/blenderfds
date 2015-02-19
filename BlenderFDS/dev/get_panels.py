import bpy
bpy_types = dir(bpy.types)
for bpy_type in bpy_types:
    bpy_type_class = getattr(bpy.types, bpy_type)
    if not issubclass(bpy_type_class, bpy.types.Panel): continue
    if not getattr(bpy_type_class, "bl_context", None) == "scene": continue
    print(bpy_type)
