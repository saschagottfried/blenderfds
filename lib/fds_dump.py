"""BlenderFDS, dump GE1 render file"""

import bpy
#from blenderfds.types import *
#from blenderfds.types.flags import *
from blenderfds.lib import geometry

# File format:
# [APPEARANCE]
# nappearances
# string (material description)
# index r g b twidth, theight, alpha, shininess, tx0, ty0, tz0
# tfile
# :
# :
# The above entry is repeated nappearances-1 more times
# :
# [FACES]
# nfaces
# x1 y1 z1 x2 y2 z2 x3 y3 z3 x4 y4 z4 index
# The above line is repeated nfaces-1 more times.

# Explanation:
# nappearances: number of appearance entries to follow Each appearance entry has 3 lines.
# string: a material description is written out by DX2FDS but is ignored by Smokeview.
# index: an index number starting at 0.
# r,g,b: red green and blue components of the CAD face from 0 to 255. If a color is not used then use -1 for each color component.
# twidth, theight: extures are tiled or repeated. The characteristic width and length of the texture file is twidth and theight respectively.
# alpha: Opaqueness value of the cad element being drawn. Values may range from 0.0 (completely transparent) to 1.0 (completely opaque. (default: 1.0)
# shininess: Shininess value of the cad element being drawn. Values may be larger than 0.0 . (default: 800.0)
# tx0, ty0, tz0: x, y and z values in physical coordinates of the offset used to apply a texture to a cad element. (default: 0.0, 0.0, 0.0)
# tfile: The name of the texture file. If one is not used or available then leave this line blank.
# nfaces: Number of face entries to follow. Each face entry has one line.
# x1/y1/z1/.../x4/y4/z4: x,y,z coordinates of a quadrilateral. T he four corners of the quad must lie in a
# plane or weird effects may result when Smokeview draws it. (This is a requirement of OpenGL).
# The four points should be in counter-clockwise order.
# index: Points to a material in the [APPEARANCE] section.

# [APPEARANCE]
# 4
# Appearance[SURF03,r=255,g=255,b=255,a=128]
# 0 1.000000 1.000000 1.000000 0.000000 0.000000 0.500000
#
# Appearance[SURF03,r=240,g=240,b=240,a=128]
# 1 0.941176 0.941176 0.941176 0.000000 0.000000 0.500000
#
# Appearance[INERT,r=0,g=0,b=0,a=255]
# 2 0.000000 0.000000 0.000000 0.000000 0.000000
#
# Appearance[SMOKE,r=102,g=0,b=102,a=255]
# 3 0.400000 0.000000 0.400000 0.000000 0.000000
#
# [FACES]
# 38
# 6.097844 3.949375 0.584987 6.097844 1.927848 0.584987 6.097844 1.927848 1.963850 6.097844 3.949375 1.963850 0
# 8.151876 1.927848 0.584987 8.151876 3.949375 0.584987 8.151876 3.949375 1.963850 8.151876 1.927848 1.963850 0
# 6.097844 1.927848 0.584987 8.151876 1.927848 0.584987 8.151876 1.927848 1.963850 6.097844 1.927848 1.963850 0
# 8.151876 3.949375 0.584987 6.097844 3.949375 0.584987 6.097844 3.949375 1.963850 8.151876 3.949375 1.963850 0
# ...

def to_ge1(context):
    # Get materials
    mas = (ma for ma in bpy.data.materials)
    choose_ma_to_appearance = dict((ma.name, index) for index, ma in enumerate(mas))
    # Get faces
    obs = (ob for ob in context.scene.objects if ob.type == "MESH" and ob.bf_namelist_export and ob.bf_namelist in ("OBST", "VENT")) # FIXME HOLE?
    faces = list()
    for ob in obs:
        me = geometry.get_global_mesh(context, ob)
        tessfaces = geometry.get_tessfaces(context, me)
        for tessface in tessfaces:
            # Get vertices
            vertices = (me.vertices[vertex] for vertex in tessface.vertices)
            # Append face vertices: x0, y0, z0, x1, y1, z1, x2, y2, z2, ... Can be tri or quad.
            faces.append(co for vertex in vertices for co in vertex.co)
            # If tri face, extend with x3, y3, z3 same as x2, y2, z2
            if len(vertices) == 3: faces[-1].extend(vertices[2])
        appearance = choose_ma_to_appearance[ob.active_material.name]
        for face in faces:
            face.append(appearance)
    # Set GE1 file
    ge1_file = str()
    ge1_file += "[APPEARANCE]\n{}\n".format(len(mas))
    for ma in mas:
        ge1_file += "{}\n{} {} {} {} {} {} {}\n\n".format(
            ma.name,
            choose_ma_to_appearance[ma.name],
            ma.diffuse_color[0], ma.diffuse_color[1], ma.diffuse_color[2],
            0.0, 0.0,
            ma.alpha
        )
    ge1_file += "[FACES]\n{}\n".format(len(faces))
    for face in faces:
        face = (str(n) for n in face)
        ge1_file += " ".join(face)
    # return
    return ge1_file
    
