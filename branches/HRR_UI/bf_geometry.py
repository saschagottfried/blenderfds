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

import bpy
from mathutils import Vector, geometry
from time import time

# FIXME psyco

### Working on Blender objects and meshes
# input: ob or me
# output: me or bool

def get_global_mesh(ob):
    """Return object mesh modified and transformed in global coordinates."""
    # Get ob mesh in global coordinates and return it
    me = ob.create_mesh(bpy.context.scene, True, "PREVIEW") # apply modifiers
    me.transform(ob.matrix_world) # transform mesh in global coordinates
    return me

def is_manifold(me):
    """Check if mesh me is manifold"""
    edgeusers = dict()
    faces = me.faces
    if len(faces) == 0: return False
    for face in faces:
        for edgekey in face.edge_keys:
            edgeusers.setdefault(edgekey, 0)
            edgeusers[edgekey] += 1
    for val in edgeusers.values():
        if val != 2: return False
    return True

### Auxiliary functions to get_boxels
#
#

class _Fa3:
    """Used to triangulate the mesh and simplify the notation"""
    def __init__(self, vertices, normal):
        self.vertices = vertices
        self.normal = normal

def _is_inside_z_projection(point, fa3s):
    """Check if point is inside z projections of faces fa3s"""
    # Calculate boundary edges of the z projection surface of fa3s
    boundary_edges = set()
    for fa3 in fa3s:
        for edge in ((fa3.vertices[0], fa3.vertices[1]),
                     (fa3.vertices[1], fa3.vertices[2]),
                     (fa3.vertices[2], fa3.vertices[0])):
            if (edge[0], edge[1]) in boundary_edges:
                boundary_edges.remove((edge[0], edge[1]))
                continue
            if (edge[1], edge[0]) in boundary_edges:
                boundary_edges.remove((edge[1], edge[0]))
                continue
            boundary_edges.add(edge)
    # Check if the point is inside the surface (2D-polygon test)
    inside = False
    for edge in boundary_edges:
        p0, p1 = edge[0].co, edge[1].co
        if (p0[1] <= point[1] < p1[1]):
            if geometry.normal(point, p0, p1)[2] < 0.0: inside = not inside
            continue
        if (p1[1] <= point[1] < p0[1]):
            if geometry.normal(point, p0, p1)[2] > 0.0: inside = not inside
            continue
    return inside

def _triangulate(me):
    """me.faces triangulation, return list of fa3 faces"""
    # fa3s is a list containing Fa3 face objects
    # me.faces -> fa3s, face -> fa3
    fa3s = list()
    for face in me.faces:
        fa3s.append(_Fa3(vertices=[me.vertices[face.vertices[0]],
                                   me.vertices[face.vertices[1]],
                                   me.vertices[face.vertices[2]]],
                         normal=face.normal))
        if len(face.vertices) != 3: # If it's a quad, add another face
            fa3s.append(_Fa3(vertices=[me.vertices[face.vertices[0]],
                                       me.vertices[face.vertices[2]],
                                       me.vertices[face.vertices[3]]],
                             normal=face.normal))
    return fa3s

def _voxelize(ob, voxel_size, solid):
    """Voxelize ob mesh, return a set of voxels
This code is copy-pasted from Cells v1.2_248 (GPL) for Blender
by Michael Schardt (m.schardt@web.de)
"""
    # Timing
    t0 = time()
    # Get ob mesh in global coordinates and triangulate it
    me = get_global_mesh(ob)
    fa3s = _triangulate(me)
    # Get bin vertices: voxels_vertices
    # it uses vertices from Blender mesh me
    voxels_vertices = dict() # {vertex obj: (3,7,3), vertex obj: (1,2,3), ...}
    for vertex in me.vertices:
        coords = vertex.co
        voxels_vertices[vertex] = (int(round(coords[0]/voxel_size[0])),
                                   int(round(coords[1]/voxel_size[1])),
                                   int(round(coords[2]/voxel_size[2])))
    # Get bin faces: voxels
    voxels = dict()
    for fa3 in fa3s:
        vertices = fa3.vertices
        # Calc min and max of fa3 integer delta x
        fidxs = [voxels_vertices[vertex][0] for vertex in vertices]
        fidxs.sort()
        min_fidx, max_fidx = fidxs[0], fidxs[-1]
        # Calc min and max of fa3 integer delta y
        fidys = [voxels_vertices[vertex][1] for vertex in vertices]
        fidys.sort()
        min_fidy, max_fidy = fidys[0], fidys[-1]
        # Calc min and max of fa3 integer delta z
        fidzs = [voxels_vertices[vertex][2] for vertex in vertices]
        fidzs.sort()
        min_fidz, max_fidz = fidzs[0], fidzs[-1]
        # Fast path: used especially for small fa3s spanning a single voxel only
        # The following uses a very smart approach for choice of category!
        category = 0
        if (max_fidx > min_fidx): category |= 1
        if (max_fidy > min_fidy): category |= 2
        if (max_fidz > min_fidz): category |= 4
        if category == 0: # single voxel
            voxels.setdefault((min_fidx, min_fidy, min_fidz), set()).add(fa3)
                # If key (min_fidx, ...) not in voxels dict,
                # voxels[key] = set().add(fa3) else voxels[key] = set(existing fa3s).add(fa3)
            continue
        if category == 1: # multiple voxels in x-, single voxel in y- and z-direction
            for fidx in range(min_fidx, max_fidx + 1):
                voxels.setdefault((fidx, min_fidy, min_fidz), set()).add(fa3)
            continue
        if category == 2: # multiple voxels in y-, single voxel in x- and z-direction
            for fidy in range(min_fidy, max_fidy + 1):
                voxels.setdefault((min_fidx, fidy, min_fidz), set()).add(fa3)
            continue
        if category == 4: # multiple voxels in z-, single voxel in x- and y-direction
            for fidz in range(min_fidz, max_fidz + 1):
                voxels.setdefault((min_fidx, min_fidy, fidz), set()).add(fa3)
            continue
        # Long path: fa3 spans multiple voxels in more than one direction
        a0 = fa3.normal
        r0 = 0.5 * (abs(a0[0])*voxel_size[0] + abs(a0[1])*voxel_size[1] + abs(a0[2])*voxel_size[2])
        es = [Vector((1.0, 0.0, 0.0)), Vector((0.0, 1.0, 0.0)), Vector((0.0, 0.0, 1.0))]
        cc = Vector((0.0, 0.0, 0.0))
        for fidx in range(min_fidx, max_fidx + 1):
            cc[0] = fidx * voxel_size[0]
            for fidy in range(min_fidy, max_fidy + 1):
                cc[1] = fidy * voxel_size[1]
                for fidz in range(min_fidz, max_fidz + 1):
                    cc[2] = fidz * voxel_size[2]
                    if not solid and (fidx, fidy, fidz) in voxels: continue
                       # voxel already populated -> no further processing needed for hollow model
                    vertices_wrk = [vertex.co - cc for vertex in vertices]
                    if not (-r0 <= a0 * vertices_wrk[0] <= r0): continue
                       # voxel not intersecting fa3 hyperplane
                    # Check overlap of voxel with fa3 (separating axis theorem)                     
                    fa3s_wrk = [vertices_wrk[1] - vertices_wrk[0],
                                vertices_wrk[2] - vertices_wrk[1],
                                vertices_wrk[0] - vertices_wrk[2]]
                    overlap = True
                    for fa3_wrk in fa3s_wrk:
                        if not overlap: break
                        for e in es:
                            if not overlap: break
                            a = e.cross(fa3_wrk)
                            r = 0.5 * (abs(a[0])*voxel_size[0] + abs(a[1])*voxel_size[1] + abs(a[2])*voxel_size[2] )
                            ds = [a * vertex_wrk for vertex_wrk in vertices_wrk]
                            ds.sort()
                            if (ds[0] > r or ds[-1] < -r): overlap = False              
                    if overlap: voxels.setdefault((fidx, fidy, fidz), set()).add(fa3)
    # The hollow voxel representation is complete, now fill
    if solid:
        # find min, max voxels in x
        idxs = [voxel[0] for voxel in voxels]
        idxs.sort() # voxel[0] is its fidx from the key
        min_idx, max_idx = idxs[0], idxs[-1]
        # find min, max voxels in y
        idys = [voxel[1] for voxel in voxels]
        idys.sort() # voxel[0] is its fidy from the key
        min_idy, max_idy = idys[0], idys[-1]
        # find min, max voxels in z
        idzs = [voxel[2] for voxel in voxels]
        idzs.sort() # voxel[0] is its fidz from the key
        min_idz, max_idz = idzs[0], idzs[-1]
        # init point_test and loop
        point_test = Vector((0.0, 0.0, 0.0))
        for idx in range(min_idx, max_idx + 1):
            point_test[0] = idx * voxel_size[0]
            for idy in range(min_idy, max_idy + 1):
                point_test[1] = idy * voxel_size[1]
                odd_parity = False
                fa3s_tested = set()
                # walk the z pile and keep track of parity
                for idz in range(min_idz, max_idz + 1):
                    fa3s_wrk = voxels.get((idx, idy, idz), set()) - fa3s_tested
                        # .get returns the value for key if key is in the dict, else the default,
                        # in this case an empty set.
                    if fa3s_wrk: # voxel contains fa3s
                        # categorize fa3s in this voxel by normal
                        fa3s_pos, fa3s_neg = list(), list()
                        for fa3_wrk in fa3s_wrk:
                            fa3_normal_z = fa3_wrk.normal[2] # get z of normal
                            if fa3_normal_z >= 0.0: fa3s_pos.append(fa3_wrk)
                            if fa3_normal_z <= 0.0: fa3s_neg.append(fa3_wrk)                        
                            fa3s_tested.add(fa3_wrk)
                        # check if point_test inside z projections       
                        if fa3s_pos:
                            if _is_inside_z_projection(point_test, fa3s_pos):
                                odd_parity = not odd_parity
                        if fa3s_neg:
                            if _is_inside_z_projection(point_test, fa3s_neg):
                                odd_parity = not odd_parity
                    else: # voxel contains no fa3s (empty voxel)
                        if odd_parity: voxels[(idx, idy, idz)] = 1
                            # odd parity -> empty voxel inside ob
    # Clean up and return voxels (keys only!)
    # each voxel is expressed in integer voxel coordinates
    # voxels is exported as a set
    voxels = set([(voxel[0], voxel[1], voxel[2]) for voxel in voxels]) # no more linked to me
    bpy.data.meshes.remove(me)
    print("   voxelize:", time()-t0)
    return voxels

### Extracting coordinates from objects
# input:  ob
# output: [(x0,x1,y0,y1,z0,z1, ), ] or
#         [(x0,y0,z0, ), ]

def get_bbox(ob):
    """Return object bounding box in global coordinates"""
    # Init mesh
    me = get_global_mesh(ob)
    # Calc the bounding box in global coordinates
    bbminx, bbminy, bbminz = me.vertices[0].co
    bbmaxx, bbmaxy, bbmaxz = me.vertices[0].co
    for vertex in me.vertices:
        x, y, z = vertex.co
        bbminx, bbminy, bbminz = min(bbminx, x), min(bbminy, y), min(bbminz, z)
        bbmaxx, bbmaxy, bbmaxz = max(bbmaxx, x), max(bbmaxy, y), max(bbmaxz, z)
    return [(bbminx, bbmaxx, bbminy, bbmaxy, bbminz, bbmaxz,), ]

def get_faces(ob):
    """Return object face straightened bounding boxes in global coordinates"""
    result = []
    me = get_global_mesh(ob)
    for face in me.faces:
        vertices = [me.vertices[vertex] for vertex in face.vertices]
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
    return result

def get_planes(ob):
    """Return object planes with orientation and coordinate"""
    xbs = get_faces(ob)
    result = tuple()
    for xb in xbs:
        if   xb[1] - xb[0] < .000001: result += (("X", xb[0]),)
        elif xb[3] - xb[2] < .000001: result += (("Y", xb[2]),)
        elif xb[5] - xb[4] < .000001: result += (("Z", xb[4]),)
    return result

def get_edges(ob):
    """Return object edges in global coordinates"""
    result = []
    me = get_global_mesh(ob)
    for edge in me.edges:
        pt0x, pt0y, pt0z = me.vertices[edge.vertices[0]].co
        pt1x, pt1y, pt1z = me.vertices[edge.vertices[1]].co
        result.append((pt0x, pt1x, pt0y, pt1y, pt0z, pt1z,),)
    result.sort()
    return result

def get_vertices(ob):
    """Return object vertices in global coordinates"""
    result = []
    me = get_global_mesh(ob)
    for vertex in me.vertices:
        pt0x, pt0y, pt0z = vertex.co
        result.append((pt0x, pt0y, pt0z),)
    result.sort()
    return result

def get_center(ob):
    """Return object center in global coordinates"""
    return [(ob.location[0], ob.location[1], ob.location[2]), ]

def get_boxels(ob, voxel_size, solid):
    """Return object boxels by summing voxels in as big as possible boxes"""
    # Get voxels
    voxels = _voxelize(ob, voxel_size, solid)
    # Timing
    t0 = time()
    # Init boxes and start. boxes is now in integer voxel coordinates
    boxes = list()
    while voxels:
        # Get a voxel and init the corresponding box
        voxel = voxels.pop()
        box = [voxel[0], voxel[0], voxel[1], voxel[1], voxel[2], voxel[2]]
        # Now box size is 1*1*1
        # Expand box in +x direction
        while True:
            voxels_desired = set()
            voxels_desired.add((box[1] + 1, box[2], box[4]))
            if not voxels_desired <= voxels: break
            voxels -= voxels_desired
            box[1] += 1
        # Expand box in -x direction
        while True:
            voxels_desired = set()
            voxels_desired.add((box[0] - 1, box[2], box[4]))
            if not voxels_desired <= voxels: break
            voxels -= voxels_desired
            box[0] -= 1
        # Now box size is n*1*1
        # Expand box in +y direction
        while True:
            voxels_desired = set()
            for idx in range(box[0], box[1] + 1):
                voxels_desired.add((idx, box[3] + 1, box[4]))
            if not voxels_desired <= voxels: break
            voxels -= voxels_desired
            box[3] += 1
        # Expand box in -y direction
        while True:
            voxels_desired = set()
            for idx in range(box[0], box[1] + 1):
                voxels_desired.add((idx, box[2] - 1, box[4]))
            if not voxels_desired <= voxels: break
            voxels -= voxels_desired
            box[2] -= 1
        # Now box size is n*m*1
        # Expand box in +z direction
        while True:
            voxels_desired = set()
            for idx in range(box[0], box[1] + 1):
                for idy in range(box[2], box[3] + 1):
                    voxels_desired.add((idx, idy, box[5] + 1))
            if not voxels_desired <= voxels: break
            voxels -= voxels_desired
            box[5] += 1
        # Expand box in -z direction
        while True:
            voxels_desired = set()
            for idx in range(box[0], box[1] + 1):
                for idy in range(box[2], box[3] + 1):
                    voxels_desired.add((idx, idy, box[4] - 1))
            if not voxels_desired <= voxels: break
            voxels -= voxels_desired
            box[4] -= 1
        # Now box size is n*m*l
        # Append as big as possible box to boxes list
        boxes.append(box)
    # Sort boxes and export boxes in global coos
    boxes.sort()
    print("   boxelize:", time()-t0)
    return [((box[0]-.5) * voxel_size[0], (box[1]+.5) * voxel_size[0],
             (box[2]-.5) * voxel_size[1], (box[3]+.5) * voxel_size[1],
             (box[4]-.5) * voxel_size[2], (box[5]+.5) * voxel_size[2]) for box in boxes]

### Manage mesh cells for FDS MESH object
# input:  ob
# output: [...]

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

def get_mesh_cells(ob):
    """Calc optimized and valid value for MESH IJK from voxel size"""
    cell_size = ob.bf_cell_size
    bbminx, bbmaxx, bbminy, bbmaxy, bbminz, bbmaxz = get_bbox(ob)[0]
    ijk = (int(round((bbmaxx - bbminx) / cell_size[0])),
           _n_for_poisson(int(round((bbmaxy - bbminy) / cell_size[1]))),
           _n_for_poisson(int(round((bbmaxz - bbminz) / cell_size[2]))) )
    quantity = ijk[0] * ijk[1] * ijk[2] 
    cell_size = ((bbmaxx - bbminx) / ijk[0], (bbmaxy - bbminy) / ijk[1], (bbmaxz - bbminz) / ijk[2] )
    return {"ijk": ijk, "quantity": quantity, "cell_size": cell_size}

