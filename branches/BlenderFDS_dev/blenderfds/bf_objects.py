# ##### BEGIN GPL LICENSE BLOCK #####
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 3
#  of the License, or (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#  GNU General Public License for more details.
#na
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software Foundation,
#  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# ##### END GPL LICENSE BLOCK #####
"""BlenderFDS, an open tool for the NIST Fire Dynamics Simulator"""

import bpy, os
from .bf_types import BFProp, BFNamelist, BFSection, bf_props, bf_namelists, bf_sections
from .bf_basic_types import BFResult, BFError
from . import bf_geometry, bf_config

### Mods

class NoExport():
    def is_exported(self, context, element): return False

class NoUI():
    def draw(self, context, element, layout): pass

### Good practices
# object namelist unique_id shall be in the range 1000-1999, OBST is 1000
# material namelist unique_id shall be in the range 2000-2999, SURF is 2000
# name is as similar as possible to label: "Label"
# label has unit in square brackets: "Label [unit]"
# bpy_name is "bf_lowercase_label"
# class names are as ClassName

### Special BFProp: namelist
# bf_namelist items are updated at the end of this file

class BFPropNamelist(NoExport, BFProp): pass

def ob_namelist_update(self, context):
    """On bf_prop["Ob Namelist"] update"""
    # Set geometry to None, as different namelists have different geometric possibilities
    ob = context.object
    ob.bf_xb, ob.bf_xyz, ob.bf_pb = "NONE", "NONE", "NONE"

BFPropNamelist(
    name = "Ob Namelist",
    label = "Namelist",
    description = "Type of FDS namelist",
    bpy_name = "bf_namelist",
    bpy_prop = bpy.props.EnumProperty,
    update = ob_namelist_update,
)

BFPropNamelist(
    name = "Ma Namelist",
    label = "Namelist",
    description = "Type of FDS namelist",
    bpy_name = "bf_namelist",
    bpy_prop = bpy.props.EnumProperty,
)

### Special BFProp: tmp
# Used to distiguish temporary objects and their fathers

class BFPropTmp(NoExport, NoUI, BFProp): pass

BFPropTmp(
    name = "Is Tmp",
    label = "Is Tmp",
    description = "Set if this object is tmp",
    bpy_name = "bf_is_tmp",
    bpy_prop = bpy.props.BoolProperty,
    default = False,
)

BFPropTmp(
    name = "Has Tmp",
    label = "Has Tmp",
    description = "Set if this object has a visible tmp object companion",
    bpy_name = "bf_has_tmp",
    bpy_prop = bpy.props.BoolProperty,
    default = False,
)

### Special BFProp: SURF_ID

class BFPropSURFID(BFProp):
    def is_exported(self,context,element):
        return element.active_material and element.active_material.bf_namelist_export or False

    def evaluate(self,context,element):
        if element.active_material:
            return BFResult(sender=self, value=element.active_material.name)
        
    def draw_bf_props(self,context,element,layout):
        row = layout.row()
        row.active = self.is_exported(context, element)
        row.prop_search(element, self.bpy_name, bpy.data, "materials", text=self.label)

BFPropSURFID(
    name = "SURF_ID",
    label = "SURF_ID:",
    description = "Reference to SURF",
    fds_name = "SURF_ID",
    bpy_name = "active_material",
)

# FIXME dimension_too_large?

### Special BFProp: Geometry, XB, XYZ, PB

class BFPropGeometry(BFProp):
    items = "NONE",

    def draw_bf_props(self, context, element, layout):
        # Draw enum
        split = layout.split(.1)
        col1, col2 = split.row(), split.row(align=True)
        col1.label(text="{}:".format(self.label))
        for item in self.items: col2.prop_enum(element, self.bpy_name, item)

# XB

class BFPropXB(BFPropGeometry):
    items = "NONE", "BBOX", "VOXELS", "FACES", "EDGES"

    def draw_extra(self, context, element, layout):
        if element.bf_xb == "VOXELS": layout.prop(element, "bf_xb_voxel_size")

    def format(self, value):
        return "XB={0[0]:.3f},{0[1]:.3f},{0[2]:.3f},{0[3]:.3f},{0[4]:.3f},{0[5]:.3f}".format(value)
    
    def to_fds(self, context, element):
        # Check and init
        res = self.evaluate(context, element)
        # Select case
        if res.value == "NONE" or res.value not in self.items:
            return None
        elif res.value == "BBOX":
            xbs, tt = bf_geometry.get_bbox(self, context, element)
        elif res.value == "VOXELS":
            xbs, tt = bf_geometry.get_voxels(self, context, element)
            res.msgs.append("{0} voxels of size {1:.3f} m, in {2:.3f} s".format(len(xbs),element.bf_xb_voxel_size,tt))
        elif res.value == "FACES":
            xbs, tt = bf_geometry.get_faces(self, context, element)
            res.msgs.append("{0} faces, in {1:.3f} s".format(len(xbs), tt))
        elif res.value == "EDGES":
            xbs, tt = bf_geometry.get_edges(self, context, element)
            res.msgs.append("{0} edges, in {1:.3f} s".format(len(xbs), tt))
        else: raise Exception("XB type unknown")
        # Manage multivalues
        if len(xbs) == 1: res.value = self.format(xbs[0])
        else: res.value = tuple((self.format(xb) for xb in xbs))
        return res

BFProp(
    name = "Voxel Size",
    label = "Voxel Size [m]",
    description = "Minimum resolution for object voxelization",
    bpy_name = "bf_xb_voxel_size",
    bpy_prop = bpy.props.FloatProperty,
    step = 1,
    precision = 3,
    min = .01,
    max = 2.,
    default = .10,
    update = bf_geometry.del_all_tmp_objects,
)

def xb_update(self, context):
    """On bf_prop["XB"] update"""
    # Del all tmp_objects
    bf_geometry.del_all_tmp_objects(self, context)
    # Set other geometries to compatible settings
    ob = context.object
    if ob.bf_xb in ("VOXELS", "FACES", "EDGES"):
        if ob.bf_xyz == "VERTICES": ob.bf_xyz = "NONE"
        ob.bf_pb = "NONE"

BFPropXB(
    name = "XB",
    label = "XB",
    description = "XB",
    fds_name = "XB",
    bf_props = ("Voxel Size", "Has Tmp", "Is Tmp"),
    bpy_name = "bf_xb",
    bpy_prop = bpy.props.EnumProperty,
    items = (
        ("NONE", "None", "Not exported", 0),
        ("BBOX", "BBox", "Use object bounding box", 100),
        ("VOXELS", "Voxels", "Export voxels from voxelized solid", 200),
        ("FACES", "Faces", "Faces, one for each face of this object", 300),
        ("EDGES", "Edges", "Segments, one for each edge of this object", 400),
        ),
    update = xb_update,
)

class BFPropXBBBox(BFPropXB):
    items = "NONE", "BBOX",

BFPropXBBBox(
    name = "XB Bounding Box",
    label = "XB",
    description = "XB",
    fds_name = "XB",
    bpy_name = "bf_xb",
)

class BFPropXBSolid(BFPropXB):
    items = "NONE", "BBOX", "VOXELS"

BFPropXBSolid(
    name = "XB Solid",
    label = "XB",
    description = "XB",
    fds_name = "XB",
    bpy_name = "bf_xb",
)

class BFPropXBFaces(BFPropXB):
    items = "NONE", "FACES"

BFPropXBFaces(
    name = "XB Faces",
    label = "XB",
    description = "XB",
    fds_name = "XB",
    bpy_name = "bf_xb",
)

# XYZ

class BFPropXYZ(BFPropGeometry):
    items = "NONE", "CENTER", "VERTICES"
    
    def format(self, value):
        return "XYZ={0[0]:.3f},{0[1]:.3f},{0[2]:.3f}".format(value)
        
    def to_fds(self, context, element):
        # Check and init
        res = self.evaluate(context, element)
        # Select case
        if res.value == "NONE" or res.value not in self.items:
            return None
        elif res.value == "CENTER":
            xyzs, tt = bf_geometry.get_center(self, context, element)
        elif res.value == "VERTICES":
            xyzs, tt = bf_geometry.get_vertices(self, context, element)
            res.msgs.append("{0} vertices, in {1:.3f} s".format(len(xyzs), tt))
        else: raise Exception("XYZ type unknown")
        # Manage multivalues
        if len(xyzs) == 1: res.value = self.format(xyzs[0])
        else: res.value = tuple((self.format(xyz) for xyz in xyzs))
        return res

def xyz_update(self, context):
    """On bf_prop["XYZ"] update"""
    # Del all tmp_objects
    bf_geometry.del_all_tmp_objects(self, context)
    # Set other geometries to compatible settings
    ob = context.object
    if ob.bf_xyz == "VERTICES":
        if ob.bf_xb in ("VOXELS", "FACES", "EDGES"): ob.bf_xb = "NONE"
        ob.bf_pb = "NONE"

BFPropXYZ(
    name = "XYZ",
    label = "XYZ",
    description = "Set points",
    fds_name = "XYZ",
    bpy_name = "bf_xyz",
    bpy_prop = bpy.props.EnumProperty,
    items = [
        ("NONE", "None", "Not exported", 0),
        ("CENTER", "Center", "Point, corresponding to the center point of this object", 100),
        ("VERTICES", "Vertices", "Points, one for each vertex of this object", 200),
        ],
    update = xyz_update,
)

# PB
# FIXME what if no faces?

class BFPropPB(BFPropGeometry):
    items = "NONE", "PLANES"

    def format(self,value):
        return "PB{0[0]}={0[1]:.3f}".format(value)
    
    def to_fds(self,context,element):
        # Check and init
        res = self.evaluate(context, element)
        # Select case
        if res.value == "NONE" or res.value not in self.items:
            return None
        elif res.value == "PLANES":
            pbs, tt = bf_geometry.get_planes(self, context, element)
            res.msgs.append("{0} planes, in {1:.3f} s".format(len(pbs), tt))
        else: raise Exception("PB type unknown")
        # Manage multivalues
        if len(pbs) == 1: res.value = self.format(pbs[0])
        else: res.value = tuple((self.format(pb) for pb in pbs))
        return res

def pb_update(self, context):
    """On bf_prop["PB"] update"""
    # Del all tmp_objects
    bf_geometry.del_all_tmp_objects(self, context)
    # Set other geometries to compatible settings
    ob = context.object
    if ob.bf_pb == "PLANES":
        if ob.bf_xb in ("VOXELS", "FACES", "EDGES"): ob.bf_xb = "NONE"
        if ob.bf_xyz == "VERTICES": ob.bf_xyz = "NONE"

BFPropPB(
    name = "PB",
    label = "PB*",
    description = "Set planes",
    fds_name = "PB",
    bpy_name = "bf_pb",
    bpy_prop = bpy.props.EnumProperty,
    items = (
        ("NONE", "None", "Not exported", 0),
        ("PLANES", "Planes", "Planes, one for each face of this object", 100),
        ),
    update = pb_update,
)

### Special BFProp: Show Geometries

class BFPropShowGeometries(BFProp):
    def draw(self, context, element, layout):
        row = layout.split(.5)
        row.label(text="")
        if element.bf_has_tmp: row.operator("scene.bf_del_all_tmp_objects")
        else: row.operator("object.bf_show_fds_geometries")

# no bpy_prop, this BFProp is a placeholder
BFPropShowGeometries(
    name = "Show Geometries",
)

### Special BFProp: Custom

class BFPropCustom(BFProp):
    def evaluate(self, context,element):
        res = BFResult(sender=self, value=self.value(context, element))
        err = BFError(self)
        if isinstance(res.value, str):
            if '&' in res.value or '/' in res.value:
                err.msgs.append("& and / characters not needed")
            if "`" in res.value or "‘" in res.value or "’‌" in res.value:
                err.msgs.append("Typographic quotes not allowed, use matched single quotes")
            if '"' in res.value or "”" in res.value:
                err.msgs.append("Double quotes not allowed, use matched single quotes")
            if res.value.count("'") % 2 != 0:
                err.msgs.append("Unmatched single quotes")
        if err.msgs: raise err
        return res

    def draw_bf_props(self, context, element, layout):
        # Draw self
        row = layout.row()
        row.prop(element, self.bpy_name, text="", icon="TEXT")
        # Draw related bf_props
        for bf_prop in self.bf_props: bf_prop.draw(context, element, layout)

class BFPropObCustom(BFPropCustom):
    def draw_extra(self, context, element, layout):
        row = layout.split(.5)
        row.label(text="")
        row.operator("object.bf_props_to_sel_obs") # Copy properties to obs
        
class BFPropMaCustom(BFPropCustom):
    def draw_extra(self, context, element, layout):
        row = layout.split(.5)
        row.label(text="")
        row.operator("material.bf_surf_to_sel_obs") # Copy properties to obs

BFPropObCustom(
    name = "Ob Custom",
    label = "Custom Parameters:",
    description = "Free text parameters, use matched single quotes, eg PROP1='example'",
    bpy_name = "bf_custom",
    bpy_prop = bpy.props.StringProperty,
    maxlen = 1024,
)

BFPropMaCustom(
    name = "Ma Custom",
    label = "Custom Parameters:",
    description = "Free text parameters, use matched single quotes, eg PROP1='example'",
    bpy_name = "bf_custom",
    bpy_prop = bpy.props.StringProperty,
    maxlen = 1024,
)

BFPropCustom(
    name = "TIME Custom",
    label = "Custom Parameters:",
    description = "Free text parameters, use matched single quotes, eg PROP1='example'",
    bpy_name = "bf_time_custom",
    bpy_prop = bpy.props.StringProperty,
    maxlen = 1024,
)

BFPropCustom(
    name = "MISC Custom",
    label = "Custom Parameters:",
    description = "Free text parameters, use matched single quotes, eg PROP1='example'",
    bpy_name = "bf_misc_custom",
    bpy_prop = bpy.props.StringProperty,
    maxlen = 1024,
)

BFPropCustom(
    name = "REAC Custom",
    label = "Custom Parameters:",
    description = "Free text parameters, use matched single quotes, eg PROP1='example'",
    bpy_name = "bf_reac_custom",
    bpy_prop = bpy.props.StringProperty,
    maxlen = 1024,
)

BFPropCustom(
    name = "DUMP Custom",
    label = "Custom Parameters:",
    description = "Free text parameters, use matched single quotes, eg PROP1='example'",
    bpy_name = "bf_dump_custom",
    bpy_prop = bpy.props.StringProperty,
    maxlen = 1024,
)

BFPropCustom(
    name = "Custom Namelist",
    label = "Custom Namelist:",
    description = "Free text namelist, & and / not needed, use single quotes, eg OBST PROP1='example'",
    bpy_name = "bf_custom_namelist",
    bpy_prop = bpy.props.StringProperty,
    maxlen = 1024,
    default = "OBST ID='Example'"
)
 
### Special BFProp: Namelist export

BFProp(
    name = "Namelist Export",
    label = "Export",
    description = "Export namelist to FDS",
    bpy_name = "bf_namelist_export",
    bpy_prop = bpy.props.BoolProperty,
    default = False,
)

### Other BFProp 

BFProp(
    name = "ID",
    label = "ID",
    description = "Element identificator",
    fds_name = "ID",
    bpy_name = "name",
)

class BFPropIDNoExport(NoExport, BFProp): pass

BFPropIDNoExport(
    name = "ID No Export",
    label = "ID",
    description = "Element identificator, not exported",
    fds_name = "ID",
    bpy_name = "name",
)

BFProp(
    name = "FYI",
    label = "FYI",
    description = "Free text description",
    fds_name = "FYI",
    bpy_name = "bf_fyi",
    bpy_prop = bpy.props.StringProperty,
    maxlen = 128,
)

class BFPropFilename(BFProp):
    def evaluate(self, context, element):
        res = BFResult(sender=self, value=self.value(context, element))
        if bpy.path.clean_name(res.value) != res.value: raise BFError(self, "Illegal characters in filename")
        return res

BFPropFilename(
    name = "CHID",
    label = "CHID",
    description = "Case identificator",
    fds_name = "CHID",
    bpy_name = "name",
)

BFProp(
    name = "TITLE",
    label = "TITLE",
    description = "Case description",
    fds_name = "TITLE",
    bpy_name = "bf_head_title",
    bpy_prop = bpy.props.StringProperty,
    maxlen = 64,
)

class BFPropPath(NoExport, BFProp):
    def evaluate(self, context, element):
        res = BFResult(sender=self, value=self.value(context, element))
        if not os.path.exists(bpy.path.abspath(res.value)): raise BFError(self, "Path does not exist")
        return res

BFPropPath(
    name = "Case Directory",
    label = "Case Directory",
    description = "Case directory",
    bpy_name = "bf_case_directory",
    bpy_prop = bpy.props.StringProperty,
    subtype = "DIR_PATH",
    maxlen = 1024,
)

BFPropPath(
    name = "Ext Config File",
    label = "Ext'l Config File",
    description = "Path to external configuration file",
    has_export_flag = True,
    bpy_name = "bf_case_config_filepath",
    bpy_prop = bpy.props.StringProperty,
    default = "//config.fds",
    subtype = "FILE_PATH",
    maxlen = 1024,
)

class BFPropVersion(NoExport, NoUI, BFProp): pass
# FIXME bl_info["version"]
BFPropVersion(
    name = "Version",
    label = "Version",
    description = "BlenderFDS version used for file creation and exporting",
    bpy_name = "bf_case_version",
    bpy_prop = bpy.props.IntVectorProperty,
    default = (0,0,0),
    size = 3,
)

class BFPropTBEGIN(BFProp):
    def is_exported(self, context, element):
        # No T_BEGIN if SMV setup
        return not element.bf_time_smv_setup

BFPropTBEGIN(
    name = "T_BEGIN",
    label = "T_BEGIN [s]",
    description = "Simulation starting time",
    fds_name = "T_BEGIN",
    bpy_name = "bf_time_t_begin",
    bpy_prop = bpy.props.FloatProperty,
    step = 100.,
    precision = 1,
    min = 0.,
    default = 0.,
)

class BFPropTEND(BFProp):
    def evaluate(self, context, element):
        # T_END=0 if SMV setup
        if element.bf_time_smv_setup: value = 0.
        else: value = element.bf_time_t_end
        return BFResult(self, value=value)

BFPropTEND(
    name = "T_END",
    label = "T_END [s]",
    description = "Simulation ending time",
    fds_name = "T_END",
    bpy_name = "bf_time_t_end",
    bpy_prop = bpy.props.FloatProperty,
    step = 100.,
    precision = 1,
    min = 0.,
    default= 0.,
)

class BFPropSMVSetup(NoExport, BFProp): pass

BFPropSMVSetup(
    name = "SMV setup",
    label = "Smokeview setup only",
    description = "Perform Smokeview setup only",
    bpy_name = "bf_time_smv_setup",
    bpy_prop = bpy.props.BoolProperty,
    default = False,
)

BFProp(
    name = "FUEL",
    label = "FUEL",
    description = "Identificator of fuel species",
    fds_name = "FUEL",
    bpy_name = "bf_reac_fuel",
    bpy_prop = bpy.props.StringProperty,
    maxlen = 32,
)

BFProp(
    name = "FORMULA",
    label = "FORMULA",
    description = "Chemical formula of fuel species, it can only contain C, H, O, or N",
    fds_name = "FORMULA",
    has_export_flag = True,
    bpy_name = "bf_reac_formula",
    bpy_prop = bpy.props.StringProperty,
    maxlen = 32,
)

BFProp(
    name = "CO_YIELD",
    label = "CO_YIELD [kg/kg]",
    description = "Fraction of fuel mass converted into carbon monoxide",
    fds_name = "CO_YIELD",
    has_export_flag = True,
    bpy_name = "bf_reac_co_yield",
    bpy_prop = bpy.props.FloatProperty,
    step = 1.,
    precision = 2,
    min = 0.,
    max = 1.,
    default = 0.,
)

BFProp(
    name = "SOOT_YIELD",
    label = "SOOT_YIELD [kg/kg]",
    description = "Fraction of fuel mass converted into smoke particulate",
    fds_name = "SOOT_YIELD",
    has_export_flag = True,
    bpy_name = "bf_reac_soot_yield",
    bpy_prop = bpy.props.FloatProperty,
    step = 1.,
    precision = 2,
    min = 0.,
    max = 1.,
    default = 0.,
)

BFProp(
    name = "HEAT_OF_COMBUSTION",
    label = "HEAT_OF_COMBUSTION [kJ/kg]",
    description = "Fuel heat of combustion",
    fds_name = "HEAT_OF_COMBUSTION",
    has_export_flag = True,
    bpy_name = "bf_reac_heat_of_combustion",
    bpy_prop = bpy.props.FloatProperty,
    step = 100000.,
    precision = 1,
    min = 0.,
    default = 0.,
)

class BFPropNFRAMES(BFProp):
    def evaluate(self, context, element):
        msgs = "Output is dumped every {:.2f}\"".format((element.bf_time_t_end - element.bf_time_t_begin) / element.bf_dump_nframes)
        return BFResult(self, value=element.bf_dump_nframes, msgs=msgs)

BFPropNFRAMES(
    name = "NFRAMES",
    label = "NFRAMES",
    description = "Number of output dumps per calculation",
    fds_name = "NFRAMES",
    has_export_flag = True,
    bpy_name = "bf_dump_nframes",
    bpy_prop = bpy.props.IntProperty,
    min = 1,
    default = 1000,
)

BFProp(
    name = "QUANTITY",
    label = "QUANTITY",
    description = "Output quantity",
    fds_name = "QUANTITY",
    bpy_name = "bf_devc_quantity",
    bpy_prop = bpy.props.StringProperty,
    maxlen = 32,
)

BFProp(
    name = "VECTOR",
    label = "VECTOR",
    description = "Create animated vectors",
    fds_name = "VECTOR",
    bpy_name = "bf_slcf_vector",
    bpy_prop = bpy.props.BoolProperty,
    default = True,
)

class BFPropIJK(BFProp):
    def evaluate(self, context, element):
        res = BFResult(sender=self, value=element.bf_mesh_ijk)
        # Init
        bf_mesh_ijk = res.value
        dimensions = element.dimensions
        cell_size = [dimensions[0] / bf_mesh_ijk[0], dimensions[1] / bf_mesh_ijk[1], dimensions[2] / bf_mesh_ijk[2],]
        cell_number = bf_mesh_ijk[0] * bf_mesh_ijk[1] * bf_mesh_ijk[2]        
        # Good IJK
        if tuple(bf_mesh_ijk) != bf_geometry.get_good_ijk(bf_mesh_ijk):
            res.msgs.append("J and K not optimal for Poisson solver")
            res.operators.append("object.bf_correct_ijk")
        # Cells
        res.msgs.append("{0} mesh cells of size {1[0]:.3f} m x {1[1]:.3f} m x {1[2]:.3f} m".format(cell_number, cell_size))
        # Aspect ratio
        cell_size.sort()
        max_ratio = max(cell_size[2] / cell_size[0], cell_size[2] / cell_size[1], cell_size[1] / cell_size[0])
        if max_ratio > 2.: res.msgs.append("Max cell aspect ratio is {:.1f}".format(max_ratio))      
        # Return
        return res

BFPropIJK(
    name = "IJK",
    label = "IJK",
    description = "Cell number in x, y, and z direction",
    fds_name = "IJK",
    has_export_flag = True,
    bpy_name = "bf_mesh_ijk",
    bpy_prop = bpy.props.IntVectorProperty,
    default = (10,10,10),
    size = 3,
)

class BFPropRGB(BFProp):
    def evaluate(self,context,element):
        diffuse_color = element.diffuse_color
        return BFResult(self, value=(int(diffuse_color[0]*255), int(diffuse_color[1]*255), int(diffuse_color[2]*255)))

BFPropRGB(
    name = "RGB",
    label = "RGB",
    description = "RGB",
    fds_name = "RGB",
    bpy_name = "diffuse_color",
)

BFProp(
    name = "TRANSPARENCY export",
    label = "Export",
    description = "Export parameter",
    bpy_name = "use_transparency",
)

BFProp(
    name = "TRANSPARENCY",
    label = "TRANSPARENCY",
    description = "Transparency",
    fds_name = "TRANSPARENCY",
    has_export_flag = True,
    bf_prop_export = "TRANSPARENCY export",
    bpy_name = "alpha",
)

BFProp(
    name = "HRRPUA",
    label = "HRRPUA [kW/m²]",
    description = "Heat release rate per unit area",
    fds_name = "HRRPUA",
    bpy_name = "bf_surf_hrrpua",
    bpy_prop = bpy.props.FloatProperty,
    step = 1000,
    precision = 1,
    min = 0.,
    default = 1000.,
)


class BFPropTAUQ(BFProp):
    def evaluate(self, context, element):
        return BFResult(
            sender = self,
            value = element.bf_surf_tau_q,
            msgs = element.bf_surf_tau_q <= 0 and "HRR(t) has a t² ramp" or "HRR(t) has a tanh(t/τ) ramp",
            operators = "material.set_tau_q"
            )

BFPropTAUQ(
    name = "TAU_Q",
    label = "TAU_Q [s]",
    description = "Ramp time for heat release rate",
    fds_name = "TAU_Q",
    bpy_name = "bf_surf_tau_q",
    bpy_prop = bpy.props.FloatProperty,
    step = 10,
    precision = 1,
    default = 100.,
)

BFProp(
    name = "INITIAL_STATE",
    label = "INITIAL_STATE",
    description = "Set device initial state to .TRUE.",
    fds_name = "INITIAL_STATE",
    bpy_name = "bf_devc_initial_state",
    bpy_prop = bpy.props.BoolProperty,
    default = False,
)

BFProp(
    name = "LATCH",
    label = "LATCH",
    description = "Device only changes state once",
    fds_name = "LATCH",
    bpy_name = "bf_devc_latch",
    bpy_prop = bpy.props.BoolProperty,
    default = True,
)

BFProp(
    name = "SETPOINT",
    label = "SETPOINT [~]",
    description = "Value of the device at which its state changes",
    fds_name = "SETPOINT",
    has_export_flag = True,
    bpy_name = "bf_devc_setpoint",
    bpy_prop = bpy.props.FloatProperty,
    step = 10,
    precision = 1,
    default = 100.,
)

BFProp(
    name = "PROP_ID",
    label = "PROP_ID",
    description = "Reference to a PROP (Property) line for self properties",
    fds_name = "PROP_ID",
    has_export_flag = True,
    bpy_name = "bf_devc_prop_id",
    bpy_prop = bpy.props.StringProperty,
    maxlen = 32,
)

# BFNamelists

BFNamelist(
    name = "HEAD",
    label = "HEAD",
    description = "Header",
    fds_name = "HEAD",
    unique_id = 1001,
    bpy_type = bpy.types.Scene,
    bf_props = ("CHID","TITLE"),
)

class BFNamelistCaseConfiguration(NoExport, BFNamelist): pass

BFNamelistCaseConfiguration(
    name = "Case Configuration",
    label = "Case Configuration",
    description = None,
    unique_id = 1002,
    bpy_type = bpy.types.Scene,
    bf_props = ("Case Directory","Ext Config File","Version"),
)

class BFNamelistTIME(BFNamelist):
    def draw_bf_props(self, context, element, layout):
        bf_time_smv_setup = element.bf_time_smv_setup
        row = layout.row()
        row.active = not bf_time_smv_setup
        for b_prop in self.bf_props[0:2]: # T_BEGIN and T_END non active if SMV setup only
            row.prop(element, b_prop.bpy_name, text=b_prop.label)
        self.bf_props["SMV setup"].draw(context, element, layout) # Draw SMV setup

BFNamelistTIME(
    name = "TIME",
    label = "TIME",
    description = "Time",
    fds_name = "TIME",
    unique_id = 1003,
    bpy_type = bpy.types.Scene,
    bf_props = ("T_BEGIN", "T_END", "SMV setup", "TIME Custom"),
)

BFNamelist(
    name = "MISC",
    label = "MISC",
    description = "Miscellaneous parameters",
    fds_name = "MISC",
    unique_id = 1004,
    has_export_flag = True,
    bpy_type = bpy.types.Scene,
    bf_props = ("MISC Custom",),
)

BFNamelist(
    name = "REAC",
    label = "REAC",
    description = "Reaction",
    fds_name = "REAC",
    unique_id = 1005,
    has_export_flag = True,
    bpy_type = bpy.types.Scene,
    bf_props = ("FUEL","FORMULA","CO_YIELD","SOOT_YIELD","HEAT_OF_COMBUSTION","REAC Custom"),
)

BFNamelist(
    name = "DUMP",
    label = "DUMP",
    description = "Output parameters",
    fds_name = "DUMP",
    unique_id = 1006,
    has_export_flag = True,
    bpy_type = bpy.types.Scene,
    bf_props = ("NFRAMES","DUMP Custom"),
)

BFNamelist(
    name = "Custom namelist",
    label = "Custom namelist",
    description = None,
    unique_id = 1007,
    has_export_flag = True,
    bf_prop_export = "Namelist Export",
    bpy_type = bpy.types.Object,
    bf_props = ("Ob Namelist", "Custom Namelist", "FYI", "SURF_ID", "XB", "XYZ", "PB", "Show Geometries"),
)

BFNamelist(
    name = "OBST",
    label = "OBST",
    description = "Obstruction",
    fds_name = "OBST",
    unique_id = 1000,
    has_export_flag = True,
    bf_prop_export = "Namelist Export",
    bpy_type = bpy.types.Object,
    bf_props = ("Ob Namelist", "ID", "FYI", "SURF_ID", "XB Solid", "Show Geometries", "Ob Custom",),
)

BFNamelist(
    name = "HOLE",
    label = "HOLE",
    description = "Obstruction Cutout",
    fds_name = "HOLE",
    unique_id = 1009,
    has_export_flag = True,
    bf_prop_export = "Namelist Export",
    bpy_type = bpy.types.Object,
    bf_props = ("Ob Namelist", "ID No Export", "FYI", "XB Solid", "Show Geometries", "Ob Custom",),
)

BFNamelist(
    name = "VENT",
    label = "VENT",
    description = "Boundary Condition Patch",
    fds_name = "VENT",
    unique_id = 1010,
    has_export_flag = True,
    bf_prop_export = "Namelist Export",
    bpy_type = bpy.types.Object,
    bf_props = ("Ob Namelist", "ID", "FYI", "SURF_ID", "XB Faces", "XYZ", "PB", "Show Geometries", "Ob Custom",),
)

BFNamelist(
    name = "DEVC",
    label = "DEVC",
    description = "Device",
    fds_name = "DEVC",
    unique_id = 1011,
    has_export_flag = True,
    bf_prop_export = "Namelist Export",
    bpy_type = bpy.types.Object,
    bf_props = ("Ob Namelist", "ID", "FYI", "QUANTITY", "SETPOINT", "INITIAL_STATE", "LATCH", "PROP_ID", "XB", "XYZ", "Show Geometries", "Ob Custom",),
)

BFNamelist(
    name = "SLCF",
    label = "SLCF",
    description = "Slice File",
    fds_name = "SLCF",
    unique_id = 1012,
    has_export_flag = True,
    bf_prop_export = "Namelist Export",
    bpy_type = bpy.types.Object,
    bf_props = ("Ob Namelist", "ID No Export", "FYI", "QUANTITY", "VECTOR", "XB Faces", "PB", "Show Geometries", "Ob Custom",),
)

BFNamelist(
    name = "PROF",
    label = "PROF",
    description = "Wall Profile Output",
    fds_name = "PROF",
    unique_id = 1013,
    has_export_flag = True,
    bf_prop_export = "Namelist Export",
    bpy_type = bpy.types.Object,
    bf_props = ("Ob Namelist", "ID", "FYI", "QUANTITY", "XYZ", "Show Geometries", "Ob Custom",),
)

BFNamelist(
    name = "MESH",
    label = "MESH",
    description = "Domain of simulation",
    fds_name = "MESH",
    unique_id = 1014,
    has_export_flag = True,
    bf_prop_export = "Namelist Export",
    bpy_type = bpy.types.Object,
    bf_props = ("Ob Namelist", "ID", "FYI", "IJK", "XB Bounding Box", "Show Geometries", "Ob Custom",),
)

BFNamelist(
    name = "INIT",
    label = "INIT",
    description = "Initial condition",
    fds_name = "INIT",
    unique_id = 1015,
    has_export_flag = True,
    bf_prop_export = "Namelist Export",
    bpy_type = bpy.types.Object,
    bf_props = ("Ob Namelist", "ID", "FYI", "XB Solid", "XYZ", "Show Geometries", "Ob Custom",),
)

BFNamelist(
    name = "ZONE",
    label = "ZONE",
    description = "Pressure zone",
    fds_name = "ZONE",
    unique_id = 1016,
    has_export_flag = True,
    bf_prop_export = "Namelist Export",
    bpy_type = bpy.types.Object,
    bf_props = ("Ob Namelist", "ID", "FYI", "XB Bounding Box", "Show Geometries", "Ob Custom",),
)

class BFNamelistSURF(BFNamelist):
    def evaluate(self, context, element):
        if not set(bf_config.predefined_material_names) <= set(bpy.data.materials.keys()):
            raise BFError(sender=self, msgs="Predefined SURFs unset", operators="material.bf_set_predefined")
        return BFResult(sender=self)

BFNamelistSURF(
    name = "SURF",
    label = "SURF",
    description = "Generic Boundary Condition",
    fds_name = "SURF",
    unique_id = 2000,
    has_export_flag = True,
    bf_prop_export = "Namelist Export",
    bpy_type = bpy.types.Material,
    bf_props = ("Ma Namelist", "ID", "FYI", "RGB", "TRANSPARENCY", "Ma Custom",),
)

BFNamelistSURF(
    name = "SURF burner",
    label = "SURF burner",
    description = "A simple burner",
    fds_name = "SURF",
    unique_id = 2001,
    has_export_flag = True,
    bf_prop_export = "Namelist Export",
    bpy_type = bpy.types.Material,
    bf_props = ("Ma Namelist", "ID", "FYI", "RGB", "HRRPUA", "TAU_Q", "Ma Custom",),
)

## BFSections

BFSection(
    name = "General configuration",
    bf_namelists = ("HEAD","TIME","MISC","REAC","DUMP"),
)

class BFSectionExtConfig(BFSection):
    def to_fds(self,context,element):
        print("BlenderFDS: > BFSection.to_fds: {}".format(self.name))
        bf_prop = bf_props["Ext Config File"]
        if not bf_prop.bf_prop_export.value(context,context.scene): return None # Nothing to send
        # Evaluate res to get external config filepath
        res = bf_prop.evaluate(context,context.scene) # Do not trap exceptions, pass them upward
        if res is None or res.value is None: return res # Could have msgs
        filepath = bpy.path.abspath(res.value)
        # Read file
        try:
            with open(filepath) as in_file:
                res.value = in_file.read() + "\n"
        except IOError: raise BFError(sender=self, msgs="External config file not readable: {}".format(filepath))
        # Check
        res.msgs.append(filepath)
        res.value = self.format(res.value, res.labels)
        return res

BFSectionExtConfig(
    name = "External Config File",
)

BFSection(
    name = "Boundary Conditions",
    bf_namelists = ("SURF","SURF burner"),
)

BFSection(
    name = "Computational Domain",
    bf_namelists = ("MESH","INIT","ZONE"),
)

BFSection(
    name = "Geometry",
    bf_namelists = ("OBST","HOLE","VENT"),
)

# FIXME ("EVAC","EVHO","EXIT","ENTR","DOOR","CORR","EVSS","STRS")
#BFSection(
#    name = "Evacuation",
#    bf_namelists = (),
#)

BFSection(
    name = "Control Logic and Output",
    bf_namelists = ("DEVC", "SLCF", "PROF"),
)

BFSection(
    name = "Other",
    bf_namelists = ("Custom namelist"),
)

### Update bf_namelist menu with all namelists

def get_bf_namelist_items(bpy_type):
    """Get all bf_namelist.menu_item except for empty ones"""
    items = list(bf_namelist.menu_item for bf_namelist in bf_namelists \
        if bf_namelist.bpy_type == bpy_type and bf_namelist.menu_item)
    items.sort()
    return items

bf_props["Ob Namelist"].bpy_other["items"] = get_bf_namelist_items(bpy.types.Object)
bf_props["Ob Namelist"].bpy_other["default"] = "OBST"

bf_props["Ma Namelist"].bpy_other["items"] = get_bf_namelist_items(bpy.types.Material)
bf_props["Ma Namelist"].bpy_other["default"] = "SURF"

### Some sanity checks

# Check if unused bf_props
bf_props_unused = set(bf_props) \
    - set(bf_prop for bf_namelist in bf_namelists for bf_prop in bf_namelist.bf_props) \
    - set(bf_prop_sub for bf_prop in bf_props for bf_prop_sub in bf_prop.bf_props) \
    - set(bf_prop.bf_prop_export for bf_prop in bf_props if bf_prop.bf_prop_export is not None) \
    - set(bf_namelist.bf_prop_export for bf_namelist in bf_namelists if bf_namelist.bf_prop_export is not None)
if bf_props_unused: raise Exception("Unused bf_props: {}".format(bf_props_unused))

# Check if unused bf_namelists
bf_namelists_unused = set(bf_namelists) \
    - set(bf_namelist for bf_section in bf_sections for bf_namelist in bf_section.bf_namelists) \
    - set((bf_namelists["Case Configuration"],))
if bf_namelists_unused: raise Exception("Unused bf_namelists: {}".format(bf_namelists_unused))
