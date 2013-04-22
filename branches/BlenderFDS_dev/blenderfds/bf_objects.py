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
from . import bf_operators, bf_geometry, bf_config

### Mods

class BFPropNoExport(BFProp):
    def is_exported(self, context, element):
        return False

class BFNamelistNoExport(BFNamelist):
    def is_exported(self, context, element):
        return False

class BFPropCustom(BFProp):
    def check(self, value):
        err = BFError(self)
        if isinstance(value, str):
            if '&' in value or '/' in value:
                err.msgs.append("& and / characters not needed")
            if "`" in value or "‘" in value or "’‌" in value:
                err.msgs.append("Typographic quotes not allowed, use matched single quotes")
            if '"' in value or "”" in value:
                err.msgs.append("Double quotes not allowed, use matched single quotes")
            if value.count("'") % 2 != 0:
                err.msgs.append("Unmatched single quotes")
        if err.msgs: raise err

    def draw_bf_props(self, context, element, layout):
        # Draw self
        row = layout.row()
        row.prop(element, self.bpy_name, text="", icon="TEXT")
        # Draw related bf_props
        for bf_prop in self.bf_props: bf_prop.draw(context, element, layout)

class BFPropFilename(BFProp):
    def check(self, value):
        if bpy.path.clean_name(value) != value: raise BFError(self,"Illegal characters in filename")

class BFPropPathExists(BFPropNoExport):
    def check(self, value):
        if not os.path.exists(bpy.path.abspath(value)): raise BFError(self,"Path does not exist")

class BFPropNoUI(BFPropNoExport):
    def draw(self, context, element, layout):
        pass

# FIXME DEVC_ID cannot be cleanly supported.
# Blender does not currently have a nice way to choose a referred object
# from a filtered list of objects

### Default BFNamelist and BFProp
# FIXME update each time?
# Or update at the end of this file?

BFProp(
    name = "Ob namelist",
    label = "Namelist",
    description = "Type of FDS namelist",
    bpy_name = "bf_namelist",
    bpy_prop = bpy.props.EnumProperty,
    default = "OBST",
    items = ("OBST","OBST","OBST",0),
)

BFProp(
    name = "Ma namelist",
    label = "Namelist",
    description = "Type of FDS namelist",
    bpy_name = "bf_namelist",
    bpy_prop = bpy.props.EnumProperty,
    default = "SURF",
    items = ("SURF","SURF","SURF",0),
)

class BFNamelistTMP(BFNamelist):
    
    menu_item = None # No bf_namelist menu item
    
    def draw_header(self,context,element,layout):
        return "BlenderFDS Temporary Object"
    
    def draw(self,context,element,layout):
        row = layout.row()
        row.prop(context.object,"bf_namelist")
        row.operator("object.bf_hide_voxels")
    
BFNamelistTMP(
    name = "TMP",
    label = "TMP",
    description = "Temporary object",
    unique_id = 18,
    bpy_type = bpy.types.Object,
)

### SURF_ID

class BFPropSURFID(BFProp):
    def is_exported(self,context,element):
        return element.active_material and element.active_material.bf_namelist_export or False

    def value(self,context,element):
        if element.active_material: return element.active_material.name
        
    def draw_bf_props(self,context,element,layout):
        row = layout.row()
        row.active = self.is_exported(context, element)
        row.prop_search(element, self.bpy_name, bpy.data, "materials", text=self.label)

BFPropSURFID(
    name = "SURF_ID",
    label = "SURF_ID",
    description = "Reference to SURF",
    fds_name = "SURF_ID",
    bpy_name = "active_material",
)

### XB

class BFPropXB(BFProp):
    def value(self, context, element):
        value = element.bf_xb
        if value == "VOXELS":
            xbs = bf_geometry.get_bbox(context, element)[0]
            (bbminx, bbmaxx, bbminy, bbmaxy, bbminz, bbmaxz,) = xbs[0]
            dimension = max(bbmaxx-bbminx, bbmaxy-bbminy, bbmaxz-bbminz)
            dimension_too_large = bf_geometry.calc_remesh(context, dimension, element.bf_voxel_size)[3]
            if dimension_too_large: raise BFError(self, "Object too large, voxel size not guaranteed")
        return value

    def draw_extra(self, context, element, layout):
        if element.bf_xb == "VOXELS":
            row = layout.row()
            bf_prop = self.bf_props["Voxel size"]
            row.prop(element, bf_prop.bpy_name, text=bf_prop.label)
            if element.bf_has_voxels_shown: row.operator("object.bf_hide_voxels") 
            else: row.operator("object.bf_show_voxels")

    def format(self, value):
        return "XB={0[0]:.3f},{0[1]:.3f},{0[2]:.3f},{0[3]:.3f},{0[4]:.3f},{0[5]:.3f}".format(value)
    
    def to_fds(self, context, element):
        # Check and init
        if not self.is_exported(context, element): return None
        res = self.evaluate(context, element)
        # Select case
        if res.value == "BBOX":
            xbs, tt = bf_geometry.get_bbox(context, element)
        elif res.value == "VOXELS":
            xbs, tt, dimension_too_large = bf_geometry.get_voxels(context, element)
            res.msgs.append("{0} voxels of size {1:.3f} m, in {2:.3f} s".format(len(xbs),element.bf_voxel_size,tt))
        elif res.value == "FACES":
            xbs, tt = bf_geometry.get_faces(context, element)
            res.msgs.append("{0} faces, in {1:.3f} s".format(len(xbs), tt))
        elif res.value == "EDGES":
            xbs, tt = bf_geometry.get_edges(context, element)
            res.msgs.append("{0} edges, in {1:.3f} s".format(len(xbs), tt))
        else: raise Exception("XB type unknown")
        res.value = tuple((self.format(xb) for xb in xbs))
        return res

BFPropNoUI(
    name = "Voxel size",
    label = "Voxel Size [m]",
    description = "Minimum resolution for object voxelization",
    bpy_name = "bf_voxel_size",
    bpy_prop = bpy.props.FloatProperty,
    step = 1,
    precision = 3,
    min = .01,
    max = 2.,
    default = .10,
    update = bf_operators.update_voxels,
)

BFPropNoUI(
    name = "Is voxels",
    label = "Is a Voxel Object",
    description = "This is a voxel temporary object",
    bpy_name = "bf_is_voxels",
    bpy_prop = bpy.props.BoolProperty,
    default = False,
)

BFPropNoUI(
    name = "Has voxels shown",
    label = "Has Voxel Shown",
    description = "This object has a visible voxel object companion",
    bpy_name = "bf_has_voxels_shown",
    bpy_prop = bpy.props.BoolProperty,
    default = False,
)

BFPropXB(
    name = "XB",
    label = "XB",
    description = "XB",
    fds_name = "XB",
    has_export_flag = True,
    bf_props = ("Voxel size","Is voxels","Has voxels shown"),
    bpy_name = "bf_xb",
    bpy_prop = bpy.props.EnumProperty,
    items = (
        ("VOXELS", "Voxelized Solid", "Export voxels from voxelized solid"),
        ("BBOX", "Bounding Box", "Use object bounding box"),
        ("FACES", "Faces", "Faces, one for each face of this object"),
        ("EDGES", "Edges", "Segments, one for each edge of this object"),
        ),
    default = "BBOX",
)

class BFPropXBBBox(BFPropXB):
    def value(self, context, element):
        return "BBOX"

    def draw_bf_props(self, context, element, layout):
        row = layout.row()
        row.label(text=self.label)

BFPropXBBBox(
    name = "XB Bounding Box",
    label = "XB:  Bounding Box",
    description = "XB",
    fds_name = "XB",
    bpy_name = "bf_xb",
)

### XYZ

class BFPropXYZ(BFProp):
    def value(self, context, element):
        bf_xb, bf_xyz = None, None
        if element.has_bf_prop("XB") and element.bf_xb_export: bf_xb = element.bf_xb
        if element.has_bf_prop("XYZ") and element.bf_xyz_export: bf_xyz = element.bf_xyz
        if bf_xyz == "VERTICES" and bf_xb in ("VOXELS", "FACES", "EDGES"): raise BFError(self, "Conflicting with XB")
        return bf_xyz

    def format(self, value):
        return "XYZ={0[0]:.3f},{0[1]:.3f},{0[2]:.3f}".format(value)
        
    def to_fds(self, context, element):
        # Check and init
        if not self.is_exported(context, element): return None
        res = self.evaluate(context, element)
        # Select case
        if res.value == "CENTER":
            xyzs, tt = bf_geometry.get_center(context, element)
        elif res.value == "VERTICES":
            xyzs, tt = bf_geometry.get_vertices(context, element)
            res.msgs.append("{0} vertices, in {1:.3f} s".format(len(xyzs), tt))
        else: raise Exception("XYZ type unknown")
        res.value = tuple((self.format(xyz) for xyz in xyzs))
        return res

BFPropXYZ(
    name = "XYZ",
    label = "XYZ",
    description = "Set points",
    fds_name = "XYZ",
    has_export_flag = True,
    bpy_name = "bf_xyz",
    bpy_prop = bpy.props.EnumProperty,
    items = [
        ("CENTER","Center","Point, corresponding to the center point of this object"),
        ("VERTICES","Vertices","Points, one for each vertex of this object"),
        ],
    default = "CENTER",
)

### PB
# FIXME what if no faces?

class BFPropPB(BFProp):
    def value(self,context,element):
        err = BFError(self)
        bf_xb, bf_xyz, bf_pb = None, None, None
        if element.has_bf_prop("XB") and element.bf_xb_export: bf_xb = element.bf_xb
        if element.has_bf_prop("XYZ") and element.bf_xyz_export: bf_xyz = element.bf_xyz
        if element.bf_pb_export: bf_pb = element.bf_pb
        if bf_pb == "PLANES" and bf_xb in ("VOXELS", "FACES", "EDGES"): err.msgs.append("Conflicting with XB")
        if bf_pb == "PLANES" and bf_xyz == "VERTICES": err.msgs.append("Conflicting with XYZ")
        # Errors?
        if err.msgs: raise err
        return bf_pb

    def format(self,value):
        return "PB{0[0]}={0[1]:.3f}".format(value)
    
    def to_fds(self,context,element):
        # Check and init
        if not self.is_exported(context, element): return None
        res = self.evaluate(context, element)
        # Select case
        if res.value == "PLANES":
            pbs, tt = bf_geometry.get_planes(context, element)
            res.msgs.append("{0} planes, in {1:.3f} s".format(len(pbs), tt))
        else: raise Exception("PB type unknown")
        res.value = tuple((self.format(pb) for pb in pbs))
        return res

BFPropPB(
    name = "PB",
    label = "PB*",
    description = "Set planes",
    fds_name = "PB",
    has_export_flag = True,
    bpy_name = "bf_pb",
    bpy_prop = bpy.props.EnumProperty,
    items = [("PLANES", "Planes", "Planes, one for each face of this object"),],
    default = "PLANES",
)

### Custom

BFPropCustom(
    name = "Custom",
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
    name = "Namelist Custom",
    label = "Custom Namelist:",
    description = "Free text namelist, & and / not needed, use single quotes, eg OBST PROP1='example'",
    bpy_name = "bf_custom_namelist",
    bpy_prop = bpy.props.StringProperty,
    maxlen = 1024,
    default = "OBST ID='Example'"
)
 
### Namelist export BFProp

BFProp(
    name = "Namelist export",
    label = "Export",
    description = "Export namelist to FDS",
    bpy_name = "bf_namelist_export",
    bpy_prop = bpy.props.BoolProperty,
    default = False,
)

### BFProp

BFProp(
    name = "ID",
    label = "ID",
    description = "Element identificator",
    fds_name = "ID",
    bpy_name = "name",
)

BFPropNoExport(
    name = "ID no export",
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

BFPropPathExists(
    name = "Case Directory",
    label = "Case Directory",
    description = "Case directory",
    bpy_name = "bf_case_directory",
    bpy_prop = bpy.props.StringProperty,
    subtype = "DIR_PATH",
    maxlen = 1024,
)

BFPropPathExists(
    name = "External Config File",
    label = "Ext Config File",
    description = "Path to external configuration file",
    has_export_flag = True,
    bpy_name = "bf_ext_config_filepath",
    bpy_prop = bpy.props.StringProperty,
    default = "//config.fds",
    subtype = "FILE_PATH",
    maxlen = 1024,
)

BFPropNoExport(
    name = "Version",
    label = "Version",
    description = "BlenderFDS version used for file creation and exporting",
    bpy_name = "bf_version",
    bpy_prop = bpy.props.IntVectorProperty,
    default = (0,0,0),
    size = 3,
)
# FIXME bl_info["version"]

class BFPropTBEGIN(BFProp):
    def is_exported(self, context, element):
        # No T_BEGIN if SMV setup
        if element.bf_time_smv_setup: return False
        return True

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
    def value(self, context, element):
        # T_END=0 if SMV setup
        if element.bf_time_smv_setup: value = 0.
        else: value = getattr(element,self.bpy_name)
        self.check(value)
        return value

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

BFPropNoExport(
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
    def msgs(self, context, element):
        return "Output dumps every {:.2f} s".format((element.bf_time_t_end - element.bf_time_t_begin) / element.bf_dump_nframes)

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
        msgs = list()
        operators = list()
        # Good IJK
        good_ijk = bf_geometry.get_good_ijk(element)
        if tuple(element.bf_mesh_ijk) != good_ijk:
            msgs.append("J and K not optimal for Poisson solver")
            operators.append("object.bf_correct_ijk")
        # Cells
        cell_size = list(bf_geometry.get_cell_size(element))
        cell_number = bf_geometry.get_cell_number(element)
        msgs.append("{0} mesh cells of size {1[0]:.3f} m x {1[1]:.3f} m x {1[2]:.3f} m".format(cell_number, cell_size))
        # Aspect ratio
        cell_size.sort()
        max_ratio = max(cell_size[2] / cell_size[0], cell_size[2] / cell_size[1], cell_size[1] / cell_size[0])
        if max_ratio > 2.: msgs.append("Max cell aspect ratio is {:.1f}".format(max_ratio))      
        return BFResult(sender=self, value=self.value(context,element), msgs=msgs, operators=operators)

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
    def value(self,context,element):
        value = element.diffuse_color
        return int(value[0]*255), int(value[1]*255), int(value[2]*255)

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

BFProp(
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

# TODO check variable name: bf_surf_hrrpua

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
    unique_id = 1,
    bpy_type = bpy.types.Scene,
    bf_props = ("CHID","TITLE"),
)

BFNamelistNoExport(
    name = "Case Configuration",
    label = "Case Configuration",
    description = None,
    unique_id = 2,
    bpy_type = bpy.types.Scene,
    bf_props = ("Case Directory","External Config File","Version"),
)

class BFNamelistTIME(BFNamelist):
    def evaluate(self, context, element):
        bf_time_t_begin = element.bf_time_t_begin
        bf_time_t_end = element.bf_time_t_end
        duration = bf_time_t_end - bf_time_t_begin
        msgs = None
        if bf_time_t_begin > bf_time_t_end: raise BFError(self, "T_END < T_BEGIN")
        elif duration > 0. and bf_time_t_begin > 0. and not element.bf_time_smv_setup: msgs = "Simulation duration is {} s".format(duration) 
        elif duration == 0. or element.bf_time_smv_setup: msgs = "Smokeview setup only"
        return BFResult(sender=self, value=None, msgs=msgs)

    def to_fds(self,context,element):
        print("BlenderFDS: > > > BFNamelist.to_fds: {}".format(self.name))
        # Check
        if not self.is_exported(context,element): return None
        # Export
        res = self.evaluate(context,element) # Do not trap exceptions, pass them upward
        children_values, children_msgs = self.to_fds_children(children=self.bf_props, context=context, element=element)
        # res.value from self.evaluate() is used then replaced with new content
        res.value = self.format(context, element, children_values, children_msgs)
        return res

    def draw_bf_props(self, context, element, layout):
        """Draw self bf_props for element in the layout."""
        bf_time_smv_setup = element.bf_time_smv_setup
        for b_prop in self.bf_props[0:2]: # T_BEGIN and T_END non active if SMV setup only
            row = layout.row()
            row.active = not bf_time_smv_setup
            row.prop(element, b_prop.bpy_name, text=b_prop.label)
        self.bf_props["SMV setup"].draw(context, element, layout) # Draw SMV setup

BFNamelistTIME(
    name = "TIME",
    label = "TIME",
    description = "Time",
    fds_name = "TIME",
    unique_id = 3,
    bpy_type = bpy.types.Scene,
    bf_props = ("T_BEGIN", "T_END", "SMV setup", "TIME Custom"),
)

BFNamelist(
    name = "MISC",
    label = "MISC",
    description = "Miscellaneous parameters",
    fds_name = "MISC",
    unique_id = 4,
    has_export_flag = True,
    bpy_type = bpy.types.Scene,
    bf_props = ("MISC Custom",),
)

BFNamelist(
    name = "REAC",
    label = "REAC",
    description = "Reaction",
    fds_name = "REAC",
    unique_id = 5,
    has_export_flag = True,
    bpy_type = bpy.types.Scene,
    bf_props = ("FUEL","FORMULA","CO_YIELD","SOOT_YIELD","HEAT_OF_COMBUSTION","REAC Custom"),
)

BFNamelist(
    name = "DUMP",
    label = "DUMP",
    description = "Output parameters",
    fds_name = "DUMP",
    unique_id = 6,
    has_export_flag = True,
    bpy_type = bpy.types.Scene,
    bf_props = ("NFRAMES","DUMP Custom"),
)

BFNamelist(
    name = "Custom",
    label = "Custom",
    description = None,
    unique_id = 7,
    has_export_flag = True,
    bf_prop_export = "Namelist export",
    bpy_type = bpy.types.Object,
    bf_props = ("Ob namelist","Namelist Custom","FYI","XB","XYZ","PB"),
)

BFNamelist(
    name = "OBST",
    label = "OBST",
    description = "Obstruction",
    fds_name = "OBST",
    unique_id = 0,
    has_export_flag = True,
    bf_prop_export = "Namelist export",
    bpy_type = bpy.types.Object,
    bf_props = ("Ob namelist","ID","FYI","SURF_ID","XB","Custom",),
)

BFNamelist(
    name = "HOLE",
    label = "HOLE",
    description = "Obstruction Cutout",
    fds_name = "HOLE",
    unique_id = 9,
    has_export_flag = True,
    bf_prop_export = "Namelist export",
    bpy_type = bpy.types.Object,
    bf_props = ("Ob namelist","ID no export","FYI","XB","Custom",),
)

BFNamelist(
    name = "VENT",
    label = "VENT",
    description = "Boundary Condition Patch",
    fds_name = "VENT",
    unique_id = 10,
    has_export_flag = True,
    bf_prop_export = "Namelist export",
    bpy_type = bpy.types.Object,
    bf_props = ("Ob namelist", "ID", "FYI", "SURF_ID", "XB", "XYZ", "PB",  "Custom",),
)

BFNamelist(
    name = "DEVC",
    label = "DEVC",
    description = "Device",
    fds_name = "DEVC",
    unique_id = 11,
    has_export_flag = True,
    bf_prop_export = "Namelist export",
    bpy_type = bpy.types.Object,
    bf_props = ("Ob namelist", "ID", "FYI", "QUANTITY", "SETPOINT", "INITIAL_STATE", "LATCH", "XB", "XYZ", "PROP_ID",  "Custom",),
)

BFNamelist(
    name = "SLCF",
    label = "SLCF",
    description = "Slice File",
    fds_name = "SLCF",
    unique_id = 12,
    has_export_flag = True,
    bf_prop_export = "Namelist export",
    bpy_type = bpy.types.Object,
    bf_props = ("Ob namelist","ID no export","FYI","QUANTITY","VECTOR","XB","PB","Custom",),
)

# FIXME IOR
BFNamelist(
    name = "PROF",
    label = "PROF",
    description = "Wall Profile Output",
    fds_name = "PROF",
    unique_id = 13,
    has_export_flag = True,
    bf_prop_export = "Namelist export",
    bpy_type = bpy.types.Object,
    bf_props = ("Ob namelist","ID","FYI","QUANTITY","XYZ","Custom",),
)

BFNamelist(
    name = "MESH",
    label = "MESH",
    description = "Domain of simulation",
    fds_name = "MESH",
    unique_id = 14,
    has_export_flag = True,
    bf_prop_export = "Namelist export",
    bpy_type = bpy.types.Object,
    bf_props = ("Ob namelist","ID","FYI","IJK","XB Bounding Box","Custom",),
)

BFNamelist(
    name = "INIT",
    label = "INIT",
    description = "Initial condition",
    fds_name = "INIT",
    unique_id = 15,
    has_export_flag = True,
    bf_prop_export = "Namelist export",
    bpy_type = bpy.types.Object,
    bf_props = ("Ob namelist","ID","FYI","XB","XYZ","Custom",),
)

BFNamelist(
    name = "ZONE",
    label = "ZONE",
    description = "Pressure zone",
    fds_name = "ZONE",
    unique_id = 16,
    has_export_flag = True,
    bf_prop_export = "Namelist export",
    bpy_type = bpy.types.Object,
    bf_props = ("Ob namelist","ID","FYI","XB Bounding Box","Custom",),
)

class BFNamelistSURF(BFNamelist):
    def evaluate(self, context, element):
        if not set(bf_config.predefined_material_names) <= set(bpy.data.materials.keys()):
            raise BFError(sender=self, msgs="Predefined SURFs unset", operators="material.bf_set_predefined")
        return BFResult(sender=self, value=None, msgs=None)

BFNamelistSURF(
    name = "SURF",
    label = "SURF",
    description = "Boundary Condition",
    fds_name = "SURF",
    unique_id = 0,
    has_export_flag = True,
    bf_prop_export = "Namelist export",
    bpy_type = bpy.types.Material,
    bf_props = ("Ma namelist","ID","FYI","RGB","TRANSPARENCY","Custom",),
)

class BFNamelistSURFBurner(BFNamelist):
    def evaluate(self, context, element):
        msgs = element.bf_surf_tau_q <= 0 and "HRR(t) has a t² ramp" or "HRR(t) has a tanh(t/τ) ramp"
        return BFResult(sender=self, value=None, msgs=msgs, operators="material.set_tau_q")

BFNamelistSURFBurner(
    name = "SURF burner",
    label = "SURF burner",
    description = "A simple burner",
    fds_name = "SURF",
    unique_id = 1,
    has_export_flag = True,
    bf_prop_export = "Namelist export",
    bpy_type = bpy.types.Material,
    bf_props = ("Ma namelist","ID","FYI","RGB","HRRPUA","TAU_Q","Custom",),
)

## BFSections

BFSection(
    name = "General configuration",
    bf_namelists = ("HEAD","TIME","MISC","REAC","DUMP"),
)

class BFSection_External_Config(BFSection):
    def to_fds(self,context,element):
        print("BlenderFDS: > BFSection.to_fds: {}".format(self.name))
        bf_prop = bf_props["External Config File"]
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
        res.value = self.format(res.value,res.labels)
        return res

BFSection_External_Config(
    name = "External configuration",
)

BFSection(
    name = "Boundary conditions",
    bf_namelists = ("SURF","SURF burner"),
)

BFSection(
    name = "Computational domain",
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
    name = "Control logic and output",
    bf_namelists = ("DEVC", "SLCF", "PROF"),
)

BFSection(
    name = "Other",
    bf_namelists = ("Custom"),
)

### Update bf_namelist menu with all namelists

def get_bf_namelist_items(bpy_type):
    items = list(bf_namelist.menu_item for bf_namelist in bf_namelists \
        if bf_namelist.bpy_type == bpy_type and bf_namelist.menu_item)
    print(items)
    items.sort()
    return items

bf_props["Ob namelist"].bpy_other["items"] = get_bf_namelist_items(bpy.types.Object)
bf_props["Ma namelist"].bpy_other["items"] = get_bf_namelist_items(bpy.types.Material)

### Some sanity checks
# Check unused defined objects
bf_props_unused = set(bf_props) - set(b_prop for bf_namelist in bf_namelists for b_prop in bf_namelist.bf_props) \
    - set(bf_prop.bf_prop_export for bf_prop in bf_props if bf_prop.bf_prop_export is not None) \
    - set(bf_namelist.bf_prop_export for bf_namelist in bf_namelists if bf_namelist.bf_prop_export is not None)
print("BlenderFDS: Unused bf_props:",bf_props_unused)

bf_namelists_unused = set(bf_namelists) - set(bf_namelist for bf_section in bf_sections for bf_namelist in bf_section.bf_namelists)
print("BlenderFDS: Unused bf_namelists:",bf_namelists_unused)

