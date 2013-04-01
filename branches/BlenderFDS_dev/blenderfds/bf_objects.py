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
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software Foundation,
#  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# ##### END GPL LICENSE BLOCK #####
"""BlenderFDS, an open tool for the NIST Fire Dynamics Simulator"""

from .bf_types import *
from . import bf_operators

from bpy.props import StringProperty, EnumProperty, BoolProperty, IntProperty, IntVectorProperty, FloatProperty, FloatVectorProperty
from bpy.types import Scene, Object, Material
import bpy
import os



### Default BFNamelist and BFParam

BProp(
    name = "bf_namelist",
    label = "Namelist",
    description = "Type of FDS namelist",
    bpy_prop = StringProperty,
)

BFParam(
    name = "Namelist",
    b_props = "bf_namelist",
)

class BFNamelist_TMP(BFNamelist):
    def draw_header(self,context,element,layout):
        return "BlenderFDS Temporary Object"
    def draw(self,context,element,layout):
        row = layout.row()
        row.operator("object.bf_hide_voxels")

BFNamelist_TMP(
    name = "TMP",
    description = "Temporary object",
    bpy_type = Object,
)

### Generic BFParam

BProp(
    name = "bf_id",
    b_name = "name",
    label = "ID",
    description = "Element identificator",
)

BFParam(
    name = "ID",
    f_name = "ID",
    b_props = "bf_id",
)

BFParam(
    name = "ID_no_export",
    b_props = "bf_id",
)

BProp(
    name = "bf_fyi",
    label = "FYI",
    description = "Free text description",
    bpy_prop = StringProperty,
    other = {"maxlen":60},
)

BFParam(
    name = "FYI",
    f_name = "FYI",
    b_props = "bf_fyi",
)

class BFParam_Custom(BFParam):
    def _check(self,value):
        if isinstance(value,str):
            if "`" in value or "‘" in value or "’‌" in value:
                raise BFError(self,"Typographic quotes not allowed, use matched single quotes")
            elif '"' in value or "”" in value:
                raise BFError(self,"Double quotes not allowed, use matched single quotes")
            elif value.count("'") % 2 != 0:
                raise BFError(self,"Unmatched single quotes")
    def _draw_b_props(self,context,element,layout):
        col = layout.column()
        col.label(text="Custom Parameters:")
        col.prop(element,self.b_props[0].b_name,text="")

BProp(
    name = "bf_custom",
    label = "Custom parameters:",
    description = "Custom parameters are appended verbatim to namelist parameters. Use matched single quotes.",
    bpy_prop = StringProperty,
    other = {"maxlen":1024},
)

BFParam_Custom(
    name = "Custom",
    b_props = "bf_custom",
)

### HEAD

class BFParam_filename(BFParam):
    def _check(self,value):
        if bpy.path.clean_name(value) != value: raise BFError(self,"Illegal characters in filename")

BProp(
    name = "bf_chid",
    b_name = "name",
    label = "CHID",
    description = "Case identificator",
)

BFParam_filename(
    name = "CHID",
    f_name = "CHID",
    b_props = "bf_chid",
)

BProp(
    name = "bf_head_title",
    label = "TITLE",
    description = "Case description",
    bpy_prop = StringProperty,
    other = {"maxlen":64},
)

BFParam(
    name = "TITLE",
    f_name = "TITLE",
    b_props = "bf_head_title",
)

class BFParam_path_exists(BFParam):
    def _check(self,value):
        if not os.path.exists(bpy.path.abspath(value)): raise BFError(self,"Path does not exists")

BProp(
    name = "bf_case_directory",
    label = "Case Directory",
    description = "Case directory",
    bpy_prop = StringProperty,
    other = {"subtype":"DIR_PATH","maxlen":1024},
)

BFParam_path_exists(
    name = "Case Directory",
    b_props = "bf_case_directory",
)

BProp(
    name = "bf_config_filepath",
    label = "Config File",
    description = "Path to external configuration file",
    bpy_prop = StringProperty,
    other = {"default":"config.fds","subtype":"FILE_PATH","maxlen":1024},
)

BFParam_path_exists(
    name = "Config File",
    b_props = "bf_config_filepath",
    b_default_export = True,
)

BProp(
    name = "bf_version",
    label = "Version",
    description = "BlenderFDS version used for file creation and exporting",
    bpy_prop = IntVectorProperty,
    other = {"default":(0,0,0),"size":3},
)

BFParam(
    name = "Version",
    b_props = "bf_version",
)
# FIXME bl_info["version"]

BFNamelist(
    name = "HEAD",
    description = "Header and Case Config",
    f_name = "HEAD",
    bpy_type = Scene,
    bf_params = ("CHID","TITLE","Case Directory","Config File","Version"),
)

### TIME

BProp(
    name = "bf_time_t_begin",
    label = "T_BEGIN",
    description = "Simulation starting time",
    bpy_prop = FloatProperty,
    other = {"step":100.,"precision":1,"min":0.,"default":0.},
)

BFParam(
    name = "T_BEGIN",
    f_name = "T_BEGIN",
    b_props = "bf_time_t_begin",
)

BProp(
    name = "bf_time_t_end",
    label = "T_END",
    description = "Simulation ending time",
    bpy_prop = FloatProperty,
    other = {"step":100.,"precision":1,"min":0.,"default":0.},
)

BFParam(
    name = "T_END",
    f_name = "T_END",
    b_props = "bf_time_t_end",
)

BProp(
    name = "bf_time_custom",
    label = "Custom",
    description = "Custom parameters are appended verbatim to namelist parameters. Use matched single quotes.",
    bpy_prop = StringProperty,
    other = {"maxlen":1024},
)

BFParam_Custom(
    name = "TIME_Custom",
    b_props = "bf_time_custom",
)

BFNamelist(
    name = "TIME",
    description = "Time",
    f_name = "TIME",
    bpy_type = Scene,
    bf_params = ("T_BEGIN","T_END","TIME_Custom"),
    b_default_export = True,
)

### MISC

# FIXME TMPA, VISIBILITY_FACTOR, EVAC params

BProp(
    name = "bf_misc_custom",
    label = "Custom",
    description = "Custom parameters are appended verbatim to namelist parameters. Use matched single quotes.",
    bpy_prop = StringProperty,
    other = {"maxlen":1024},
)

BFParam_Custom(
    name = "MISC_Custom",
    b_props = "bf_misc_custom",
)

BFNamelist(
    name = "MISC",
    description = "Miscellaneous parameters",
    f_name = "MISC",
    bpy_type = Scene,
    bf_params = ("MISC_Custom",),
    b_default_export = True,
)

### REAC

BProp(
    name = "bf_reac_fuel",
    label = "FUEL",
    description = "Identificator of fuel species for the reaction",
    bpy_prop = StringProperty,
    other = {"maxlen":32},
)

BFParam(
    name = "FUEL",
    f_name = "FUEL",
    b_props = "bf_reac_fuel",
)

BProp(
    name = "bf_reac_formula",
    label = "FORMULA",
    description = "Chemical formula of the fuel species for the reaction, it can only contain C, H, O, or N",
    bpy_prop = StringProperty,
    other = {"maxlen":32},
)

BFParam(
    name = "FORMULA",
    f_name = "FORMULA",
    b_props = "bf_reac_formula",
    b_default_export = True,
)

BProp(
    name = "bf_reac_co_yield",
    label = "CO_YIELD",
    description = "Fraction of fuel mass converted into carbon monoxide",
    bpy_prop = FloatProperty,
    other = {"step":1.,"precision":2,"min":0.,"max":1.,"default":0.},
)

BFParam(
    name = "CO_YIELD",
    f_name = "CO_YIELD",
    b_props = "bf_reac_co_yield",
    b_default_export = True,
)

BProp(
    name = "bf_reac_soot_yield",
    label = "SOOT_YIELD",
    description = "Fraction of fuel mass converted into smoke particulate",
    bpy_prop = FloatProperty,
    other = {"step":1.,"precision":2,"min":0.,"max":1.,"default":0.},
)

BFParam(
    name = "SOOT_YIELD",
    f_name = "SOOT_YIELD",
    b_props = "bf_reac_soot_yield",
    b_default_export = True,
)

BProp(
    name = "bf_reac_heat_of_combustion",
    label = "HEAT_OF_COMBUSTION",
    description = "Fuel heat of combustion",
    bpy_prop = FloatProperty,
    other = {"step":100000.,"precision":1,"min":0.,"default":0.},
)

BFParam(
    name = "HEAT_OF_COMBUSTION",
    f_name = "HEAT_OF_COMBUSTION",
    b_props = "bf_reac_heat_of_combustion",
    b_default_export = True,
)

BProp(
    name = "bf_reac_custom",
    label = "Custom",
    description = "Custom parameters are appended verbatim to namelist parameters. Use matched single quotes.",
    bpy_prop = StringProperty,
    other = {"maxlen":1024},
)

BFParam_Custom(
    name = "REAC_Custom",
    b_props = "bf_reac_custom",
)

BFNamelist(
    name = "REAC",
    description = "Reaction",
    f_name = "REAC",
    bpy_type = Scene,
    bf_params = ("FUEL","FORMULA","CO_YIELD","SOOT_YIELD","HEAT_OF_COMBUSTION","REAC_Custom"),
    b_default_export = True,
)

### DUMP

# FIXME show DT

BProp(
    name = "bf_dump_nframes",
    label = "NFRAMES",
    description = "Number of output dumps per calculation",
    bpy_prop = IntProperty,
    other = {"min":1,"default":1000},
)

BFParam(
    name = "NFRAMES",
    f_name = "NFRAMES",
    b_props = "bf_dump_nframes",
    b_default_export = True,
)

BProp(
    name = "bf_dump_custom",
    label = "Custom",
    description = "Custom parameters are appended verbatim to namelist parameters. Use matched single quotes.",
    bpy_prop = StringProperty,
    other = {"maxlen":1024},
)

BFParam_Custom(
    name = "DUMP_Custom",
    b_props = "bf_dump_custom",
)

BFNamelist(
    name = "DUMP",
    description = "Output parameters",
    f_name = "DUMP",
    bpy_type = Scene,
    bf_params = ("NFRAMES","DUMP_Custom"),
    b_default_export = True,
)

### Object

class BFParam_XB(BFParam):

    def _value(self,context,element):
        value = element.bf_xb
        if value == "VOXELS":
            xbs = get_bbox(context,element)[0]
            (bbminx, bbmaxx, bbminy, bbmaxy, bbminz, bbmaxz,) = xbs[0]
            dimension = max(bbmaxx-bbminx, bbmaxy-bbminy, bbmaxz-bbminz)
            dimension_too_large = calc_remesh(context, dimension, element.bf_voxel_size)[3]
            if dimension_too_large: raise BFError(self,"Object too large, voxel size not guaranteed")
        return value

    def _draw_post(self,context,element,layout):
        if element.bf_xb == "VOXELS":
            row = layout.row()
            b_prop = self.b_props["bf_voxel_size"]
            row.prop(element,b_prop.b_name,text=b_prop.label)
            if element.bf_has_voxels_shown: row.operator("object.bf_hide_voxels") 
            else: row.operator("object.bf_show_voxels")

# FIXME could use _format?
    def to_fds(self,context,element):
        # Check and init
        if not self._is_exported(context,element): return None
        res = self._evaluate(context,element)
        # Select case
        if res.value == "BBOX":
            xbs, tt = get_bbox(context,element)
        elif res.value == "VOXELS":
            xbs, tt, dimension_too_large = get_voxels(context,element)
            res.msgs.append("{0} voxels of size {1:.3f}, in {2:.3f} s".format(len(xbs),element.bf_voxel_size,tt))
        elif res.value == "FACES":
            xbs, tt = get_faces(context,element)
            res.msgs.append("{0} faces, in {1:.3f} s".format(len(xbs),tt))
        elif res.value == "EDGES":
            xbs, tt = get_edges(context,element)
            res.msgs.append("{0} edges, in {1:.3f} s".format(len(xbs),tt))
        else: raise Exception("XB type unknown")
        res.value = tuple((format_xb(xb) for xb in xbs))
        return res

items = (
    ("VOXELS","Voxelized Solid","Export voxels from voxelized solid"),
    ("BBOX","Bounding Box","Use object bounding box"),
    ("FACES","Faces","Faces, one for each face of this object"),
    ("EDGES","Edges","Segments, one for each edge of this object"),
    )

BProp(
    name = "bf_xb",
    label = "XB",
    description = "XB",
    bpy_prop = EnumProperty,
    other = {"items":items,"default":"BBOX"},
)

BProp(
    name = "bf_voxel_size",
    label = "Voxel Size",
    has_ui = False,
    description = "Minimum resolution for object voxelization",
    bpy_prop = FloatProperty,
    other = {"step":1,"precision":3,"min":.01,"max":2.,"default":.10,"update":bf_operators.update_voxels},
)

BProp(
    name = "bf_is_voxels",
    label = "Is a Voxel Object",
    has_ui = False,
    description = "This is a voxel temporary object",
    bpy_prop = BoolProperty,
    other = {"default":False},
)

BProp(
    name = "bf_has_voxels_shown",
    label = "Has Voxel Shown",
    has_ui = False,
    description = "This object has a visible voxel object companion",
    bpy_prop = BoolProperty,
    other = {"default":False},
)

BFParam_XB(
    name = "XB",
    f_name = "XB",
    b_props = ("bf_xb","bf_voxel_size","bf_is_voxels","bf_has_voxels_shown"),
    b_default_export = True,
)

class BFParam_XYZ(BFParam):
    def _value(self,context,element):
        bf_xb, bf_xyz = None, None
        if element.has_bf_param("XB") and element.bf_xb_export: bf_xb = element.bf_xb
        if element.has_bf_param("XYZ") and element.bf_xyz_export: bf_xyz = element.bf_xyz
        if bf_xyz == "VERTICES" and bf_xb in ("VOXELS","FACES","EDGES"): raise BFError(self,"Conflicting with XB")
        return bf_xyz
    
    def to_fds(self,context,element):
        # Check and init
        if not self._is_exported(context,element): return None
        res = self._evaluate(context,element)
        # Select case
        if res.value == "CENTER":
            xyzs, tt = get_center(context,element)
        elif res.value == "VERTICES":
            xyzs, tt = get_vertices(context,element)
            res.msgs.append("{0} vertices, in {1:.3f} s".format(len(xyzs),tt))
        else: raise Exception("XYZ type unknown")
        res.value = tuple((format_xyz(xyz) for xyz in xyzs))
        return res

items = [
    ("CENTER","Center","Point, corresponding to the center point of this object"),
    ("VERTICES","Vertices","Points, one for each vertex of this object"),
    ]

BProp(
    name = "bf_xyz",
    label = "XYZ",
    description = "Set points",
    bpy_prop = EnumProperty,
    other = {"items":items,"default":"CENTER"},
)

BFParam_XYZ(
    name = "XYZ",
    f_name = "XYZ",
    b_props = "bf_xyz",
    b_default_export = True,
)

# FIXME what if no faces?

class BFParam_PB(BFParam):
    def _value(self,context,element):
        # Check
        err = BFError()
        bf_xb, bf_xyz, bf_pb = None, None, None
        if element.has_bf_param("XB") and element.bf_xb_export: bf_xb = element.bf_xb
        if element.has_bf_param("XYZ") and element.bf_xyz_export: bf_xyz = element.bf_xyz
        if element.bf_pb_export: bf_pb = element.bf_pb
        if bf_pb == "PLANES" and bf_xb in ("VOXELS","FACES","EDGES"): err.msgs.append("Conflicting with XB")
        if bf_pb == "PLANES" and bf_xyz == "VERTICES": err.msgs.append("Conflicting with XYZ")
        # Errors?
        if err.msgs: raise err
        return bf_pb
    def to_fds(self,context,element):
        # Check and init
        if not self.is_exported(context,element): return None
        res = self.evaluate(context,element)
        # Select case
        if res.value == "PLANES":
            pbs, tt = get_planes(context,element)
            res.msgs.append("{0} planes, in {1:.3f} s".format(len(pbs),tt))
        else: raise Exception("PB type unknown")
        res.value = tuple((format_pb(pb) for pb in pbs))
        return res

items = [("PLANES","Planes","Planes, one for each face of this object"),]

BProp(
    name = "bf_pb",
    label = "PB*",
    description = "Set planes",
    bpy_prop = EnumProperty,
    other = {"items":items,"default":"PLANES"},
),

BFParam_PB(
    name = "PB",
    f_name = "PB",
    b_props = "bf_pb",
    b_default_export = True,
)

class BFParam_DEVC_ID(BFParam):
    def _draw_b_props(self,context,element,layout):
        row = layout.row()
        row.prop_search(element,"bf_devc_id",bpy.data,"objects",text="DEVC_ID")

BProp(
    name = "bf_devc_id",
    label = "DEVC_ID",
    description = "DEVC_ID",
    bpy_prop = StringProperty,
)

BFParam_DEVC_ID(
    name = "DEVC_ID",
    f_name = "DEVC_ID",
    b_props = "bf_devc_id",
    b_default_export = True,
)

class BFParam_SURF_ID(BFParam):
    def _draw_b_props(self,context,element,layout):
        row = layout.row()
        row.prop_search(element,"active_material",bpy.data,"materials",text="SURF_ID")
    def _is_exported(self,context,element):
        return element.active_material and element.active_material.bf_namelist_export or False
    def _has_active_ui(self,context,element):
        return self.is_exported(context,element)
    def _value(self,context,element):
        if element.active_material: return element.active_material.name

BFParam_SURF_ID(
    name = "SURF_ID",
    f_name = "SURF_ID",
)

BProp(
    name = "bf_devc_quantity",
    label = "QUANTITY",
    description = "Output quantity",
    bpy_prop = StringProperty,
    other = {"maxlen":32},
)

BFParam(
    name = "QUANTITY",
    f_name = "QUANTITY",
    b_props = "bf_devc_quantity",
)

BProp(
    name = "bf_slcf_vector",
    label = "VECTOR",
    description = "Create animated vectors",
    bpy_prop = BoolProperty,
    other = {"default":False},
)

BFParam(
    name = "VECTOR",
    f_name = "VECTOR",
    b_props = "bf_slcf_vector",
    b_default_export = True,
)


BProp(
    name = "bf_mesh_ijk",
    label = "IJK",
    description = "Cell number in x, y, and z direction",
    bpy_prop = IntVectorProperty,
    other = {"default":(10,10,10),"size":3},
    operator = "object.bf_correct_ijk",
)

# FIXME check number for Poisson and inform user
class BFParam_IJK(BFParam):
    def _evaluate(self,context,element):
        value = element.bf_mesh_ijk
        msg = "{0} mesh cells of size {1[0]:.3f} x {1[1]:.3f} x {1[2]:.3f}".format(get_cell_number(element),get_cell_size(element))
        return BFResult(sender=self,value=value,msg=msg)
        
BFParam_IJK(
    name = "IJK",
    f_name = "IJK",
    b_props = "bf_mesh_ijk",
    b_default_export = True,
)

### SURF

class BFParam_RGB(BFParam):
    def _value(self,context,element):
        value = element.diffuse_color
        return int(value[0]*255),int(value[1]*255),int(value[2]*255)

BProp(
    name = "bf_rgb",
    b_name = "diffuse_color",
    label = "RGB",
    description = "RGB",
)

BFParam_RGB(
    name = "RGB",
    f_name = "RGB",
    b_props = "bf_rgb",
)

BProp(
    name = "bf_transparency",
    b_name = "alpha",
    label = "TRANSPARENCY",
    description = "Transparency",
)

BProp(
    name = "bf_transparency_export",
    b_name = "use_transparency",
    label = "Export",
    description = "Export parameter",
)

BFParam(
    name = "TRANSPARENCY",
    f_name = "TRANSPARENCY",
    b_props = "bf_transparency",
    b_prop_export = "bf_transparency_export",
)

### Object bf_namelists
BProp(
    name = "bf_namelist_export",
    label = "Export",
    description = "Export namelist to FDS",
    bpy_prop = bpy.props.BoolProperty,
    other = {"default":False},
)

BFNamelist(
    name = "Other",
    description = "Other Namelist",
    bpy_type = Object,
    bf_params = ("Namelist","FYI","Custom",),
    b_prop_export = "bf_namelist_export",
)

BFNamelist(
    name = "OBST",
    description = "Obstruction",
    f_name = "OBST",
    bpy_type = Object,
    bf_params = ("Namelist","ID","FYI","SURF_ID","XB","DEVC_ID","Custom",),
    b_prop_export = "bf_namelist_export",
)

BFNamelist(
    name = "HOLE",
    description = "Obstruction Cutout",
    f_name = "HOLE",
    bpy_type = Object,
    bf_params = ("Namelist","ID_no_export","FYI","XB","DEVC_ID","Custom",),
    b_prop_export = "bf_namelist_export",
)

BFNamelist(
    name = "VENT",
    description = "Boundary Condition Patch",
    f_name = "VENT",
    bpy_type = Object,
    bf_params = ("Namelist","ID","FYI","SURF_ID","XB","XYZ","PB","DEVC_ID","Custom",),
    b_prop_export = "bf_namelist_export",
)

# FIXME INITIAL_STATE, LATCH, PROP_ID, SETPOINT, TRIP_DIRECTION
BFNamelist(
    name = "DEVC",
    description = "Device",
    f_name = "DEVC",
    bpy_type = Object,
    bf_params = ("Namelist","ID","FYI","QUANTITY","XB","XYZ","DEVC_ID","Custom",),
    b_prop_export = "bf_namelist_export",
)

BFNamelist(
    name = "SLCF",
    description = "Slice File",
    f_name = "SLCF",
    bpy_type = Object,
    bf_params = ("Namelist","ID_no_export","FYI","QUANTITY","VECTOR","XB","PB","DEVC_ID","Custom",),
    b_prop_export = "bf_namelist_export",
)

# FIXME IOR
BFNamelist(
    name = "PROF",
    description = "Wall Profile Output",
    f_name = "PROF",
    bpy_type = Object,
    bf_params = ("Namelist","ID","FYI","QUANTITY","XYZ","Custom",),
    b_prop_export = "bf_namelist_export",
)

# FIXME RGB???
BFNamelist(
    name = "MESH",
    description = "Domain of simulation",
    f_name = "MESH",
    bpy_type = Object,
    bf_params = ("Namelist","ID","FYI","IJK","XB","Custom",),
    b_prop_export = "bf_namelist_export",
)

BFNamelist(
    name = "INIT",
    description = "Initial condition",
    f_name = "INIT",
    bpy_type = Object,
    bf_params = ("Namelist","ID","FYI","XB","XYZ","DEVC_ID","Custom",),
    b_prop_export = "bf_namelist_export",
)

BFNamelist(
    name = "ZONE",
    description = "Pressure zone",
    f_name = "ZONE",
    bpy_type = Object,
    bf_params = ("Namelist","ID","FYI","XB","Custom",),
    b_prop_export = "bf_namelist_export",
)

### Material bf_namelists

class BFNamelist_SURF(BFNamelist):
    def _check(self,value):
        if not set(bf_config.mas_predefined) <= set(bpy.data.materials.keys()):
            raise BFError(sender=self,msg="Predefined SURFs unset",operator="material.bf_set_predefined")

BFNamelist_SURF(
    name = "SURF",
    description = "Boundary Condition",
    f_name = "SURF",
    bpy_type = Material,
    bf_params = ("Namelist","ID","FYI","RGB","TRANSPARENCY","Custom",),
    b_prop_export = "bf_namelist_export",
)

# FIXME other views of SURF and work on this!!!
BFNamelist_SURF(
    name = "SURF2",
    description = "Boundary Condition second",
    f_name = "SURF",
    bpy_type = Material,
    bf_params = ("Namelist","ID","FYI","Custom",),
    b_prop_export = "bf_namelist_export",
)

## Sections

BFSection(
    name = "General configuration",
    bf_namelists = ("HEAD","REAC","DUMP"),
)

BFSection(
    name = "Boundary conditions",
    bf_namelists = ("SURF","SURF2"),
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
    name = "Others",
)


