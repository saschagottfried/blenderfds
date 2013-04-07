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
from .bf_types import BFProp, BFParam, BFNamelist, BFSection, bf_props, bf_params, bf_namelists, bf_sections
from .bf_basic_types import BFList, BFResult, BFError
from . import bf_operators, bf_geometry, bf_config

### Generic BFParam

BFProp(
    name = "bf_id",
    label = "ID",
    description = "Element identificator",
    bpy_name = "name",
)

BFParam(
    name = "ID",
    fds_name = "ID",
    bf_props = "bf_id",
)

BFParam(
    name = "ID_no_export",
    bf_props = "bf_id",
)

BFProp(
    name = "bf_fyi",
    label = "FYI",
    description = "Free text description",
    bpy_prop = bpy.props.StringProperty,
    maxlen = 60,
)

BFParam(
    name = "FYI",
    fds_name = "FYI",
    bf_props = "bf_fyi",
)

class BFParam_Custom(BFParam):
    def _is_exported(self,context,element):
        return True
    def _check(self,value):
        if isinstance(value,str):
            if "`" in value or "‘" in value or "’‌" in value:
                raise BFError(self,"Typographic quotes not allowed, use matched single quotes")
            elif '"' in value or "”" in value:
                raise BFError(self,"Double quotes not allowed, use matched single quotes")
            elif value.count("'") % 2 != 0:
                raise BFError(self,"Unmatched single quotes")
    def _draw_bf_props(self,context,element,layout):
        col = layout.column()
        col.label(text="Custom Parameters:")
        col.prop(element,self.bf_props[0].bpy_name,text="")

BFProp(
    name = "bf_custom",
    label = "Custom parameters:",
    description = "Custom parameters are appended verbatim to namelist parameters. Use matched single quotes.",
    bpy_prop = bpy.props.StringProperty,
    maxlen = 1024,
)

BFParam_Custom(
    name = "Custom",
    bf_props = "bf_custom",
)

### HEAD

class BFParam_filename(BFParam):
    def _check(self,value):
        if bpy.path.clean_name(value) != value: raise BFError(self,"Illegal characters in filename")

BFProp(
    name = "bf_chid",
    label = "CHID",
    description = "Case identificator",
    bpy_name = "name",
)

BFParam_filename(
    name = "CHID",
    fds_name = "CHID",
    bf_props = "bf_chid",
)

BFProp(
    name = "bf_head_title",
    label = "TITLE",
    description = "Case description",
    bpy_prop = bpy.props.StringProperty,
    maxlen = 64,
)

BFParam(
    name = "TITLE",
    fds_name = "TITLE",
    bf_props = "bf_head_title",
)

BFNamelist(
    name = "HEAD",
    unique_id = 1,
    description = "Header",
    fds_name = "HEAD",
    bpy_type = bpy.types.Scene,
    bf_params = ("CHID","TITLE"),
)

### Case Config

class BFParam_path_exists(BFParam):
    def _check(self,value):
        if not os.path.exists(bpy.path.abspath(value)): raise BFError(self,"Path does not exist")

BFProp(
    name = "bf_case_directory",
    label = "Case Directory",
    description = "Case directory",
    bpy_prop = bpy.props.StringProperty,
    subtype = "DIR_PATH",
    maxlen = 1024,
)

BFParam_path_exists(
    name = "Case Directory",
    bf_props = "bf_case_directory",
)

BFProp(
    name = "bf_ext_config_filepath",
    label = "Ext Config File",
    description = "Path to external configuration file",
    bpy_prop = bpy.props.StringProperty,
    default = "//config.fds",
    subtype = "FILE_PATH",
    maxlen = 1024,
)

BFParam_path_exists(
    name = "External Config File",
    bf_props = "bf_ext_config_filepath",
    has_auto_export = True,
)

BFProp(
    name = "bf_version",
    label = "Version",
    description = "BlenderFDS version used for file creation and exporting",
    bpy_prop = bpy.props.IntVectorProperty,
    default = (0,0,0),
    size = 3,
)

BFParam(
    name = "Version",
    bf_props = "bf_version",
)
# FIXME bl_info["version"]

BFNamelist(
    name = "Case Configuration",
    unique_id = 2,
    bpy_type = bpy.types.Scene,
    bf_params = ("Case Directory","External Config File","Version"),
)

### TIME

BFProp(
    name = "bf_time_t_begin",
    label = "T_BEGIN",
    description = "Simulation starting time",
    bpy_prop = bpy.props.FloatProperty,
    step = 100.,
    precision = 1,
    min = 0.,
    default = 0.,
)

BFParam(
    name = "T_BEGIN",
    fds_name = "T_BEGIN",
    bf_props = "bf_time_t_begin",
)

BFProp(
    name = "bf_time_t_end",
    label = "T_END",
    description = "Simulation ending time",
    bpy_prop = bpy.props.FloatProperty,
    step = 100.,
    precision = 1,
    min = 0.,
    default= 0.,
)

BFParam(
    name = "T_END",
    fds_name = "T_END",
    bf_props = "bf_time_t_end",
)

BFProp(
    name = "bf_time_custom",
    label = "Custom",
    description = "Custom parameters are appended verbatim to namelist parameters. Use matched single quotes.",
    bpy_prop = bpy.props.StringProperty,
    maxlen = 1024,
)

BFParam_Custom(
    name = "TIME_Custom",
    bf_props = "bf_time_custom",
)

BFNamelist(
    name = "TIME",
    unique_id = 3,
    description = "Time",
    fds_name = "TIME",
    bpy_type = bpy.types.Scene,
    bf_params = ("T_BEGIN","T_END","TIME_Custom"),
    has_auto_export = True,
)

### MISC

# FIXME TMPA, VISIBILITY_FACTOR, EVAC params

BFProp(
    name = "bf_misc_custom",
    label = "Custom",
    description = "Custom parameters are appended verbatim to namelist parameters. Use matched single quotes.",
    bpy_prop = bpy.props.StringProperty,
    maxlen = 1024,
)

BFParam_Custom(
    name = "MISC_Custom",
    bf_props = "bf_misc_custom",
)

BFNamelist(
    name = "MISC",
    unique_id = 4,
    description = "Miscellaneous parameters",
    fds_name = "MISC",
    bpy_type = bpy.types.Scene,
    bf_params = ("MISC_Custom",),
    has_auto_export = True,
)

### REAC

BFProp(
    name = "bf_reac_fuel",
    label = "FUEL",
    description = "Identificator of fuel species",
    bpy_prop = bpy.props.StringProperty,
    maxlen = 32,
)

BFParam(
    name = "FUEL",
    fds_name = "FUEL",
    bf_props = "bf_reac_fuel",
)

BFProp(
    name = "bf_reac_formula",
    label = "FORMULA",
    description = "Chemical formula of fuel species, it can only contain C, H, O, or N",
    bpy_prop = bpy.props.StringProperty,
    maxlen = 32,
)

BFParam(
    name = "FORMULA",
    fds_name = "FORMULA",
    bf_props = "bf_reac_formula",
    has_auto_export = True,
)

BFProp(
    name = "bf_reac_co_yield",
    label = "CO_YIELD",
    description = "Fraction of fuel mass converted into carbon monoxide",
    bpy_prop = bpy.props.FloatProperty,
    step = 1.,
    precision = 2,
    min = 0.,
    max = 1.,
    default = 0.,
)

BFParam(
    name = "CO_YIELD",
    fds_name = "CO_YIELD",
    bf_props = "bf_reac_co_yield",
    has_auto_export = True,
)

BFProp(
    name = "bf_reac_soot_yield",
    label = "SOOT_YIELD",
    description = "Fraction of fuel mass converted into smoke particulate",
    bpy_prop = bpy.props.FloatProperty,
    step = 1.,
    precision = 2,
    min = 0.,
    max = 1.,
    default = 0.,
)

BFParam(
    name = "SOOT_YIELD",
    fds_name = "SOOT_YIELD",
    bf_props = "bf_reac_soot_yield",
    has_auto_export = True,
)

BFProp(
    name = "bf_reac_heat_of_combustion",
    label = "HEAT_OF_COMBUSTION",
    description = "Fuel heat of combustion",
    bpy_prop = bpy.props.FloatProperty,
    step = 100000.,
    precision = 1,
    min = 0.,
    default = 0.,
)

BFParam(
    name = "HEAT_OF_COMBUSTION",
    fds_name = "HEAT_OF_COMBUSTION",
    bf_props = "bf_reac_heat_of_combustion",
    has_auto_export = True,
)

BFProp(
    name = "bf_reac_custom",
    label = "Custom",
    description = "Custom parameters are appended verbatim to namelist parameters. Use matched single quotes.",
    bpy_prop = bpy.props.StringProperty,
    maxlen = 1024,
)

BFParam_Custom(
    name = "REAC_Custom",
    bf_props = "bf_reac_custom",
)

BFNamelist(
    name = "REAC",
    description = "Reaction",
    unique_id = 5,
    fds_name = "REAC",
    bpy_type = bpy.types.Scene,
    bf_params = ("FUEL","FORMULA","CO_YIELD","SOOT_YIELD","HEAT_OF_COMBUSTION","REAC_Custom"),
    has_auto_export = True,
)

### DUMP

# FIXME show DT

BFProp(
    name = "bf_dump_nframes",
    label = "NFRAMES",
    description = "Number of output dumps per calculation",
    bpy_prop = bpy.props.IntProperty,
    min = 1,
    default = 1000,
)

BFParam(
    name = "NFRAMES",
    fds_name = "NFRAMES",
    bf_props = "bf_dump_nframes",
    has_auto_export = True,
)

BFProp(
    name = "bf_dump_custom",
    label = "Custom",
    description = "Custom parameters are appended verbatim to namelist parameters. Use matched single quotes.",
    bpy_prop = bpy.props.StringProperty,
    maxlen = 1024,
)

BFParam_Custom(
    name = "DUMP_Custom",
    bf_props = "bf_dump_custom",
)

BFNamelist(
    name = "DUMP",
    unique_id = 6,
    description = "Output parameters",
    fds_name = "DUMP",
    bpy_type = bpy.types.Scene,
    bf_params = ("NFRAMES","DUMP_Custom"),
    has_auto_export = True,
)

### Object

class BFParam_XB(BFParam):

    def _value(self,context,element):
        value = element.bf_xb
        if value == "VOXELS":
            xbs = bf_geometry.get_bbox(context,element)[0]
            (bbminx, bbmaxx, bbminy, bbmaxy, bbminz, bbmaxz,) = xbs[0]
            dimension = max(bbmaxx-bbminx, bbmaxy-bbminy, bbmaxz-bbminz)
            dimension_too_large = bf_geometry.calc_remesh(context, dimension, element.bf_voxel_size)[3]
            if dimension_too_large: raise BFError(self,"Object too large, voxel size not guaranteed")
        return value

    def _draw_extra(self,context,element,layout):
        if element.bf_xb == "VOXELS":
            row = layout.row()
            bf_prop = self.bf_props["bf_voxel_size"]
            row.prop(element,bf_prop.bpy_name,text=bf_prop.label)
            if element.bf_has_voxels_shown: row.operator("object.bf_hide_voxels") 
            else: row.operator("object.bf_show_voxels")

    def _format(self,value):
        return "XB={0[0]:.3f},{0[1]:.3f},{0[2]:.3f},{0[3]:.3f},{0[4]:.3f},{0[5]:.3f}".format(value)
    
    def to_fds(self,context,element):
        # Check and init
        if not self._is_exported(context,element): return None
        res = self.evaluate(context,element)
        # Select case
        if res.value == "BBOX":
            xbs, tt = bf_geometry.get_bbox(context,element)
        elif res.value == "VOXELS":
            xbs, tt, dimension_too_large = bf_geometry.get_voxels(context,element)
            res.msgs.append("{0} voxels of size {1:.3f}, in {2:.3f} s".format(len(xbs),element.bf_voxel_size,tt))
        elif res.value == "FACES":
            xbs, tt = bf_geometry.get_faces(context,element)
            res.msgs.append("{0} faces, in {1:.3f} s".format(len(xbs),tt))
        elif res.value == "EDGES":
            xbs, tt = bf_geometry.get_edges(context,element)
            res.msgs.append("{0} edges, in {1:.3f} s".format(len(xbs),tt))
        else: raise Exception("XB type unknown")
        res.value = tuple((self._format(xb) for xb in xbs))
        return res

BFProp(
    name = "bf_xb",
    label = "XB",
    description = "XB",
    bpy_prop = bpy.props.EnumProperty,
    items = (
        ("VOXELS","Voxelized Solid","Export voxels from voxelized solid"),
        ("BBOX","Bounding Box","Use object bounding box"),
        ("FACES","Faces","Faces, one for each face of this object"),
        ("EDGES","Edges","Segments, one for each edge of this object"),
        ),
    default = "BBOX",
)

BFProp(
    name = "bf_voxel_size",
    label = "Voxel Size",
    has_auto_ui = False,
    description = "Minimum resolution for object voxelization",
    bpy_prop = bpy.props.FloatProperty,
    step = 1,
    precision = 3,
    min = .01,
    max = 2.,
    default = .10,
    update = bf_operators.update_voxels,
)

BFProp(
    name = "bf_is_voxels",
    label = "Is a Voxel Object",
    has_auto_ui = False,
    description = "This is a voxel temporary object",
    bpy_prop = bpy.props.BoolProperty,
    default = False,
)

BFProp(
    name = "bf_has_voxels_shown",
    label = "Has Voxel Shown",
    has_auto_ui = False,
    description = "This object has a visible voxel object companion",
    bpy_prop = bpy.props.BoolProperty,
    default = False,
)

BFParam_XB(
    name = "XB",
    fds_name = "XB",
    bf_props = ("bf_xb","bf_voxel_size","bf_is_voxels","bf_has_voxels_shown"),
    has_auto_export = True,
)

class BFParam_XYZ(BFParam):
    def _value(self,context,element):
        bf_xb, bf_xyz = None, None
        if element.has_bf_param("XB") and element.bf_xb_export: bf_xb = element.bf_xb
        if element.has_bf_param("XYZ") and element.bf_xyz_export: bf_xyz = element.bf_xyz
        if bf_xyz == "VERTICES" and bf_xb in ("VOXELS","FACES","EDGES"): raise BFError(self,"Conflicting with XB")
        return bf_xyz

    def _format(self,value):
        return "XYZ={0[0]:.3f},{0[1]:.3f},{0[2]:.3f}".format(value)
        
    def to_fds(self,context,element):
        # Check and init
        if not self._is_exported(context,element): return None
        res = self.evaluate(context,element)
        # Select case
        if res.value == "CENTER":
            xyzs, tt = bf_geometry.get_center(context,element)
        elif res.value == "VERTICES":
            xyzs, tt = bf_geometry.get_vertices(context,element)
            res.msgs.append("{0} vertices, in {1:.3f} s".format(len(xyzs),tt))
        else: raise Exception("XYZ type unknown")
        res.value = tuple((self._format(xyz) for xyz in xyzs))
        return res

BFProp(
    name = "bf_xyz",
    label = "XYZ",
    description = "Set points",
    bpy_prop = bpy.props.EnumProperty,
    items = [
        ("CENTER","Center","Point, corresponding to the center point of this object"),
        ("VERTICES","Vertices","Points, one for each vertex of this object"),
        ],
    default = "CENTER",
)

BFParam_XYZ(
    name = "XYZ",
    fds_name = "XYZ",
    bf_props = "bf_xyz",
    has_auto_export = True,
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

    def _format(self,value):
        return "PB{0[0]}={0[1]:.3f}".format(value)
    
    def to_fds(self,context,element):
        # Check and init
        if not self._is_exported(context,element): return None
        res = self.evaluate(context,element)
        # Select case
        if res.value == "PLANES":
            pbs, tt = bf_geometry.get_planes(context,element)
            res.msgs.append("{0} planes, in {1:.3f} s".format(len(pbs),tt))
        else: raise Exception("PB type unknown")
        res.value = tuple((self._format(pb) for pb in pbs))
        return res

BFProp(
    name = "bf_pb",
    label = "PB*",
    description = "Set planes",
    bpy_prop = bpy.props.EnumProperty,
    items = [("PLANES","Planes","Planes, one for each face of this object"),],
    default = "PLANES",
)

BFParam_PB(
    name = "PB",
    fds_name = "PB",
    bf_props = "bf_pb",
    has_auto_export = True,
)

class BFParam_DEVC_ID(BFParam):
    def _draw_bf_props(self,context,element,layout):
        row = layout.row()
        row.prop_search(element,"bf_devc_id",bpy.data,"objects",text="DEVC_ID")

BFProp(
    name = "bf_devc_id",
    label = "DEVC_ID",
    description = "DEVC_ID",
    bpy_prop = bpy.props.StringProperty,
)

BFParam_DEVC_ID(
    name = "DEVC_ID",
    fds_name = "DEVC_ID",
    bf_props = "bf_devc_id",
    has_auto_export = True,
)

class BFParam_SURF_ID(BFParam):
    def _draw_bf_props(self,context,element,layout):
        row = layout.row()
        row.prop_search(element,"active_material",bpy.data,"materials",text="SURF_ID:")
    def _is_exported(self,context,element):
        return element.active_material and element.active_material.bf_namelist_export or False
    def _has_active_ui(self,context,element):
        return self.is_exported(context,element)
    def _value(self,context,element):
        if element.active_material: return element.active_material.name

BFParam_SURF_ID(
    name = "SURF_ID",
    fds_name = "SURF_ID",
)

BFProp(
    name = "bf_devc_quantity",
    label = "QUANTITY",
    description = "Output quantity",
    bpy_prop = bpy.props.StringProperty,
    maxlen = 32,
)

BFParam(
    name = "QUANTITY",
    fds_name = "QUANTITY",
    bf_props = "bf_devc_quantity",
)

BFProp(
    name = "bf_slcf_vector",
    label = "VECTOR",
    description = "Create animated vectors",
    bpy_prop = bpy.props.BoolProperty,
    default = False,
)

BFParam(
    name = "VECTOR",
    fds_name = "VECTOR",
    bf_props = "bf_slcf_vector",
    has_auto_export = True,
)


BFProp(
    name = "bf_mesh_ijk",
    label = "IJK",
    description = "Cell number in x, y, and z direction",
    operator = "object.bf_correct_ijk",
    bpy_prop = bpy.props.IntVectorProperty,
    default = (10,10,10),
    size = 3,
)

# FIXME check number for Poisson and inform user
class BFParam_IJK(BFParam):
    def evaluate(self,context,element):
        value = element.bf_mesh_ijk
        msg = "{0} mesh cells of size {1[0]:.3f} x {1[1]:.3f} x {1[2]:.3f}".format(bf_geometry.get_cell_number(element),bf_geometry.get_cell_size(element))
        return BFResult(sender=self,value=value,msg=msg)
        
BFParam_IJK(
    name = "IJK",
    fds_name = "IJK",
    bf_props = "bf_mesh_ijk",
    has_auto_export = True,
)

### SURF

class BFParam_RGB(BFParam):
    def _value(self,context,element):
        value = element.diffuse_color
        return int(value[0]*255),int(value[1]*255),int(value[2]*255)

BFProp(
    name = "bf_rgb",
    label = "RGB",
    description = "RGB",
    bpy_name = "diffuse_color",
)

BFParam_RGB(
    name = "RGB",
    fds_name = "RGB",
    bf_props = "bf_rgb",
)

BFProp(
    name = "bf_transparency",
    label = "TRANSPARENCY",
    description = "Transparency",
    bpy_name = "alpha",
)

BFProp(
    name = "bf_transparency_export",
    label = "Export",
    description = "Export parameter",
    bpy_name = "use_transparency",
)

BFParam(
    name = "TRANSPARENCY",
    fds_name = "TRANSPARENCY",
    bf_props = "bf_transparency",
    bf_prop_export = "bf_transparency_export",
)

### Custom Namelist

BFProp(
    name = "bf_custom_namelist",
    description = "Custom namelist body",
    bpy_prop = bpy.props.StringProperty,
    maxlen = 1024,
    default = "ABCD P1='Example' P2=1234.56"
)

class BFParam_Custom_Namelist(BFParam):
    def _check(self,value):
        if isinstance(value,str):
            if not value:
                raise BFError(self,"Empty string")
            elif '&' in value or '/' in value:
                raise BFError(self,"Starting '&' and ending '/' not needed")
            elif "`" in value or "‘" in value or "’‌" in value:
                raise BFError(self,"Typographic quotes not allowed, use matched single quotes")
            elif '"' in value or "”" in value:
                raise BFError(self,"Double quotes not allowed, use matched single quotes")
            elif value.count("'") % 2 != 0:
                raise BFError(self,"Unmatched single quotes")
    def _draw_bf_props(self,context,element,layout):
        row = layout.row()
        row.prop(element,self.bf_props[0].bpy_name,text="")

# No fds_name, no automatic export
BFParam_Custom_Namelist(
    name = "Custom Namelist",
    bf_props = "bf_custom_namelist",
)

class BFNamelist_Custom(BFNamelist):
    
    def _is_exported(self,context,element):
        """If self with element is going to be exported return True, else False"""
        if self.bf_prop_export: return self.bf_prop_export.value(context,element)
        return True

    def to_fds(self,context,element):
        print("BlenderFDS: > > > BFNamelist.to_fds: {}".format(self.name))
        # Check
        if not self._is_exported(context,element): return None
        # Export FIXME works but...
        res = self.evaluate(context,element) # Do not trap exceptions, pass them upward
        index = self.bf_params[:].index(bf_params["Custom Namelist"])
        children = self.bf_params[:]
        children.pop(index)
        children_values, children_msgs = self._to_fds_children(children=children,context=context,element=element)
        # Check and append value and msgs
        child_res = bf_params["Custom Namelist"].evaluate(context=context,element=element)
        if child_res is None: return None
        if child_res.msgs: children_msgs.extend(child_res.labels)
        if not child_res.value: return None
        fds_name = child_res.value
        # res.value from self.evaluate() is used then replaced with new content
        res.value = self._format(context,element,fds_name,children_values,children_msgs)
        return res

BFProp(
    name = "bf_namelist_export",
    label = "Export",
    description = "Export namelist to FDS",
    bpy_prop = bpy.props.BoolProperty,
    default = False,
)


### Object bf_namelists

BFNamelist_Custom(
    name = "Custom",
    label = "Custom",
    unique_id = 7,
    bpy_type = bpy.types.Object,
    bf_params = ("Namelist","Custom Namelist","FYI","XB","XYZ","PB"),
    bf_prop_export = "bf_namelist_export",
)

BFNamelist(
    name = "OBST",
    unique_id = 8,
    description = "Obstruction",
    fds_name = "OBST",
    bpy_type = bpy.types.Object,
    bf_params = ("Namelist","ID","FYI","SURF_ID","XB","DEVC_ID","Custom",),
    bf_prop_export = "bf_namelist_export",
)

BFNamelist(
    name = "HOLE",
    unique_id = 9,
    description = "Obstruction Cutout",
    fds_name = "HOLE",
    bpy_type = bpy.types.Object,
    bf_params = ("Namelist","ID_no_export","FYI","XB","DEVC_ID","Custom",),
    bf_prop_export = "bf_namelist_export",
)

BFNamelist(
    name = "VENT",
    unique_id = 10,
    description = "Boundary Condition Patch",
    fds_name = "VENT",
    bpy_type = bpy.types.Object,
    bf_params = ("Namelist","ID","FYI","SURF_ID","XB","XYZ","PB","DEVC_ID","Custom",),
    bf_prop_export = "bf_namelist_export",
)

# FIXME INITIAL_STATE, LATCH, PROP_ID, SETPOINT, TRIP_DIRECTION
BFNamelist(
    name = "DEVC",
    unique_id = 11,
    description = "Device",
    fds_name = "DEVC",
    bpy_type = bpy.types.Object,
    bf_params = ("Namelist","ID","FYI","QUANTITY","XB","XYZ","DEVC_ID","Custom",),
    bf_prop_export = "bf_namelist_export",
)

BFNamelist(
    name = "SLCF",
    unique_id = 12,
    description = "Slice File",
    fds_name = "SLCF",
    bpy_type = bpy.types.Object,
    bf_params = ("Namelist","ID_no_export","FYI","QUANTITY","VECTOR","XB","PB","DEVC_ID","Custom",),
    bf_prop_export = "bf_namelist_export",
)

# FIXME IOR
BFNamelist(
    name = "PROF",
    unique_id = 13,
    description = "Wall Profile Output",
    fds_name = "PROF",
    bpy_type = bpy.types.Object,
    bf_params = ("Namelist","ID","FYI","QUANTITY","XYZ","Custom",),
    bf_prop_export = "bf_namelist_export",
)

# FIXME RGB???
BFNamelist(
    name = "MESH",
    unique_id = 14,
    description = "Domain of simulation",
    fds_name = "MESH",
    bpy_type = bpy.types.Object,
    bf_params = ("Namelist","ID","FYI","IJK","XB","Custom",),
    bf_prop_export = "bf_namelist_export",
)

BFNamelist(
    name = "INIT",
    unique_id = 15,
    description = "Initial condition",
    fds_name = "INIT",
    bpy_type = bpy.types.Object,
    bf_params = ("Namelist","ID","FYI","XB","XYZ","DEVC_ID","Custom",),
    bf_prop_export = "bf_namelist_export",
)

BFNamelist(
    name = "ZONE",
    unique_id = 16,
    description = "Pressure zone",
    fds_name = "ZONE",
    bpy_type = bpy.types.Object,
    bf_params = ("Namelist","ID","FYI","XB","Custom",),
    bf_prop_export = "bf_namelist_export",
)

### Material bf_namelists

class BFNamelist_SURF(BFNamelist):
    def _check(self,value):
        if not set(bf_config.predefined_material_names) <= set(bpy.data.materials.keys()):
            raise BFError(sender=self,msg="Predefined SURFs unset",operator="material.bf_set_predefined")

BFNamelist_SURF(
    name = "SURF",
    unique_id = 17,
    description = "Boundary Condition",
    fds_name = "SURF",
    bpy_type = bpy.types.Material,
    bf_params = ("Namelist","ID","FYI","RGB","TRANSPARENCY","Custom",),
    bf_prop_export = "bf_namelist_export",
)

# FIXME other views of SURF and work on this!!!
BFNamelist_SURF(
    name = "SURF2",
    unique_id = 18,
    description = "Boundary Condition second",
    fds_name = "SURF",
    bpy_type = bpy.types.Material,
    bf_params = ("Namelist","ID","FYI","Custom",),
    bf_prop_export = "bf_namelist_export",
)

## Sections

BFSection(
    name = "General configuration",
    bf_namelists = ("HEAD","TIME","MISC","REAC","DUMP"),
)

class BFSection_External_Config(BFSection):
    def to_fds(self,context,element):
        print("BlenderFDS: > BFSection.to_fds: {}".format(self.name))
        bf_param = bf_params["External Config File"]
        if not bf_param.bf_prop_export.value(context,context.scene): return None # Nothing to send
        # Evaluate res to get external config filepath
        res = bf_param.evaluate(context,context.scene) # Do not trap exceptions, pass them upward
        if res is None or res.value is None: return res # Could have msgs
        filepath = bpy.path.abspath(res.value)
        # Read file
        try:
            with open(filepath) as in_file:
                res.value = in_file.read() + "\n"
        except IOError: raise BFError(sender=self,msgs="External config file not readable: {}".format(filepath))
        # Check
        res.msgs.append(filepath)
        res.value = self._format(res.value,res.labels)
        return res

BFSection_External_Config(
    name = "External configuration",
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
    name = "Other",
    bf_namelists = ("Custom"),
)

# Some sanity checks

# Check unused defined objects
bf_props_unused = set(bf_props) - set(bf_prop for bf_param in bf_params for bf_prop in bf_param.bf_props) \
    - set(bf_param.bf_prop_export for bf_param in bf_params if bf_param.bf_prop_export is not None) \
    - set(bf_namelist.bf_prop_export for bf_namelist in bf_namelists if bf_namelist.bf_prop_export is not None)
bf_params_unused = set(bf_params) - set(bf_param for bf_namelist in bf_namelists for bf_param in bf_namelist.bf_params)
bf_namelists_unused = set(bf_namelists) - set(bf_namelist for bf_section in bf_sections for bf_namelist in bf_section.bf_namelists)
# bf_sections_unused = set(bf_sections) - set(bf_file.bf_sections) All bf_sections are always used!
print("BlenderFDS: Unused bf_props:",bf_props_unused)
print("BlenderFDS: Unused bf_params:",bf_params_unused)
print("BlenderFDS: Unused bf_namelists:",bf_namelists_unused)

# Check double bf_sections, bf_namelists, bf_params, bf_props
