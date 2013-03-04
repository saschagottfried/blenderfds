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

from .bf_types import BFError, BFResult, BProp, BFParam, BFNamelist, BFSection, bf_params, bf_namelists
from .bf_geometry import get_edges, get_center, get_faces, get_planes, get_vertices, get_bbox, get_voxels, calc_remesh
from .bf_format import *
from . import bf_config

from bpy.props import StringProperty, EnumProperty, BoolProperty, IntVectorProperty, FloatProperty
from bpy.types import Scene, Object, Material
import bpy
import os

### OLD FIXME

# Namelist description
nls = {"OBST": "Obstruction",
       "HOLE": "Obstruction Cutout",
       "VENT": "Boundary Condition Patch",
       "DEVC": "Device",
       "SLCF": "Slice File",
       "PROF": "Wall Profile Output",
       "MESH": "Domain of Simulation",
       "INIT": "Initial Condition",
       "ZONE": "Pressure Zone",
       "EVAC": "Evac Agents Position",
       "EVHO": "Evac Agents Hole",
       "EXIT": "Evac Exit",
       "ENTR": "Evac Entry",
       "DOOR": "Evac Door",
       "CORR": "Evac Corridor",
       "EVSS": "Evac Incline",
       "STRS": "Evac Staircase",
       "SURF": "Boundary Condition",
      }

# Namelist parameters
# Possible choices: "ID","FYI","SURF_ID","SAWTOOTH","IJK","XB","XYZ","PB","RGB","TRANSPARENCY","OUTLINE"
nl_params = {"OBST": ("ID","FYI","SURF_ID","SAWTOOTH","THICKEN","XB","OUTLINE"),
             "HOLE": (     "FYI","XB",),
             "VENT": ("ID","FYI","SURF_ID","XB","XYZ","PB","OUTLINE"),
             "DEVC": ("ID","FYI","SURF_ID","XB","XYZ","PB",),
             "SLCF": (     "FYI","XB","PB",),
             "PROF": ("ID","FYI","XYZ",),
             "MESH": ("ID","FYI","IJK","XB",),
             "INIT": (     "FYI","XB",),
             "ZONE": ("ID","FYI","XB",),
             "EVAC": ("ID","FYI","XB",),
             "EVHO": ("ID","FYI","XB",),
             "EXIT": ("ID","FYI","XB","XYZ",),
             "ENTR": ("ID","FYI","XB",),
             "DOOR": ("ID","FYI","XB","XYZ",),
             "CORR": ("ID","FYI","XB",),
             "EVSS": ("ID","FYI","XB",),
             "STRS": ("ID","FYI","XB","XYZ",),
             "SURF": ("ID","FYI","RGB","TRANSPARENCY",),
            }

# Namelist groups, a tuple for selecting
# The last empty group is used to collect all the remaining namelist groups.
nl_groups = (("Boundary condition defs",  ("SURF",)),
             ("Computational domain",     ("MESH", "INIT", "ZONE")),
             ("Geometry",                 ("OBST", "HOLE", "VENT")),
             ("Evacuation",               ("EVAC","EVHO","EXIT","ENTR","DOOR","CORR","EVSS","STRS")),
             ("Control logic and output", ("DEVC", "SLCF", "PROF")),
             ("Others",                   ()),
            )

### Generic

BFParam(
    name = "FYI",
    f_name = "FYI",
    b_props = BProp(
        name = "bf_fyi",
        label = "FYI",
        description = "Free text description",
        bpy_prop = StringProperty,
        other = {"maxlen":60},
    ),
    b_prop_export = BProp(
        name = "bf_fyi_export",
        label = "Export",
        description = "Export parameter",
        bpy_prop = BoolProperty,
        other = {"default":False},
    ),
)

class BFParam_Custom(BFParam):
    def is_exported(self,context,element):
        return self.b_prop_export.value(context,element)
    def value(self,context,element):
        value = self.b_props[0].value(context,element)
        if not value:
            raise BFError(self,"Empty string")
        elif "`" in value or "‘" in value or "’‌" in value:
            raise BFError(self,"Typographic quotes not allowed, use matched single quotes")
        elif '"' in value or "”" in value:
            raise BFError(self,"Double quotes not allowed, use matched single quotes")
        elif value.count("'") % 2 != 0:
            raise BFError(self,"Unmatched single quotes")
        return value
    def to_fds(self,context,element):
        # Check and init
        if not self.is_exported(context,element): return None
        return BFResult(sender=self,value=str(self.value(context,element)))

BFParam_Custom(
    name = "Custom",
    f_name = "Custom",
    b_props = BProp(
        name = "bf_custom",
        label = "Custom",
        description = "Custom parameters are appended verbatim to namelist parameters. Use matched single quotes.",
        bpy_prop = StringProperty,
        other = {"maxlen":1024},
    ),
    b_prop_export = BProp(
        name = "bf_custom_export",
        label = "Export",
        description = "Export parameter",
        bpy_prop = BoolProperty,
        other = {"default":False},
    ),
)

### HEAD

class BFParam_filename(BFParam):
    def value(self,context,element):
        value = BFParam.value(self,context,element) # Add new check to previous ones
        if bpy.path.clean_name(value) != value: raise BFError(self,"Illegal character in filename")
        return value

BFParam_filename(
    name = "CHID",
    f_name = "CHID",
    b_props = BProp(
        name = "name",
        label = "CHID",
        description = "Case identificator",
    ),
)

BFParam(
    name = "TITLE",
    f_name = "TITLE",
    b_props = BProp(
        name = "bf_title",
        label = "TITLE",
        description = "Case description",
        bpy_prop = StringProperty,
        other = {"maxlen":60},
    ),
)

class BFParam_path_exists(BFParam):
    def value(self,context,element):
        value = self.b_props[0].value(context,element)
        if not os.path.exists(bpy.path.abspath(value)): raise BFError(self,"Path does not exists")
        return value

BFParam_path_exists(
    name = "Case Directory",
    b_props = BProp(
        name = "bf_case_directory",
        label = "Case Directory",
        description = "Case directory",
        bpy_prop = StringProperty,
        other = {"subtype":"DIR_PATH","maxlen":1024},
    ),
)

BFParam_path_exists(
    name = "Config File",
    b_props = BProp(
        name = "bf_config_filepath",
        label = "Config File",
        description = "Path to external configuration file",
        bpy_prop = StringProperty,
        other = {"default":"config.fds","subtype":"FILE_PATH","maxlen":1024},
    ),
    b_prop_export = BProp(
        name = "bf_config_filepath_export",
        label = "Export",
        description = "Export parameter",
        bpy_prop = BoolProperty,
        other = {"default":False},
    ),
)

BFParam(
    name = "Version",
    b_props = BProp(
        name = "bf_version",
        label = "Version",
        description = "BlenderFDS version used for file creation and exporting",
        bpy_prop = IntVectorProperty,
        other = {"default":(0,0,0),"size":3},
    ),
)
# FIXME bl_info["version"]

### REAC

BFParam(
    name = "FUEL",
    f_name = "FUEL",
    b_props = BProp(
        name = "bf_reac_fuel",
        label = "FUEL",
        description = "Fuel",
        bpy_prop = StringProperty,
        other = {"maxlen":32},
    ),
)

BFParam_Custom(
    name = "REAC_Custom",
    f_name = "Custom",
    b_props = BProp(
        name = "bf_reac_custom",
        label = "Custom",
        description = "Custom parameters are appended verbatim to namelist parameters. Use matched single quotes.",
        bpy_prop = StringProperty,
        other = {"maxlen":1024},
    ),
    b_prop_export = BProp(
        name = "bf_reac_custom_export",
        label = "Export",
        description = "Export parameter",
        bpy_prop = BoolProperty,
        other = {"default":False},
    ),
)

### Object

BFParam(
    name = "Namelist",
    b_props = BProp(
        name = "bf_namelist",
        label = "Namelist",
        description = "Namelist group name (OBST, VENT, ...)",
        bpy_prop = StringProperty,
        other = {"default":"OBST","maxlen":4},
    ),
)

BFParam(
    name = "ID",
    f_name = "ID",
    b_props = BProp(
        name = "name",
        label = "ID",
        description = "Element identificator",
    ),
)

#FIXME 
BFParam(
    name = "ID_no_export",
    b_props = BProp(
        name = "name",
        label = "ID",
        description = "Element identificator",
    ),
)

class BFParam_XB(BFParam):
    def value(self,context,element):
        value = element.bf_xb
        if value == "VOXELS":
            xbs = get_bbox(context,element)[0]
            (bbminx, bbmaxx, bbminy, bbmaxy, bbminz, bbmaxz,) = xbs[0]
            dimension = max(bbmaxx-bbminx, bbmaxy-bbminy, bbmaxz-bbminz)
            dimension_too_large = calc_remesh(context, dimension, element.bf_voxel_size)[3]
            if dimension_too_large: raise BFError(self,"Object too large, voxel size not guaranteed")
        return value
    def draw(self,context,element,layout):
        row = layout.row()
        layout_export, layout_ui = row.column(), row.column()
        layout_export.prop(element,self.b_prop_export.name,text="")
        layout_ui.active = self.has_active_ui(context,element)
        # bf_xb
        b_prop = self.b_props["bf_xb"]
        row = layout_ui.row()
        row.prop(element,b_prop.name,text=b_prop.label)
        if element.bf_xb == "VOXELS":
            b_prop = self.b_props["bf_voxel_size"]
            row = layout_ui.row()
            row.prop(element,b_prop.name,text=b_prop.label)
        # Errors?
        if layout.active and layout_ui.active:
            try: self.value(context,element)
            except BFError as err: err.draw(layout_ui)
    def to_fds(self,context,element):
        # Check and init
        if not self.is_exported(context,element): return None
        value = self.value(context,element)
        # Select case
        msg = None
        if value == "BBOX":
            xbs, tt = get_bbox(context,element)
        elif value == "VOXELS":
            xbs, tt, dimension_too_large = get_voxels(context,element)
            msg = "{0} voxels, sized {1:.3f}, in {2:.3f} s".format(len(xbs),element.bf_voxel_size,tt)
        elif value == "FACES":
            xbs, tt = get_faces(context,element)
            msg = "{0} faces, in {1:.3f} s".format(len(xbs),tt)
        elif value == "EDGES":
            xbs, tt = get_edges(context,element)
            msg = "{0} edges, in {1:.3f} s".format(len(xbs),tt)
        else: raise Exception("XB type unknown")
        value = tuple((format_xb(xb) for xb in xbs))
        return BFResult(self,value,msg)

items = (
    ("VOXELS","Voxelized Solid","Export voxels from voxelized solid"),
    ("BBOX","Bounding Box","Use object bounding box"),
    ("FACES","Faces","Faces, one for each face of this object"),
    ("EDGES","Edges","Segments, one for each edge of this object"),
    )

BFParam_XB(
    name = "XB",
    f_name = "XB",
    b_props = ((
        BProp(
            name = "bf_xb",
            label = "XB",
            description = "XB",
            bpy_prop = EnumProperty,
            other = {"items":items,"default":"BBOX"},
        ),
        BProp(
            name = "bf_voxel_size",
            label = "Voxel Size",
            description = "Minimum resolution for object voxelization",
            bpy_prop = FloatProperty,
            other = {"step":1,"precision":3,"min":.01,"max":2.,"default":.10},
            # FIXME update
        ),
        BProp(
            name = "bf_is_voxels",
            description = "This is a voxel temporary object",
            bpy_prop = BoolProperty,
            other = {"default":False},
        ),
    )),
    b_prop_export = BProp(
        name = "bf_xb_export",
        label = "Export",
        description = "Export parameter",
        bpy_prop = BoolProperty,
        other = {"default":False},
    ),
)

class BFParam_XYZ(BFParam):
    def value(self,context,element):
        # Get calling bf_namelist
        bf_namelist = bf_namelists.get(element.bf_namelist,bf_namelists["Unknown"])
        # Check
        bf_xb, bf_xyz = None, None
        if "XB" in bf_namelist.bf_params and element.bf_xb_export: bf_xb = element.bf_xb
        if "XYZ" in bf_namelist.bf_params and element.bf_xyz_export: bf_xyz = element.bf_xyz
        if bf_xyz == "VERTICES" and bf_xb in ("VOXELS","FACES","EDGES"): raise BFError(self,"Conflicting with XB")
        return element.bf_xyz
    
    def to_fds(self,context,element):
        # Check and init
        if not self.is_exported(context,element): return None
        value = self.value(context,element)
        # Select case
        msg = None
        if value == "CENTER":
            xyzs, tt = get_center(context,element)
        elif value == "VERTICES":
            xyzs, tt = get_vertices(context,element)
            msg = "{0} vertices, in {1:.3f} s".format(len(xyzs),tt)
        else: raise Exception("XYZ type unknown")
        value = tuple((format_xyz(xyz) for xyz in xyzs))
        return BFResult(self,value,msg)

items = [
    ("CENTER","Center","Point, corresponding to the center point of this object"),
    ("VERTICES","Vertices","Points, one for each vertex of this object"),
    ]

BFParam_XYZ(
    name = "XYZ",
    f_name = "XYZ",
    b_props = BProp(
        name = "bf_xyz",
        label = "XYZ",
        description = "Set points",
        bpy_prop = EnumProperty,
        other = {"items":items,"default":"CENTER"},
    ),
    b_prop_export = BProp(
        name = "bf_xyz_export",
        label = "Export",
        description = "Export parameter",
        bpy_prop = BoolProperty,
        other = {"default":False},
    ),
)

# FIXME what if no faces?

class BFParam_PB(BFParam):
    def value(self,context,element):
        # Get calling bf_namelist
        bf_namelist = bf_namelists.get(element.bf_namelist,bf_namelists["Unknown"])
        # Check
        msgs = list()
        bf_xb, bf_xyz, bf_pb = None, None, None
        if "XB" in bf_namelist.bf_params and element.bf_xb_export: bf_xb = element.bf_xb
        if "XYZ" in bf_namelist.bf_params and element.bf_xyz_export: bf_xyz = element.bf_xyz
        if "PB" in bf_namelist.bf_params and element.bf_pb_export: bf_pb = element.bf_pb
        if bf_pb == "PLANES" and bf_xb in ("VOXELS","FACES","EDGES"): msgs.append("Conflicting with XB")
        if bf_pb == "PLANES" and bf_xyz == "VERTICES": msgs.append("Conflicting with XYZ")
        # Errors?
        if msgs: raise BFError(self,msgs)
        return element.bf_pb
    def to_fds(self,context,element):
        # Check and init
        if not self.is_exported(context,element): return None
        value = self.value(context,element)
        # Select case
        msg = None
        if value == "PLANES":
            pbs, tt = get_planes(context,element)
            msg = "{0} planes, in {1:.3f} s".format(len(pbs),tt)
        else: raise Exception("PB type unknown")
        value = tuple((format_pb(pb) for pb in pbs))
        return BFResult(self,value,msg)

items = [("PLANES","Planes","Planes, one for each face of this object"),]

BFParam_PB(
    name = "PB",
    f_name = "PB",
    b_props = BProp(
        name = "bf_pb",
        label = "PB*",
        description = "Set planes",
        bpy_prop = EnumProperty,
        other = {"items":items,"default":"PLANES"},
    ),
    b_prop_export = BProp(
        name = "bf_pb_export",
        label = "Export",
        description = "Export parameter",
        bpy_prop = BoolProperty,
        other = {"default":False},
    ),
)

class BFParam_SURF_ID(BFParam):
    def is_exported(self,context,element):
        return element.active_material and element.active_material.bf_surf_export
    def value(self,context,element):
        if element.active_material:
            return element.active_material.name

BFParam_SURF_ID(
    name = "SURF_ID",
    f_name = "SURF_ID",
)

### SURF

class BFParam_RGB(BFParam):
    def value(self,context,element):
        value = BFParam.value(self,context,element)
        return int(value[0]*255),int(value[1]*255),int(value[2]*255)

BFParam_RGB(
    name = "RGB",
    f_name = "RGB",
    b_props = BProp(
        name = "diffuse_color",
        label = "RGB",
        description = "RGB",
    ),
)

BFParam(
    name = "TRANSPARENCY",
    f_name = "TRANSPARENCY",
    b_props = BProp(
        name = "alpha",
        label = "TRANSPARENCY",
        description = "Transparency",
    ),
    b_prop_export = BProp(
        name = "use_transparency",
        label = "Export",
        description = "Export parameter",
    ),
)

### Scene bf_namelists

BFNamelist(
    name = "HEAD",
    description = "Header and Case Config",
    f_name = "HEAD",
    bpy_type = Scene,
    bf_params = bf_params["CHID","TITLE","Case Directory","Config File","Version"],
)

BFNamelist(
    name = "REAC",
    description = "Reaction",
    f_name = "REAC",
    bpy_type = Scene,
    bf_params = bf_params["FUEL","REAC_Custom"],
    b_prop_export = BProp(
        name = "bf_reac_export",
        label = "Export",
        description = "Export namelist",
        bpy_prop = BoolProperty,
        other = {"default":False},
    ),
)

### Object bf_namelists

b_prop_export = BProp(
    name = "bf_export",
    label = "Export",
    description = "Export namelist",
    bpy_prop = BoolProperty,
    other = {"default":False},
)

BFNamelist(
    name = "Unknown",
    description = "Unknown Namelist",
    bpy_type = Object,
    bf_params = bf_params["Namelist","FYI","Custom",],
    b_prop_export = b_prop_export,
)

BFNamelist(
    name = "OBST",
    description = "Obstruction",
    f_name = "OBST",
    bpy_type = Object,
    bf_params = bf_params["Namelist","ID","FYI","SURF_ID","XB","XYZ","PB","Custom",],
    b_prop_export = b_prop_export,
)

BFNamelist(
    name = "HOLE",
    description = "Obstruction Cutout",
    f_name = "HOLE",
    bpy_type = Object,
    bf_params = bf_params["Namelist","ID_no_export","FYI","XB","Custom",],
    b_prop_export = b_prop_export,
)

### Material bf_namelists

class BFNamelist_SURF(BFNamelist):
    def check(self,context,element):
        if not set(bf_config.mas_predefined) <= set(bpy.data.materials.keys()):
            raise BFError(sender=self,msg="Predefined SURFs unset",operator="material.bf_set_predefined")

BFNamelist_SURF(
    name = "SURF",
    description = "Boundary Condition",
    f_name = "SURF",
    bpy_type = Material,
    bf_params = bf_params["ID","FYI","RGB","TRANSPARENCY","Custom",],
    b_prop_export = BProp(
        name = "bf_surf_export",
        label = "Export",
        description = "Export namelist",
        bpy_prop = BoolProperty,
        other = {"default":False},
    ),
)

## Sections

BFSection(
    name = "General configuration",
    bf_namelists = bf_namelists["HEAD","REAC"],
)

BFSection(
    name = "Boundary conditions",
    bf_namelists = bf_namelists["SURF"],
)

BFSection(
    name = "Geometry",
    bf_namelists = bf_namelists["OBST","HOLE"],
)
