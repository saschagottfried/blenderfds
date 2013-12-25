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
"BlenderFDS, an open tool for the NIST Fire Dynamics Simulator."

# Max number of colums for output file
col_max = 80

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

# Predefined FDS SURF boundary conditions (Blender materials)
mas_predefined = ("OPEN", "MIRROR", "INERT")
# They are created by the hardcoded operator:
# class MATERIAL_OT_bf_create_predefined(bpy.types.Operator):
