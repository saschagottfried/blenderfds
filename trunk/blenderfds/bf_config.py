# ##### BEGIN GPL LICENSE BLOCK #####
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
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

# Version
version = "0.43 2010/04/28"

# Max number of colums for output file
col_max = 100

# Namelist names
nl_names = {"OBST": "Obstruction",
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
           }

# Namelist groups
# The last group is always used to collect all remaining namelist groups.
nl_groups = [("Computational domain", ("MESH", "INIT", "ZONE")),
             ("Geometry", ("OBST", "HOLE", "VENT")),
             ("Evacuation", ("EVAC","EVHO","EXIT","ENTR","DOOR","CORR","EVSS","STRS")),
             ("Control logic and output", ("DEVC", "SLCF", "PROF")),
             ("Others", ()),
            ]

# Namelist parameters
nl_params = {"SURF_ID":  ("DEVC", "OBST","VENT"),
             "SAWTOOTH": ("OBST", ),
             "IJK":      ("MESH", ),
             "XB":       ("DEVC", "HOLE", "INIT", "MESH", "OBST", "SLCF", "VENT", "ZONE", "EVAC", "EVHO", "EXIT", "ENTR", "DOOR", "CORR", "EVSS"),
             "XYZ":      ("DEVC", "PROF", "VENT", "EXIT", "DOOR", "STRS"),
             "PB":       ("SLCF", "VENT"),
            }

# Predefined FDS SURF boundary conditions (Blender materials)
mas_predefined = ["OPEN", "MIRROR", "INERT"]

# Return namelist description
def get_nl_description(nl_name):
    """Return namelist description"""
    return "FDS {} ({})".format(nl_name, nl_names.get(nl_name, "Unknown Object")) 

