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
"""BlenderFDS, specific types"""

from .bf_basic_types import BFObject, BFItem, BFList, BFResult, BFError
from .bf_format import *
from . import bf_config

import bpy

### General functions

def _check_type(p,t):
    """If p is not an instance of t raise an exception, else return p"""
    if isinstance(p,t) or p is None: return p
    raise ValueError("'{0}' type not allowed, the good type is '{1}'".format(p,t))

def _check_bf_list_of_type(p,t):
    """If p is not an instance of t or a list of t raise an exception, else return BFList(p)"""
    if isinstance(p,(tuple,list)):
        for item in p: _check_type(item,t)
        return BFList(p)
    elif p is None: return None
    return BFList((_check_type(p,t),))

### Classes

class BProp(BFObject):
    """Wrapper of several Blender properties, registered in one or more Blender bpy.types (Scene, Object, Material)

    name -- unique name, eg "bf_fyi"
    label -- label displayed by Blender UI, eg "FYI". If None, not automatically displayed in panels.
    description -- help text displayed by Blender UI, eg "For your information"
    bpy_prop -- Blender property, eg bpy.props.StringProperty
    other -- dict of other Blender property settings, eg {"maxlen":60, "default"="OBST"}
    """
    def __init__(self,name,label=None,description=None,bpy_prop=None,other=None):
        print("BlenderFDS: BProp.init:",name)
        BFObject.__init__(self,name)
        self.label = label
        self.description = description or self.label or self.name
        self.bpy_prop = bpy_prop
        self.other = other

    def register(self,bpy_type):
        """Registers Blender property in Blender bpy_type"""
        print("BlenderFDS: > > BProp.register:",self.name)
        # Try to register self
        if self.bpy_prop:
            try: setattr(bpy_type,self.name,self.bpy_prop(name=self.label or self.name,description=self.description,**self.other))
            except (ValueError,TypeError): pass

    def unregister(self,bpy_type):
        """Unregisters Blender property in Blender bpy_type"""
        print("BlenderFDS: > > BProp.unregister:",self.name)
        # Try to unregister self
        if self.bpy_prop:
            try: delattr(bpy_type,self.name)
            except AttributeError: pass

    def value(self,context,element):
        """Get self value for element"""
        return getattr(element,self.name,None)

    def has_ui(self,context,element):
        """If self shall have its user interface return True, else False"""
        if self.label: return True
        return False

class BFHavingUI():
    """Auxiliary class containing methods for types that have an UI

    f_name -- FDS parameter name, eg "ID". If None, this BFParam is not automatically exported to FDS.
    b_prop_export -- BProp or None.
                     If None, BFParam is always exported.
    """
    def __init__(self,f_name=None,b_prop_export=None):
        self.f_name = f_name
        self.b_prop_export = _check_type(b_prop_export,BProp)
        
    def is_exported(self,context,element):
        """If self with element is going to be exported return True, else False"""
        if self.f_name:
            if self.b_prop_export: return self.b_prop_export.value(context,element)
            return True
        return False

    def has_active_ui(self,context,element):
        """If self with element has an active user interface return True, else False"""
        if self.b_prop_export: return self.b_prop_export.value(context,element)
        return True

class BFHavingChildren():
    """Parent type containing methods for types that have children"""
    def _get_children(self,children,context,element=None):
        """Pile values, msgs, reraise piled errors from children instances
        
        children -- list of concerned children having the to_fds() method
        context -- Blender context
        element -- If needed, reference element used for to_fds
        """
        # Prepare piles and loop
        values, msgs, errors = list(), list(), list()
        for child in children:
            # Get FDS representation of children
            try: result = child.to_fds(context=context,element=element or self)
            # Pile errors if any
            except BFError as err:
                errors.extend(err.labels)
                continue
            # Check and append value and msgs
            if result is None: continue
            if result.msgs: msgs.extend(result.labels)
            if not result.value: continue
            values.append(result.value)
        # Rise the piled errors or return
        if errors: raise BFError(sender=self,msgs=errors)
        return values, msgs

class BFParam(BFItem,BFHavingUI):
    """BlenderFDS parameter, interface between Blender property and FDS parameter.

    name -- unique name, eg "REAC_Custom"
    f_name -- FDS parameter name, eg "ID". If None, this BFParam is not automatically exported to FDS.
    b_props -- Collection of BProp or single BProp or None.
               If None, this BFParam has no value by itself.
               By default BFParam.value(context,element) is taken from b_props[0]
    b_prop_export -- BProp or None.
                     If None, BFParam is always exported.
    bf_list -- (class attribute) a BFList containing all created BFParam objects
    """
    bf_list = BFList() # new class BFList, not that from BFItem

    def __init__(self,name,f_name=None,b_props=None,b_prop_export=None):
        print("BlenderFDS: BFParam.init:",name)
        BFItem.__init__(self,name)
        BFHavingUI.__init__(self,f_name,b_prop_export)
        self.b_props = _check_bf_list_of_type(b_props,BProp)

    def register(self,bpy_type):
        """Register corresponding Blender properties in Blender bpy_type"""
        print("BlenderFDS: > BFParam.register:",self.name)
        # First, register self.b_props
        if self.b_props:
            for b_prop in self.b_props: b_prop.register(bpy_type)
        # Then register self.b_prop_export
        if self.b_prop_export: self.b_prop_export.register(bpy_type)

    def unregister(self,bpy_type):
        """Unregister corresponding Blender properties in Blender bpy_type"""
        print("BlenderFDS: > BFParam.unregister:",self.name)
        # First, unregister self.b_props
        if self.b_props:
            for b_prop in self.b_props: b_prop.unregister(bpy_type)
        # Then unregister self.b_prop_export
        if self.b_prop_export: self.b_prop_export.unregister(bpy_type)

    def value(self,context,element):
        """Return self value. Raise a BFError exception if self cannot export a valid value"""
        # Check if self has b_props and get the default value
        if getattr(self,"b_props",None) is None: return None
        value = self.b_props[0].value(context,element)
        # Generic check of strings: no quote characters!
        if isinstance(value,str):
            if not value:
                raise BFError(sender=self,msg="Empty string")
            if "'" in value or '"' in value or "`" in value or "“" in value or "”" in value or "‘" in value or "’‌" in value:
                raise BFError(sender=self,msg="Quote characters not allowed")
        # Return the checked value
        return value

    def draw(self,context,element,layout):
        """Draw one Blender panel row of Blender element in the provided layout."""
        if self.b_prop_export:
            # Set two distinct colums: layout_export and layout_ui
            row = layout.row()
            layout_export, layout_ui = row.column(), row.column()
            layout_export.prop(element,self.b_prop_export.name,text="")
            layout_ui.active = self.has_active_ui(context,element)
        else:
            # Set one column only: layout_ui
            layout_ui = layout
        if self.b_props:
            # Has b_props, show them
            for b_prop in self.b_props:
                if b_prop.has_ui(context,element):
                    row = layout_ui.row()
                    row.prop(element,b_prop.name,text=b_prop.label)
            # Show exceptions
            if layout.active and layout_ui.active:
                try: self.value(context,element)
                except BFError as err: err.draw(layout_ui)
        else:
            # No b_props, show what self is going to export (eg SURF_ID='Material')
            try: f_result = self.to_fds(context=context,element=element) # trap exception, ui must go on!
            except BFError as err: err.draw(layout_ui)
            else:
                row = layout_ui.row()
                row.label(text=getattr(f_result,"value",None) or "{} is not set.".format(self.name))

    def to_fds(self,context,element):
        """Export self in FDS notation for element. Return a BFResult() or None."""
        print("BlenderFDS: > > > > BFParam.to_fds:",self.name)
        if not self.is_exported(context,element): return None
        return BFResult(sender=self,value=format_bf_param(self.f_name,self.value(context,element))) # Do not trap exceptions, pass them upstair

class BFNamelist(BFItem,BFHavingUI,BFHavingChildren):
    """BlenderFDS namelist, interface between Blender object and FDS namelist.

    name -- unique name, eg "REAC"
    label -- label displayed by Blender UI, eg "REAC". If None, no automatic user interface
    description -- help text displayed by Blender UI, eg "Reaction"
    f_name -- FDS parameter name, eg "REAC". If None, no automatic exporting to FDS.
    bpy_type -- One in bpy.types.Scene or bpy.types.Object or bpy.types.Material.
    bf_params -- Collection of at least one BFParam related FDS parameters.
    b_prop_export -- BProp that drives BFParam export or None.
                     If None, BFNamelist is always exported.
    bf_list -- (class attribute) a BFList containing all created BFNamelist objects
    """
    bf_list = BFList() # new class BFList, not that from BFItem

    def __init__(self,name,bpy_type,bf_params,label=None,description=None,f_name=None,b_prop_export=None):
        print("BlenderFDS: BFNamelist.init:",name)
        BFItem.__init__(self,name)
        BFHavingUI.__init__(self,f_name,b_prop_export)
        self.label = label or name
        self.description = description or self.label
        self.bpy_type = bpy_type in (bpy.types.Scene, bpy.types.Object, bpy.types.Material) and bpy_type
        self.bf_params = _check_bf_list_of_type(bf_params,BFParam)

    def register(self):
        """Register corresponding Blender properties"""
        print("BlenderFDS: BFNamelist.register:",self.name)
        # Register self.bf_params and self.b_prop_export
        for bf_param in self.bf_params: bf_param.register(self.bpy_type)
        if self.b_prop_export: self.b_prop_export.register(self.bpy_type)

    def unregister(self):
        """Unregister corresponding Blender properties"""
        print("BlenderFDS: BFNamelist.unregister:",self.name)
        # Unregister self.bf_params and self.b_prop_export
        for bf_param in self.bf_params: bf_param.unregister(self.bpy_type)
        if self.b_prop_export: self.b_prop_export.unregister(self.bpy_type)

    def draw_header(self,context,element,layout):
        """Draw Blender panel header of Blender element in the provided layout."""
        if self.b_prop_export: layout.prop(element,self.b_prop_export.name,text="")
        return "FDS {} ({})".format(self.label,self.description)

    def check(self,context,element):
        """Check self for errors"""
        pass

    def draw(self,context,element,layout):
        """Draw Blender panel of Blender element in the provided layout."""
        layout.active = self.has_active_ui(context,element)
        # Self check
        try: self.check(context,element)
        except BFError as err: err.draw(layout)
        # Show self parameters
        for bf_param in self.bf_params:
            bf_param.draw(context,element,layout)

    def _format(self,context,element,values,msgs):
        # Extract one multivalues from values
        multivalues = None
        for i, value in enumerate(values):
            if isinstance(value,(list,tuple)):
                if multivalues: raise Exception("Double multivalues not previously detected in element:",element.name)
                multivalues = values.pop(i)
        # Format namelist body
        namelist = "".join(("&",self.f_name))
        margin = bf_config.right_margin_position
        for value in values:
            if not value: continue
            if len(namelist) + len(value) + 1 - namelist.rfind("\n") < margin: separator = " "
            else: separator = "\n      "
            namelist = separator.join((namelist,value))
        # Setup multivalues or close namelist, eg &OBST ID='example' XB=2.56,1.22,.../n&OBST ID='example' XB=2.56,1.22,.../n...
        if multivalues:
            if len(namelist) + len(multivalues[0]) + 1 - namelist.rfind("\n") < margin: separator = " "
            else: separator = "\n      "
            namelist = "".join("{}{}{} /\n".format(namelist,separator,value) for value in multivalues)
        else:
            namelist = "".join((namelist," /\n"))
        return "".join((format_namelist_title(element.name),format_comment(msgs),namelist))

    def to_fds(self,context,element):
        """Export self in FDS notation for element. Return a BFResult() or None."""
        print("BlenderFDS: > > > BFNamelist.to_fds:",self.name)
        # Check
        if not self.is_exported(context,element): return None
        self.check(context,element) # do not trap, send upstair 
        # Export
        values, msgs = self._get_children(children=self.bf_params,context=context,element=element)
        return BFResult(sender=self,value=self._format(context,element,values,msgs))

class BFSection(BFItem,BFHavingChildren):
    """BlenderFDS section, used for grouping FDS namelists.

    name -- unique name, eg "Geometry"
    bf_namelists -- Collection of at least one BFNamelist
    bf_list -- (class attribute) a BFList containing all created BFSection objects
    """
    bf_list = BFList() # new class BFList, not that from BFItem

    def __init__(self,name,bf_namelists=None):
        print("BlenderFDS: BFSection.init:",name)
        BFItem.__init__(self,name)
        self.bf_namelists = _check_bf_list_of_type(bf_namelists,BFNamelist)

    def _format(self,values,msgs):
        """Format self, return str()"""
        return "".join((format_section_title(self.name),format_comment(msgs),"\n",format_body(values),"\n"))

    def to_fds(self,context,*args,**kwargs):
        print("BlenderFDS: > BFSection.to_fds:",self.name)
        # Get concerned bpy_types
        bpy_types = set(bf_namelist.bpy_type for bf_namelist in self.bf_namelists)
        # Get concerned children: Blender scenes, objects, materials
        children = list()
        if bpy.types.Scene in bpy_types:
            children.append(context.scene)
        if bpy.types.Object in bpy_types:
            children.extend(ob for ob in context.scene.objects if ob.type == "MESH" and ob.bf_export and ob.bf_namelist in self.bf_namelists)
        if bpy.types.Material in bpy_types:
            children.extend(ob.active_material for ob in context.scene.objects if ob.bf_export and not ob.bf_is_voxels and ob.active_material and ob.active_material.bf_surf_export and (ob.active_material.name not in bf_config.mas_predefined))
        # Alphabetic order by element name
        children.sort(key=lambda k:k.name)
        # Export
        values, msgs = self._get_children(children=children,context=context)
        return BFResult(sender=self,value=self._format(values,msgs))

class BFFile(BFItem,BFHavingChildren):
    """BlenderFDS file, used for generating FDS file."""
    bf_list = BFList() # new class BFList, not that from BFItem

    def __init__(self,name):
        print("BlenderFDS: BFFile.init:",name)
        BFItem.__init__(self,name)

    def _format(self,values,msgs,error=False):
        """Format self, return str()"""
        if error: return "".join((format_file_title(),"\n",format_section_title("ERRORS"),format_comment(msgs),))
        else: return "".join((format_file_title(),format_comment(msgs),"\n",format_body(values),))

    def to_fds(self,context,*args,**kwargs):
        print("BlenderFDS: BFFile.to_fds")
        # Manage error to produce output file
        try: values, msgs = self._get_children(children=bf_sections,context=context) 
        except BFError as err:
            return BFResult(sender=self,value=self._format(None,err.labels,True))
        else:
            values.append("&TAIL /")
            return BFResult(sender=self,value=self._format(values,msgs))

# Global names

bf_namelists = BFNamelist.bf_list
bf_params = BFParam.bf_list
bf_sections = BFSection.bf_list
bf_file = BFFile("Case")

# class bpy.types.*, add methods
def bpy_types_get_bf_namelists(self):
    """Get BFNamelist related to self. Return a tuple() of BFNamelist."""
    if hasattr(self,"bf_namelist"):
        # This is an element that has an only BFNamelist
        return tuple((bf_namelists.get(self.bf_namelist,bf_namelists["Unknown"]),))
    else:
        # This is an object that has many BFNamelist
        return tuple(bf_namelist for bf_namelist in bf_namelists if bf_namelist.bpy_type == self.__class__)

def bpy_types_get_bf_params(self):
    """Get BFParam related to self. Return a tuple() of BFParam."""
    return BFList(set(bf_param for bf_namelist in self.get_bf_namelists() for bf_param in bf_namelist.bf_params))

def bpy_types_to_fds(self,context=None,element=None): # element is kept for compatibility, self is the element itself.
    """Export self to FDS notation. Return a BFResult()."""
    print("BlenderFDS: > > {}.to_fds:".format(self.__class__.__name__),self.name)
    if not context: context = bpy.context
    # Get children: get values and msgs for each of my BFNamelist
    values, msgs = self._get_children(children=self.get_bf_namelists(),context=context,element=self)
    return BFResult(sender=self,value=format_body(values),msgs=msgs)

bpy.types.Scene._get_children = BFHavingChildren._get_children
bpy.types.Scene.get_bf_namelists = bpy_types_get_bf_namelists
bpy.types.Scene.get_bf_params = bpy_types_get_bf_params
bpy.types.Scene.to_fds = bpy_types_to_fds

bpy.types.Object._get_children = BFHavingChildren._get_children
bpy.types.Object.get_bf_namelists = bpy_types_get_bf_namelists
bpy.types.Object.get_bf_params = bpy_types_get_bf_params
bpy.types.Object.to_fds = bpy_types_to_fds

bpy.types.Material._get_children = BFHavingChildren._get_children
bpy.types.Material.get_bf_namelists = bpy_types_get_bf_namelists
bpy.types.Material.get_bf_params = bpy_types_get_bf_params
bpy.types.Material.to_fds = bpy_types_to_fds
