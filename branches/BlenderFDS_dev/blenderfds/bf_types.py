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
#  You should have received a copy of the GNU General Public LicenseB
#  along with this program; if not, write to the Free Software Foundation,
#  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# ##### END GPL LICENSE BLOCK #####
"""BlenderFDS, specific types"""

import bpy

from .bf_basic_types import BFItem, BFList, BFResult, BFError
from .bf_osd import bf_osd
from . import bf_format, bf_config

### General functions

def _isiterable(var):
    """Check if var is iterable or not"""
    # A str is iterable in Py... not what I want
    if isinstance(var,str): return False
    # Let's try and fail nicely
    try: var[0]
    except TypeError: return False
    return True

def _check_items(names,type_):
    """Return a BFList of elements named names of type type_"""
    if isinstance(names,str): return BFList((type_.bf_list[names],))
    if isinstance(names,(tuple,list)): return BFList(type_.bf_list[names])
    return BFList()

def _check_item(name,type_):
    """Return the element named p of type t or None"""
    if name: return type_.bf_list[name]
    else: return None

### Classes

class BProp(BFItem):
    """Wrapper of a generic Blender property

    name -- unique name, eg "bf_fyi"
    b_name -- Blender property name, eg "bf_fyi" or "name".
    label -- label displayed by Blender UI, eg "FYI".
    has_ui -- True/False if it has an automatic UI
    description -- help text displayed by Blender UI, eg "For your information"
    bpy_prop -- Blender property, eg bpy.props.StringProperty. If None, refer to existing Blender property
    other -- optional dict of additional Blender property settings, eg {"maxlen":60, "default"="OBST"}
    precision -- (readonly) precision of FloatProperty
    operator -- optional name of a related operator shown in the UI
    """
    bf_list = BFList() # new class BFList, not that from BFItem
    
    def __init__(self,name,b_name=None,has_ui=True,label=None,description=None,bpy_prop=None,precision=None,other=None,operator=None):
        print("BlenderFDS: BProp.init:{}".format(name))
        BFItem.__init__(self,name=name)
        self.b_name = b_name or name
        self.has_ui = has_ui
        self.label = label or name
        self.description = description or label or name
        self.bpy_prop = bpy_prop
        self.other = other or dict()
        if precision is not None: self.other["precision"] = precision
        self.operator = operator

    def register(self,bpy_type):
        """Register Blender property in Blender bpy_type"""
        print("BlenderFDS: > > BProp.register: {}".format(self.name))
        # Try to register self
        if self.bpy_prop:
            try: setattr(bpy_type,self.b_name,self.bpy_prop(name=self.label,description=self.description,**self.other))
            except (ValueError,TypeError): print("{} failed!".format(self.name))

    def unregister(self,bpy_type):
        """Unregister Blender property in Blender bpy_type"""
        print("BlenderFDS: > > BProp.unregister: {}".format(self.name))
        # Try to unregister self
        if self.bpy_prop:
            try: delattr(bpy_type,self.b_name)
            except AttributeError: print("{} failed!".format(self.name))

    def value(self,context,element):
        """Return self value for element"""
        return getattr(element,self.b_name)

    def _get_precision(self):
        """Get self precision for element"""
        return self.other.get("precision",2)

    precision = property(_get_precision)
    
class BFHavingUI():
    """Auxiliary class containing methods for types that have an UI

    f_name -- FDS parameter name, eg "ID". If None, this BFParam is not automatically exported to FDS.
    b_prop_export -- BProp, set a customized export flag for this parameter.
    b_default_export -- bool, set an automatic export flag for this parameter.
    """
    def __init__(self,f_name=None,b_props=None,b_prop_export=None,b_default_export=False):
        self.f_name = f_name
        self.b_props = _check_items(b_props,BProp)
        self.b_prop_export = _check_item(b_prop_export,BProp)
        if b_default_export:
            try: name = "{}_export".format(self.b_props[0].name) # use b_props[0] name
            except IndexError: name = "bf_{}_export".format(self.name.lower()) # use lowercase self name
            self.b_prop_export = BProp(
                name = name,
                label = "Export",
                description = "Export to FDS",
                bpy_prop = bpy.props.BoolProperty,
                other = {"default":False},
                )

    def _is_exported(self,context,element):
        """If self with element is going to be exported return True, else False"""
        if self.f_name:
            if self.b_prop_export: return self.b_prop_export.value(context,element)
            return True
        return False

    def _has_active_ui(self,context,element):
        """If self with element has an active user interface return True, else False"""
        if self.b_prop_export: return self.b_prop_export.value(context,element)
        return True

    def _draw_error(self,context,element,layout):
        """Draw errors and messages for element in the provided layout."""
        try: res = self.evaluate(context,element)
        except BFError as err: err.draw(layout)
        else: res and res.draw(layout) # check res existence before...    

class BFParam(BFItem,BFHavingUI):
    """BlenderFDS parameter, interface between Blender properties and FDS parameter.

    name -- unique name, eg "REAC_Custom"
    f_name -- FDS parameter name, eg "ID". If None, this BFParam is not automatically exported to FDS.
    b_props -- Collection of BProp or single BProp or None.
               If None, this BFParam has no value by itself.
               By default BFParam.value(context,element) is taken from b_props[0]
    b_prop_export -- see BFHavingUI class
    b_default_export -- see BFHavingUI class
    bf_list -- (class attribute) a BFList containing all created BFParam objects
    """
    bf_list = BFList() # new class BFList, not that from BFItem

    def __init__(self,name,f_name=None,b_props=None,b_prop_export=None,b_default_export=False):
        print("BlenderFDS: BFParam.init: {}".format(name))
        BFItem.__init__(self,name=name)
        BFHavingUI.__init__(self,f_name=f_name,b_props=b_props,b_prop_export=b_prop_export,b_default_export=b_default_export)

    def register(self,bpy_type):
        """Register corresponding Blender properties in Blender bpy_type"""
        print("BlenderFDS: > BFParam.register: {}".format(self.name))
        # First, register self.b_props
        for b_prop in self.b_props: b_prop.register(bpy_type)
        # Then register self.b_prop_export
        if self.b_prop_export: self.b_prop_export.register(bpy_type)

    def unregister(self,bpy_type):
        """Unregister corresponding Blender properties in Blender bpy_type"""
        print("BlenderFDS: > BFParam.unregister: {}".format(self.name))
        # First, unregister self.b_props
        for b_prop in self.b_props: b_prop.unregister(bpy_type)
        # Then unregister self.b_prop_export
        if self.b_prop_export: self.b_prop_export.unregister(bpy_type)

    def _check(self,value):
        """Check value. Raise BFError if not good."""
        if isinstance(value,str):
            if "'" in value or '"' in value or "`" in value or "“" in value or "”" in value or "‘" in value or "’‌" in value:
                raise BFError(sender=self,msg="Quote characters not allowed")

    def _value(self,context,element):
        """Get self value. Raise BFError if not good."""
        try: value = self.b_props[0].value(context,element)
        except KeyError: value = None # Eg: no self.b_props
        self._check(value)
        return value
    
    def evaluate(self,context,element):
        """Check self for errors and return a BFResult or None.
        Raise a BFError exception if self cannot return a valid result.
        """
        return BFResult(sender=self,value=self._value(context,element),msg="test msg from "+self.name)

    def _get_precision(self):
        """Get self precision."""
        return self.b_props[0].precision

    precision = property(_get_precision)

    def _format(self,value):
        """Format self for to_fds, return a string"""
        # Insert value into an iterable, if it's not already
        if not _isiterable(value): value = tuple((value,))
        # Check each element of the iterable
        if isinstance(value[0],bool): value = ",".join(item and ".TRUE." or ".FALSE." for item in value)
        elif isinstance(value[0],int): value = ",".join(str(item) for item in value)
        elif isinstance(value[0],float): value = ",".join("{:.{}f}".format(item,self.precision) for item in value)
        else: value = ",".join("'{}'".format(item) for item in value)
        return "=".join((self.f_name,value))

    def to_fds(self,context,element):
        """Export self in FDS notation for element. Return a BFResult() or None."""
        print("BlenderFDS: > > > > BFParam.to_fds: {}".format(self.name))
        # Check
        if not self._is_exported(context,element): return None
        # Export
        res = self.evaluate(context,element) # Do not trap exceptions, pass them upward
        if res is None or res.value is None: return res # Could have msgs
        if not self.f_name: return res # Eg: custom fields
        # res.value from self.evaluate() is used then replaced with new content
        res.value = self._format(res.value)
        return res

    def _get_layout_export(self,context,element,layout):
        """Prepare double-column Blender panel layout for element if self has a b_prop_export."""
        if self.b_prop_export:
            # Set two distinct colums: layout_export and layout_ui
            row = layout.row()
            layout_export, layout = row.column(), row.column()
            layout_export.prop(element,self.b_prop_export.b_name,text="")
            layout.active = self._has_active_ui(context,element)
        return layout

    def _draw_b_props(self,context,element,layout):
        """Draw self b_props for element in the layout."""
        for b_prop in self.b_props:
            if b_prop.has_ui:
                row = layout.row()
                row.prop(element,b_prop.b_name,text=b_prop.label)
                if b_prop.operator: row.operator(b_prop.operator)

    def _draw_extra(self,context,element,layout):
        """Draw extra customized widgets for element in the layout"""
        pass
        
    def draw(self,context,element,layout):
        """Draw Blender panel widgets for self in the provided layout."""
        layout = self._get_layout_export(context,element,layout)
        self._draw_b_props(context,element,layout)
        self._draw_extra(context,element,layout)
        self._draw_error(context,element,layout)

class BFHavingChildren():
    """Parent type containing methods for types that have children"""
    def _to_fds_children(self,children,context,element=None):
        """Pile values, msgs, reraise piled errors from children instances
        
        children -- list of concerned children having the to_fds() method
        context -- Blender context
        element -- If needed, reference element used for to_fds
        """
        # Prepare piles and loop
        values, msgs, errors = list(), list(), list()
        for child in children:
            # Get FDS representation of children
            try: child_res = child.to_fds(context=context,element=element or self)
            # Pile errors if any
            except BFError as err:
                errors.extend(err.labels)
                continue
            # Check and append value and msgs
            if child_res is None: continue
            if child_res.msgs: msgs.extend(child_res.labels)
            if not child_res.value: continue
            values.append(child_res.value)
        # Rise the piled errors or return
        if errors: raise BFError(sender=self,msgs=errors)
        return values, msgs

class BFNamelist(BFItem,BFHavingUI,BFHavingChildren):
    """BlenderFDS namelist, interface between Blender object and FDS namelist.

    name -- unique name, eg "REAC"
    label -- label displayed by Blender UI, eg "REAC". If None, no automatic user interface
    description -- help text displayed by Blender UI, eg "Reaction"
    f_name -- FDS parameter name, eg "REAC". If None, no automatic exporting to FDS.
    bpy_type -- One in bpy.types.Scene or bpy.types.Object or bpy.types.Material.
    bf_params -- Collection of at least one BFParam related FDS parameters.
    b_prop_export -- see BFHavingUI class
    b_default_export -- see BFHavingUI class
    bf_list -- (class attribute) a BFList containing all created BFNamelist objects
    """
    bf_list = BFList() # new class BFList, not that from BFItem

    def __init__(self,name,bpy_type,bf_params=None,label=None,description=None,f_name=None,b_prop_export=None,b_default_export=False):
        print("BlenderFDS: BFNamelist.init: {}".format(name))
        BFItem.__init__(self,name=name)
        BFHavingUI.__init__(self,f_name=f_name,b_prop_export=b_prop_export,b_default_export=b_default_export)
        self.label = label or name
        self.description = description
        self.bpy_type = bpy_type
        self.bf_params = _check_items(bf_params,BFParam)

    def register(self):
        """Register corresponding Blender properties"""
        print("BlenderFDS: BFNamelist.register: {}".format(self.name))
        # Register self.bf_params and self.b_prop_export
        for bf_param in self.bf_params: bf_param.register(self.bpy_type)
        if self.b_prop_export: self.b_prop_export.register(self.bpy_type)
        
    def register_panel(self):
        """Register corresponding Blender panel"""
        print("BlenderFDS: BFNamelist.register_panel: {}".format(self.name))
        if self.bpy_type == bpy.types.Scene:
            panel_name = "SCENE_PT_bf_{}".format(self.name.replace(' ', '')) # FIXME safer method!
            bpy.utils.register_class(type(panel_name,(SceneButtonsPanel,bpy.types.Panel,), {"nl":self.name}))
        if self.bpy_type == bpy.types.Object:
            panel_name = "OBJECT_PT_bf_common" # FIXME works?
            bpy.utils.register_class(type(panel_name,(ObjectButtonsPanel,bpy.types.Panel,), {}))        
        if self.bpy_type == bpy.types.Material:
            panel_name = "MATERIAL_PT_bf_common" # FIXME works?
            bpy.utils.register_class(type(panel_name,(MaterialButtonsPanel,bpy.types.Panel,), {}))        
                
    def unregister(self):
        """Unregister corresponding Blender properties"""
        print("BlenderFDS: BFNamelist.unregister: {}".format(self.name))
        # Unregister self.bf_params and self.b_prop_export
        for bf_param in self.bf_params: bf_param.unregister(self.bpy_type)
        if self.b_prop_export: self.b_prop_export.unregister(self.bpy_type)

    def unregister_panel(self):
        """Unregister corresponding Blender panel"""
        print("BlenderFDS: BFNamelist.unregister_panel: {}".format(self.name))
        # TODO

    def _check(self,value):
        """Check value. Raise BFError if not good."""
        pass    

    def _value(self,context,element):
        """Get self value. Raise BFError if not good."""
        value = None
        self._check(value)
        return value

    def evaluate(self,context,element):
        """Check self for errors and return a BFResult or None.
        Raise a BFError exception if self cannot return a valid result.
        """
        return BFResult(sender=self,value=self._value(context,element),msg="test msg from "+self.name)

    def _format(self,context,element,values,msgs):
        """Format self for to_fds, return a string"""
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
        return "".join((bf_format.format_namelist_title(element.name),bf_format.format_comment(msgs),namelist))

    def to_fds(self,context,element):
        """Export self in FDS notation for element. Return a BFResult() or None."""
        print("BlenderFDS: > > > BFNamelist.to_fds: {}".format(self.name))
        # Check
        if not self._is_exported(context,element): return None
        # Export
        res = self.evaluate(context,element) # Do not trap exceptions, pass them upward
        children_values, children_msgs = self._to_fds_children(children=self.bf_params,context=context,element=element)
        # res.value from self.evaluate() is used then replaced with new content
        res.value = self._format(context,element,children_values,children_msgs)
        return res

    def draw_header(self,context,element,layout):
        """Draw Blender panel header for element in the provided layout."""
        if self.b_prop_export: layout.prop(element,self.b_prop_export.b_name,text="")
        if self.description: return "BlenderFDS {} ({})".format(self.label,self.description)
        return "BlenderFDS {}".format(self.label)
    
    def _get_layout_export(self,context,element,layout):
        """Prepare Blender panel layout if self has a b_prop_export."""
        layout.active = self._has_active_ui(context,element)
        return layout

    def _draw_bf_params(self,context,element,layout):
        """Draw self bf_params for element in the layout."""
        for bf_param in self.bf_params:
            bf_param.draw(context,element,layout)

    def _draw_extra(self,context,element,layout):
        """Draw extra customized widgets for element in the layout"""
        pass

    def draw(self,context,element,layout):
        """Draw Blender panel for element in the layout"""
        layout = self._get_layout_export(context,element,layout)
        self._draw_extra(context,element,layout)
        self._draw_error(context,element,layout)
        self._draw_bf_params(context,element,layout)
        self._draw_extra(context,element,layout)

class BFSection(BFItem,BFHavingChildren):
    """BlenderFDS section, used for grouping FDS namelists.

    name -- unique name, eg "Geometry"
    bf_namelists -- Collection of at least one BFNamelist
    bf_list -- (class attribute) a BFList containing all created BFSection objects
    """
    bf_list = BFList() # new class BFList, not that from BFItem

    def __init__(self,name,bf_namelists=None):
        print("BlenderFDS: BFSection.init: {}".format(name))
        BFItem.__init__(self,name)
        self.bf_namelists = _check_items(bf_namelists,BFNamelist)

    def _is_exported(self,*args,**kwargs): # *args and **kwargs are for compatibility with other classes
        """If self is going to be exported return True, else False"""
        return True

    def evaluate(self,*args,**kwargs): # *args and **kwargs are for compatibility with other classes
        """Check self for errors and return a BFResult or None.
        Raise a BFError exception if self cannot return a valid result.
        """
        return BFResult(sender=self,value=None,msg="test msg from "+self.name)

    def _format(self,values,msgs):
        """Format self, return a string"""
        return "".join((bf_format.format_section_title(self.name),bf_format.format_comment(msgs),"\n",bf_format.format_body(values),"\n"))

    def to_fds(self,context,*args,**kwargs): # *args and **kwargs are for compatibility with other classes
        """Export self in FDS notation. Return a BFResult() or None."""
        print("BlenderFDS: > BFSection.to_fds: {}".format(self.name))
        # Check
        if not self._is_exported(): return None
        # Get concerned bpy_types
        bpy_types = set(bf_namelist.bpy_type for bf_namelist in self.bf_namelists)
        # Get concerned children: Blender scenes, objects, materials
        children = list()
        if bpy.types.Scene in bpy_types: children.append(context.scene) # Get scene
        if bpy.types.Object in bpy_types:
            children.extend(ob for ob in context.scene.objects \
                if ob.type == "MESH" and ob.bf_namelist_export and ob.bf_namelist in self.bf_namelists) # Get objects
        if bpy.types.Material in bpy_types:
            children.extend(ma for ma in bpy.data.materials \
                if ma.bf_namelist_export and (ma.name not in bf_config.predefined_material_names)) # Get materials
        # Alphabetic order by element name
        children.sort(key=lambda k:k.name)
        # Export
        res = self.evaluate() # Do not trap exceptions, pass them upward
        children_values, children_msgs = self._to_fds_children(children=children,context=context)
        # res.value from self.evaluate() is used then replaced with new content
        res.value = self._format(children_values,children_msgs)
        return res

class BFFile(BFItem,BFHavingChildren):
    """BlenderFDS file, used for generating FDS file."""
    bf_list = BFList() # new class BFList, not that from BFItem

    def __init__(self,name):
        print("BlenderFDS: BFFile.init: {}".format(name))
        BFItem.__init__(self,name)

    def _is_exported(self,*args,**kwargs): # *args and **kwargs are for compatibility with other classes
        """If self is going to be exported return True, else False"""
        return True

    def evaluate(self,*args,**kwargs): # *args and **kwargs are for compatibility with other classes
        """Check self for errors and return a BFResult or None.
        Raise a BFError exception if self cannot return a valid result.
        """
        return BFResult(sender=self,value=None,msg="test msg from "+self.name)

    def _format(self,values,msgs,error=False):
        """Format self, return a string"""
        if error: return "".join((bf_format.format_file_title(),"\n",bf_format.format_section_title("ERRORS"),bf_format.format_comment(msgs),))
        else: return "".join((bf_format.format_file_title(),bf_format.format_comment(msgs),"\n",bf_format.format_body(values),"\n&TAIL /"))

    def to_fds(self,context,*args,**kwargs):
        """Export self in FDS notation. Return a BFResult() or None."""
        print("BlenderFDS: BFFile.to_fds")
        # Check
        if not self._is_exported(): return None
        # Manage error to produce output file
        try:
            res = self.evaluate()
            children_values, children_msgs = self._to_fds_children(children=bf_sections,context=context) 
        except BFError as err:
            return BFResult(sender=self,value=self._format(None,err.labels,True))
        else:
            # res.value from self.evaluate() is used then replaced with new content
            res.value = self._format(children_values,children_msgs)
            return res

### Global names

b_props = BProp.bf_list
bf_params = BFParam.bf_list
bf_namelists = BFNamelist.bf_list
bf_sections = BFSection.bf_list
bf_file = BFFile("Case")

### Class bpy.types.*, add methods

def bpy_types_get_bf_namelists(self):
    """Get BFNamelist related to self. Return a tuple() of BFNamelist."""
    if hasattr(self,"bf_namelist"):
        # This is an element that has an only BFNamelist
        return tuple((bf_namelists[self.bf_namelist],))
    else:
        # This is an object that has many BFNamelist
        return tuple(bf_namelist for bf_namelist in bf_namelists if bf_namelist.bpy_type == self.__class__)

def bpy_types_get_bf_params(self):
    """Get BFParam related to self. Return a tuple() of BFParam."""
    return BFList(set(bf_param for bf_namelist in self.get_bf_namelists() for bf_param in bf_namelist.bf_params))

def bpy_types_has_bf_param(self,bf_param):
    """Check if self has bf_param parameter"""
    bf_namelists = self.get_bf_namelists()
    return bf_param in (bf_param for bf_namelist in bf_namelists for bf_param in bf_namelist.bf_params)

def bpy_types_to_fds(self,context=None,element=None): # element is kept for compatibility, self is the element itself.
    """Export self to FDS notation. Return a BFResult() or None."""
    print("BlenderFDS: > > {}.to_fds: {}".format(self.__class__.__name__,self.name))
    bf_osd.show("BlenderFDS exporting {} {}".format(self.__class__.__name__,self.name))
    if not context: context = bpy.context
    # Get children: get values and msgs for each of my BFNamelist
    values, msgs = self._to_fds_children(children=self.get_bf_namelists(),context=context,element=self)
    return BFResult(sender=self,value=bf_format.format_body(values),msgs=msgs)

# Assign methods to Scene, Object, and Material
bpy.types.Scene._to_fds_children = BFHavingChildren._to_fds_children
bpy.types.Scene.get_bf_namelists = bpy_types_get_bf_namelists
bpy.types.Scene.get_bf_params = bpy_types_get_bf_params
bpy.types.Scene.has_bf_param = bpy_types_has_bf_param
bpy.types.Scene.to_fds = bpy_types_to_fds

bpy.types.Object._to_fds_children = BFHavingChildren._to_fds_children
bpy.types.Object.get_bf_namelists = bpy_types_get_bf_namelists
bpy.types.Object.get_bf_params = bpy_types_get_bf_params
bpy.types.Object.has_bf_param = bpy_types_has_bf_param
bpy.types.Object.to_fds = bpy_types_to_fds

bpy.types.Material._to_fds_children = BFHavingChildren._to_fds_children
bpy.types.Material.get_bf_namelists = bpy_types_get_bf_namelists
bpy.types.Material.get_bf_params = bpy_types_get_bf_params
bpy.types.Material.has_bf_param = bpy_types_has_bf_param
bpy.types.Material.to_fds = bpy_types_to_fds

### Generic panels

# Scene panel

class SceneButtonsPanel():
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "scene"
    bl_label = "FDS Scene"
    nl = None

    def draw_header(self,context):
        layout = self.layout
        element = context.scene
        nl = type(self).nl # access Class variable
        self.bl_label = bf_namelists[nl].draw_header(context,element,layout)

    def draw(self,context):
        layout = self.layout
        element = context.scene
        nl = type(self).nl # access Class variable
        bf_namelists[nl].draw(context,element,layout)

# Object panel

class ObjectButtonsPanel():
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "object"
    bl_label = "FDS Object"
    
    @classmethod
    def poll(cls,context):
        ob = context.active_object
        return ob and ob.type == "MESH"

    def draw_header(self,context):
        layout = self.layout
        element = context.active_object
        nl = element.bf_namelist     
        self.bl_label = bf_namelists[nl].draw_header(context,element,layout)

    def draw(self,context):
        layout = self.layout
        element = context.active_object
        nl = element.bf_namelist
        bf_namelists[nl].draw(context,element,layout)

# Material panel

class MaterialButtonsPanel():
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "material"
    bl_label = "FDS Material"

    @classmethod    
    def poll(cls,context):
        ma = context.material
        ob = context.active_object
        return ma and ob and ob.type == "MESH" and "SURF_ID" in ob.get_bf_params() and not ob.bf_is_voxels

    def draw_header(self,context):
        layout = self.layout
        element = context.material
        nl = element.bf_namelist
        self.bl_label = bf_namelists[nl].draw_header(context,element,layout)

    def draw(self,context):
        layout = self.layout
        element = context.material
        nl = element.bf_namelist
        bf_namelists[nl].draw(context,element,layout)
