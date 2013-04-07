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

from .bf_basic_types import BFListItem, BFList, BFResult, BFError
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

class BFProp(BFListItem):
    """Wrapper of a generic Blender property

    name -- unique name, eg "bf_fyi"
    label -- UI label, eg "FYI".
    description -- UI help text, eg "For your information"
    has_auto_ui -- True/False if it has an automatic UI
    bpy_name -- Blender property name, eg "bf_fyi" or "name".
    bpy_prop -- Blender property type, eg bpy.props.StringProperty. If None, refer to existing Blender property
    operator -- optional name of a related operator shown in the UI
    **kwargs -- other optional Blender properties
    precision -- (readonly) float precision
    """
    bf_list = BFList() # new class BFList, not that from BFListItem
    
    def __init__(self,name,label=None,description=None,has_auto_ui=True,operator=None,bpy_name=None,bpy_prop=None,**kwargs):
        print("BlenderFDS: BFProp.init:{}".format(name))
        BFListItem.__init__(self,name=name)
        self.label = label or name
        self.description = description or label or name
        self.has_auto_ui = has_auto_ui
        self.operator = operator
        self.bpy_name = bpy_name
        self.bpy_prop = bpy_prop
        self.bpy_other = kwargs

    def register(self,bpy_type):
        """Register Blender property in Blender bpy_type"""
        print("BlenderFDS: > > BFProp.register: {}".format(self.name))
        if self.bpy_prop: setattr(bpy_type,self.bpy_name,self.bpy_prop(name=self.label,description=self.description,**self.bpy_other))

    def unregister(self,bpy_type):
        """Unregister Blender property in Blender bpy_type"""
        print("BlenderFDS: > > BFProp.unregister: {}".format(self.name))
        # Try to unregister self
        try: delattr(bpy_type,self.bpy_name)
        except: pass

    def value(self,context,element):
        """Return self value for element"""
        return getattr(element,self.bpy_name)

    def _get_precision(self):
        """Get self precision for element"""
        return self.bpy_other.get("precision",2)

    precision = property(_get_precision)
    
class BFHavingFDSName():
    """Auxiliary class containing common methods for types that have an fds_name

    fds_name -- FDS parameter name, eg "ID". If None, self is not automatically exported to FDS.
    bf_prop_export -- BFProp, customized export flag for self.
    has_auto_export -- bool, set an automatic export flag for self.
    """
    def __init__(self,fds_name=None,bf_props=None,bf_prop_export=None,has_auto_export=False):
        self.fds_name = fds_name
        self.bf_props = _check_items(bf_props,BFProp)
        self.bf_prop_export = _check_item(bf_prop_export,BFProp)
        if has_auto_export:
            try: name = "{}_export".format(self.bf_props[0].name) # use bf_props[0] name
            except IndexError: name = "bf_{}_export".format(self.name.lower()) # use lowercase self name
            self.bf_prop_export = BFProp(
                name = name,
                label = "Export",
                description = "Export to FDS",
                bpy_prop = bpy.props.BoolProperty,
                default = False,
                )

    def _is_exported(self,context,element):
        """If self with element is going to be exported return True, else False"""
        if self.fds_name:
            if self.bf_prop_export: return self.bf_prop_export.value(context,element)
            return True
        return False

    def _has_active_ui(self,context,element):
        """If self with element has an active user interface return True, else False"""
        if self.bf_prop_export: return self.bf_prop_export.value(context,element)
        return True

    def _draw_error(self,context,element,layout):
        """Draw errors and messages for element in the provided layout."""
        try: res = self.evaluate(context,element)
        except BFError as err: err.draw(layout)
        else: res and res.draw(layout) # check res existence before...    

class BFParam(BFListItem,BFHavingFDSName):
    """BlenderFDS parameter, interface between Blender properties and FDS parameter.

    name -- unique name, eg "REAC_Custom"
    fds_name -- FDS parameter name, eg "ID". If None, this BFParam is not automatically exported to FDS.
    bf_props -- Collection of BFProp or single BFProp or None.
                If None, this BFParam has no value by itself.
                By default BFParam.value(context,element) is taken from bf_props[0]
    bf_prop_export -- see BFHavingFDSName class
    has_auto_export -- see BFHavingFDSName class
    bf_list -- (class attribute) a BFList containing all created BFParam objects
    """
    bf_list = BFList() # new class BFList, not that from BFListItem

    def __init__(self,name,fds_name=None,bf_props=None,bf_prop_export=None,has_auto_export=False):
        print("BlenderFDS: BFParam.init: {}".format(name))
        BFListItem.__init__(self,name=name)
        BFHavingFDSName.__init__(self,fds_name=fds_name,bf_props=bf_props,bf_prop_export=bf_prop_export,has_auto_export=has_auto_export)

    def register(self,bpy_type):
        """Register corresponding Blender properties in Blender bpy_type"""
        print("BlenderFDS: > BFParam.register: {}".format(self.name))
        # First, register self.bf_props
        for bf_prop in self.bf_props: bf_prop.register(bpy_type)
        # Then register self.bf_prop_export
        if self.bf_prop_export: self.bf_prop_export.register(bpy_type)

    def unregister(self,bpy_type):
        """Unregister corresponding Blender properties in Blender bpy_type"""
        print("BlenderFDS: > BFParam.unregister: {}".format(self.name))
        # First, unregister self.bf_props
        for bf_prop in self.bf_props: bf_prop.unregister(bpy_type)
        # Then unregister self.bf_prop_export
        if self.bf_prop_export: self.bf_prop_export.unregister(bpy_type)

    def _check(self,value):
        """Check value. Raise BFError if not good."""
        if isinstance(value,str):
            if "'" in value or '"' in value or "`" in value or "“" in value \
                or "”" in value or "‘" in value or "’‌" in value:
                raise BFError(sender=self,msg="Quote characters not allowed")

    def _value(self,context,element):
        """Get self value. Raise BFError if not good."""
        try: value = self.bf_props[0].value(context,element)
        except KeyError: value = None # Eg: no self.bf_props
        self._check(value)
        return value
    
    def evaluate(self,context,element):
        """Check self for errors and return a BFResult or None.
        Raise a BFError exception if self cannot return a valid result.
        """
        return BFResult(sender=self,value=self._value(context,element),msg="test msg from "+self.name)

    def _get_precision(self):
        """Get self precision."""
        return self.bf_props[0].precision

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
        return "=".join((self.fds_name,value))

    def to_fds(self,context,element):
        """Export self in FDS notation for element. Return a BFResult() or None."""
        print("BlenderFDS: > > > > BFParam.to_fds: {}".format(self.name))
        # Check
        if not self._is_exported(context,element): return None
        # Export
        res = self.evaluate(context,element) # Do not trap exceptions, pass them upward
        if res is None or res.value is None: return res # Could have msgs
        if not self.fds_name: return res # Eg: custom fields
        # res.value from self.evaluate() is used then replaced with new content
        res.value = self._format(res.value)
        return res

    def _get_layout_export(self,context,element,layout):
        """Prepare double-column Blender panel layout for element if self has a bf_prop_export."""
        if self.bf_prop_export:
            # Set two distinct colums: layout_export and layout_ui
            row = layout.row()
            layout_export, layout = row.column(), row.column()
            layout_export.prop(element,self.bf_prop_export.bpy_name,text="")
            layout.active = self._has_active_ui(context,element)
        return layout

    def _draw_bf_props(self,context,element,layout):
        """Draw self bf_props for element in the layout."""
        for bf_prop in self.bf_props:
            if bf_prop.has_auto_ui:
                row = layout.row()
                row.prop(element,bf_prop.bpy_name,text=bf_prop.label)
                if bf_prop.operator: row.operator(bf_prop.operator)

    def _draw_extra(self,context,element,layout):
        """Draw extra customized widgets for element in the layout"""
        pass
        
    def draw(self,context,element,layout):
        """Draw Blender panel widgets for self in the provided layout."""
        layout = self._get_layout_export(context,element,layout)
        self._draw_bf_props(context,element,layout)
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

class BFNamelist(BFListItem,BFHavingFDSName,BFHavingChildren):
    """BlenderFDS namelist, interface between Blender object and FDS namelist.

    name -- unique name, eg "REAC"
    unique_id -- integer, unique id, eg 345
    label -- label displayed by Blender UI, eg "REAC"
    description -- help text displayed by Blender UI, eg "Reaction"
    has_auto_ui -- True/False if it has an automatic UI
    fds_name -- FDS parameter name, eg "REAC". If None, no automatic exporting to FDS.
    bpy_type -- One in bpy.types.Scene or bpy.types.Object or bpy.types.Material. Defaults to Object.
    bf_params -- Collection of at least one BFParam related FDS parameters.
    bf_prop_export -- see BFHavingFDSName class
    has_auto_export -- see BFHavingFDSName class
    bf_list -- (class attribute) a BFList containing all created BFNamelist objects
    """
    bf_list = BFList() # new class BFList, not that from BFListItem

    def __init__(self,name,unique_id,label=None,description=None,fds_name=None,bpy_type=None,bf_params=None,bf_prop_export=None,has_auto_export=False):
        print("BlenderFDS: BFNamelist.init: {}".format(name))
        BFListItem.__init__(self,name=name)
        BFHavingFDSName.__init__(self,fds_name=fds_name,bf_prop_export=bf_prop_export,has_auto_export=has_auto_export)
        self.unique_id = unique_id # TODO test it is really unique
        self.label = label or name
        self.description = description or label or name
        self.bpy_type = bpy_type or bpy.types.Object
        self.bf_params = _check_items(bf_params,BFParam)

    def register(self):
        """Register corresponding Blender properties"""
        print("BlenderFDS: BFNamelist.register: {}".format(self.name))
        # Register self.bf_params and self.bf_prop_export
        for bf_param in self.bf_params: bf_param.register(self.bpy_type)
        if self.bf_prop_export: self.bf_prop_export.register(self.bpy_type)
        # Register panel
        if self.bpy_type == bpy.types.Scene:
            name = "SCENE_PT_bf_" + self.name.replace(' ', '_')
            self._b_panel = type(name,(ScenePanel,bpy.types.Panel,),{"nl":self.name})
        elif self.bpy_type == bpy.types.Object:
            self._b_panel = type("OBJECT_PT_bf_common",(ObjectPanel,bpy.types.Panel,),{})       
        elif self.bpy_type == bpy.types.Material:
            self._b_panel = type("MATERIAL_PT_bf_common",(MaterialPanel,bpy.types.Panel,),{})
        else: raise Exception("Unrecognized bpy_type in BFNamelist {}".format(self.name))    
        bpy.utils.register_class(self._b_panel)
        
    def unregister(self):
        """Unregister corresponding Blender properties"""
        print("BlenderFDS: BFNamelist.unregister: {}".format(self.name))
        # Unregister self.bf_params and self.bf_prop_export
        for bf_param in self.bf_params: bf_param.unregister(self.bpy_type)
        if self.bf_prop_export: self.bf_prop_export.unregister(self.bpy_type)
        # Unregister panel
        try: bpy.utils.unregister_class(self._b_panel)
        except: pass       

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
        namelist = "".join(("&",self.fds_name))
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
        if self.bf_prop_export: layout.prop(element,self.bf_prop_export.bpy_name,text="")
        if self.description: return "BlenderFDS {} ({})".format(self.label,self.description)
        return "BlenderFDS {}".format(self.label)
    
    def _get_layout_export(self,context,element,layout):
        """Prepare Blender panel layout if self has a bf_prop_export."""
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

class BFSection(BFListItem,BFHavingChildren):
    """BlenderFDS section, used for grouping FDS namelists.

    name -- unique name, eg "Geometry"
    bf_namelists -- Collection of at least one BFNamelist
    bf_list -- (class attribute) a BFList containing all created BFSection objects
    """
    bf_list = BFList() # new class BFList, not that from BFListItem

    def __init__(self,name,bf_namelists=None):
        print("BlenderFDS: BFSection.init: {}".format(name))
        BFListItem.__init__(self,name=name)
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
        # Get scene
        if bpy.types.Scene in bpy_types: children.append(context.scene)
        # Get objects
        if bpy.types.Object in bpy_types:
            children.extend(ob for ob in context.scene.objects \
                if ob.type == "MESH" and ob.bf_namelist_export \
                and ob.bf_namelist in self.bf_namelists)
        # Get materials
        if bpy.types.Material in bpy_types:
            children.extend(ma for ma in bpy.data.materials \
                if ma.bf_namelist_export and \
                (ma.name not in bf_config.predefined_material_names)) 
        # Alphabetic order by element name
        children.sort(key=lambda k:k.name)
        # Export
        res = self.evaluate() # Do not trap exceptions, pass them upward
        children_values, children_msgs = self._to_fds_children(children=children,context=context)
        # res.value from self.evaluate() is used then replaced with new content
        res.value = self._format(children_values,children_msgs)
        return res

class BFFile(BFListItem,BFHavingChildren):
    """BlenderFDS file, used for generating FDS file."""
    bf_list = BFList() # new class BFList, not that from BFListItem

    def __init__(self,name):
        print("BlenderFDS: BFFile.init: {}".format(name))
        BFListItem.__init__(self,name=name)

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

bf_props = BFProp.bf_list
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

### Default BFNamelist and BFParam

BFProp(
    name = "bf_namelist",
    label = "Namelist",
    description = "Type of FDS namelist",
    bpy_prop = bpy.props.StringProperty,
    default = "OBST",
)

class BFParam_namelist(BFParam):
    def _update_bf_namelist(self,bpy_type,default):
        items = (bf_namelist for bf_namelist in bf_namelists if bf_namelist.bpy_type == bpy_type)
        items = list((item.name,"{} ({})".format(item.label,item.description),item.description,item.unique_id) \
            for item in items)
        items.sort()
        print("items",items)
        bpy_type.bf_namelist = bpy.props.EnumProperty(
            name="Namelist",
            description="Type of FDS namelist",
            items=items,
            default=default,
            )
            
    def update_bf_namelist_items(self):
        print("BlenderFDS: update_bf_namelist_items")
        # namelists for Object
        bpy_type = bpy.types.Object
        default = "OBST"
        self._update_bf_namelist(bpy_type,default)
        # namelists for Material
        bpy_type = bpy.types.Material
        default = "SURF"
        self._update_bf_namelist(bpy_type,default)

BFParam_namelist(
    name = "Namelist",
    bf_props = "bf_namelist",
)

class BFNamelist_TMP(BFNamelist):
    def draw_header(self,context,element,layout):
        return "BlenderFDS Temporary Object"
    def draw(self,context,element,layout):
        row = layout.row()
        row.operator("object.bf_hide_voxels")

BFNamelist_TMP(
    name = "TMP",
    unique_id = 0,
    description = "Temporary object",
    bpy_type = bpy.types.Object,
)

### Generic panels

# Scene panel

class ScenePanel():
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "scene"
    bl_label = "BlenderFDS Scene"
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

class ObjectPanel():
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "object"
    bl_label = "BlenderFDS Object"
    
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

class MaterialPanel():
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "material"
    bl_label = "BlenderFDS Material"

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
