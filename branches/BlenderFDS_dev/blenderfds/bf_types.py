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

import bpy

from .bf_basic_types import BFList, BFListItem, BFResult, BFError
from .bf_osd import bf_osd
from . import bf_format, bf_config

### General functions

def _isiterable(var):
    """Check if var is iterable or not
    
    >>> _isiterable("hello"), _isiterable((1,2,3)), _isiterable({1,2,3})
    (False, True, True)
    """
    # A str is iterable in Py... not what I want
    if isinstance(var,str): return False
    # Let's try and fail nicely
    try:
        for item in var: break
    except TypeError: return False
    return True

def _check_items(inputs,type_):
    """Return a BFList of elements named names of type type_
    
    >>> a, b = BFProp("first"), BFProp("second")
    BlenderFDS: BFProp.init: first
    BlenderFDS: BFProp.init: second
    >>> _check_items(("first","second"),BFProp), _check_items((a,b),BFProp)
    (<BFList([<BFProp('first')>, <BFProp('second')>])>, <BFList([<BFProp('first')>, <BFProp('second')>])>)
    """
    result = BFList()
    if inputs is None: return result
    if not _isiterable(inputs): inputs = (inputs,)
    for input_ in inputs:
        if isinstance(input_,type_): result.append(input_)
        else: result.append(type_.bf_list[input_])
    return result

def _check_item(input_,type_):
    """Return the element named p of type t or None
    
    >>> a = BFProp("first")
    BlenderFDS: BFProp.init: first
    >>> _check_item("first",BFProp), _check_item(a,BFProp)
    (<BFProp('first')>, <BFProp('first')>)
    """
    if isinstance(input_,type_) or input_ is None: return input_
    else: return type_.bf_list[input_]

### Classes

class BFHavingFDSName():
    """Auxiliary class containing common methods for types that have an fds_name

    fds_name -- string, FDS parameter name, eg "ID"
    has_export_flag -- bool, set an automatic or custom export flag for self.
    bf_prop_export -- BFProp or string, customized export flag for self.
    bf_props -- list of BFProp, other related BFProp
    """

    def __init__(self, label, description=None, fds_name=None, has_export_flag=False, bf_prop_export=None, bf_props=None):
        self.label = label
        self.description = description
        self.fds_name = fds_name
        self.has_export_flag = has_export_flag
        if has_export_flag:
            if bf_prop_export: self.bf_prop_export = _check_item(bf_prop_export,BFProp)
            else:
                self.bf_prop_export = BFProp(
                    name = "{} export".format(self.name),
                    label = "Export",
                    description = "Export to FDS",
                    bpy_name = "bf_{}_export".format(self.name.lower()),                    
                    bpy_prop = bpy.props.BoolProperty,
                    default = False,
                    )
        else: self.bf_prop_export = None
        self.bf_props = _check_items(bf_props,BFProp)

    def is_exported_to_fds(self, context, element):
        if self.has_export_flag: return self.bf_prop_export.value(context, element)
        return True

    def draw_error(self,context,element,layout):
        """Draw errors and messages for element in the provided layout."""
        try: res = self.evaluate(context,element)
        except BFError as err: err.draw(layout)
        else: res and res.draw(layout) # check res existence before...    

class BFProp(BFListItem,BFHavingFDSName):
    """Wrapper of a generic Blender property
    
    name -- string, unique name, eg "bf_fyi"
    label -- string, UI label, eg "FYI".
    description -- string, UI help text, eg "For your information"
    operator -- string, optional name of a related operator shown in the UI
    fds_name -- string, FDS parameter name, eg "ID"
    has_export_flag -- bool, set an automatic or custom export flag for self.
    bf_prop_export -- BFProp, customized export flag for self.
    bf_props -- list of BFProp, other related BFProp
    bpy_name -- string, Blender property name, eg "bf_fyi" or "name". If None, use name.
    bpy_prop -- Blender property type, eg bpy.props.StringProperty. If None, refer to existing Blender property.
    **kwargs -- other optional Blender property parameters

    precision -- (object attribute, readonly) float precision
    bf_list -- (class attribute) a BFList containing all created BFProp objects
    
    >>> BFProp("bf_first",bpy_prop=bpy.props.FloatProperty,precision=3)
    BlenderFDS: BFProp.init: bf_first
    <BFProp('bf_first')>
    >>> bf_props["bf_first"].register(bpy.types.Object)
    BlenderFDS: > > BFProp.register: bf_first
    >>> bpy.context.object.bf_first = 3.1234
    >>> bf_props["bf_first"].value(bpy.context,bpy.context.object), bf_props["bf_first"].precision  # doctest:+ELLIPSIS
    (3.123..., 3)
    >>> bf_props["bf_first"].unregister(bpy.types.Object)
    BlenderFDS: > > BFProp.unregister: bf_first
    """
    bf_list = BFList() # new class BFList, not that from BFListItem
    
    def __init__(self, name, label, bpy_name, description=None, operator=None, \
        fds_name=None, has_export_flag=False, bf_prop_export=None, bf_props=None, \
        bpy_prop=None, **kwargs):
        print("BlenderFDS: BFProp.init:", name)
        BFListItem.__init__(self, name=name)
        self.operator = operator
        BFHavingFDSName.__init__(self, label=label, description=description, \
            fds_name=fds_name, has_export_flag=has_export_flag, bf_prop_export=bf_prop_export, bf_props=bf_props)
        self.bpy_name = bpy_name
        self.bpy_prop = bpy_prop
        self.bpy_other = kwargs

    def register(self, bpy_type):
        """Register Blender property in Blender bpy_type"""
        print("BlenderFDS: > > BFProp.register:", self.name)
        # Register self
        if self.bpy_prop: setattr(bpy_type,self.bpy_name,self.bpy_prop(name=self.label,description=self.description,**self.bpy_other))
        # Register bf_prop_export
        if self.bf_prop_export: self.bf_prop_export.register(bpy_type)
        # Register related bf_props
        if self.bf_props:
            for bf_prop in self.bf_props: bf_prop.register(bpy_type)

    def unregister(self, bpy_type):
        """Unregister Blender property in Blender bpy_type"""
        print("BlenderFDS: > > BFProp.unregister:", self.name)
        # Unregister self
        try: delattr(bpy_type,self.bpy_name)
        except: pass
        # Unegister bf_prop_export
        if self.bf_prop_export: self.bf_prop_export.unregister(bpy_type)
        # Unregister related bf_props
        if self.bf_props:
            for bf_prop in self.bf_props: bf_prop.unregister(bpy_type)        

    # evaluate

    def _get_precision(self):
        """Get self precision for element"""
        return self.bpy_other.get("precision",2)

    precision = property(_get_precision)
            
    def check(self,value):
        """Check value. Raise BFError if not good."""
        err = BFError(self)
        if isinstance(value,str):
            if '&' in value or '/' in value:
                err.msgs.append("& and / characters not allowed")
            if "'" in value or '"' in value or "`" in value or "“" in value \
                or "”" in value or "‘" in value or "’‌" in value:
                err.msgs.append("Quote characters not allowed")
        if err.msgs: raise err

    def value(self, context, element):
        """Return self value for element. Raise BFError if not good."""
        value = getattr(element,self.bpy_name)
        self.check(value)
        return value

    def msgs(self, context, element):
        """Get one or more self msgs. Raise BFError if not good."""
        return "Test msgs from {}".format(self.name)

    def evaluate(self, context, element):
        """Check self for errors and return a BFResult or None.
        Raise a BFError exception if self cannot return a valid result.
        """
        return BFResult(sender=self,value=self.value(context,element),msgs=self.msgs(context,element))

    # to_fds

    def format(self,value):
        """Format self for to_fds, return a string"""
        # Custom?
        if not self.fds_name: return str(value)
        # Insert value into an iterable, if it's not already
        if not _isiterable(value): value = tuple((value,))
        # Check each element of the iterable
        if isinstance(value[0],bool): value = ",".join(item and ".TRUE." or ".FALSE." for item in value)
        elif isinstance(value[0],int): value = ",".join(str(item) for item in value)
        elif isinstance(value[0],float): value = ",".join("{:.{}f}".format(item,self.precision) for item in value)
        elif value[0]: value = ",".join("'{}'".format(item) for item in value) # No empty values!
        else: return None # At this point, it's a None!
        return "=".join((self.fds_name,value))

    def to_fds(self,context,element):
        """Export self in FDS notation for element. Return a BFResult() or None."""
        print("BlenderFDS: > > > > BFProp.to_fds: {}".format(self.name))
        if not self.is_exported_to_fds(context, element): return None
        # Evaluate
        res = self.evaluate(context, element) # Do not trap exceptions, pass them upward
        if res is None or res.value is None: return res # Could have msgs
        # res.value from self.evaluate() is used then replaced with new content
        res.value = self.format(res.value)
        return res

    # draw

    def get_layout_export(self, context, element, layout):
        """Prepare double-column Blender panel layout for element if self has a bf_prop_export."""
        if self.bf_prop_export:
            # Set two distinct colums: layout_export and layout_ui
            row = layout.row()
            layout_export, layout = row.column(), row.column()
            layout_export.prop(element, self.bf_prop_export.bpy_name, text="")
            if self.bf_prop_export: layout.active = self.bf_prop_export.value(context, element)
        return layout

    def draw_bf_props(self, context, element, layout):
        """Draw self bf_props for element in the layout."""
        # Draw self
        row = layout.row()
        row.prop(element, self.bpy_name, text=self.label)
        if self.operator: row.operator(self.operator)
        # Draw related bf_props
        for bf_prop in self.bf_props: bf_prop.draw(context, element, layout)

    def draw_extra(self, context, element, layout):
        """Draw extra customized widgets for element in the layout"""
        pass
        
    def draw(self, context, element, layout):
        """Draw Blender panel widgets for self in the provided layout."""
        layout = self.get_layout_export(context, element, layout)
        self.draw_bf_props(context, element, layout)
        self.draw_extra(context, element, layout)
        self.draw_error(context, element, layout)

class BFHavingChildren():
    """Parent type containing methods for types that have children"""

    def to_fds_children(self,children,context,element=None):
        """Pile values, msgs, reraise piled errors from children instances
        
        children -- list of concerned children having the to_fds() method
        context -- Blender context
        element -- If needed, reference element used for to_fds
        """
        # Prepare piles and loop
        values, msgs, errors = list(), list(), list()
        for child in children:
            # Get FDS representation of children
            try: child_res = child.to_fds(context=context, element=element or self)
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
        if errors: raise BFError(sender=self, msgs=errors)
        return values, msgs

class BFNamelist(BFListItem,BFHavingFDSName,BFHavingChildren):
    """BlenderFDS namelist, interface between Blender object and FDS namelist.

    name -- string, unique name, eg "REAC"
    unique_id -- integer, unique id, eg 345
    bpy_type -- One in bpy.types.Scene or bpy.types.Object or bpy.types.Material
    label -- string, UI label, eg "REAC"
    description -- string, UI help text, eg "Reaction"
    fds_name -- string, FDS namelist name, eg "REAC".
    has_export_flag -- bool, set an automatic or custom export flag for self.    
    bf_prop_export -- BFProp, customized export flag for self.
    bf_props -- list of BFProp, one or more BFProp related FDS parameters.

    bf_list -- (class attribute) a BFList containing all created BFNamelist objects
    """
    bf_list = BFList() # new class BFList, not that from BFListItem

    def __init__(self, name, unique_id, bpy_type, label, description=None, \
        fds_name=None, has_export_flag=False, bf_prop_export=None, bf_props=None):
        print("BlenderFDS: BFNamelist.init:", name)
        BFListItem.__init__(self, name=name)
        self.unique_id = unique_id # TODO test it is really unique
        BFHavingFDSName.__init__(self, label=label, description=description, \
            fds_name=fds_name, has_export_flag=has_export_flag, bf_prop_export=bf_prop_export, bf_props=bf_props)
        self.bpy_type = bpy_type

    def register(self):
        """Register corresponding Blender properties"""
        print("BlenderFDS: BFNamelist.register: {}".format(self.name))
        # Register self.bf_props and self.bf_prop_export
        for bf_prop in self.bf_props: bf_prop.register(self.bpy_type)
        if self.bf_prop_export: self.bf_prop_export.register(self.bpy_type)
        # Register panel
        if self.bpy_type == bpy.types.Scene:
            name = "SCENE_PT_bf_" + self.name.replace(' ', '_')
            self._b_panel = type(name,(ScenePanel,bpy.types.Panel,),{"bf_namelist":self.name})
        elif self.bpy_type == bpy.types.Object:
            self._b_panel = type("OBJECT_PT_bf_common",(ObjectPanel,bpy.types.Panel,),{})       
        elif self.bpy_type == bpy.types.Material:
            self._b_panel = type("MATERIAL_PT_bf_common",(MaterialPanel,bpy.types.Panel,),{})
        else: raise Exception("Unrecognized bpy_type in BFNamelist {}".format(self.name))    
        bpy.utils.register_class(self._b_panel)
        
    def unregister(self):
        """Unregister corresponding Blender properties"""
        print("BlenderFDS: BFNamelist.unregister: {}".format(self.name))
        # Unregister self.bf_props and self.bf_prop_export
        for bf_prop in self.bf_props: bf_prop.unregister(self.bpy_type)
        if self.bf_prop_export: self.bf_prop_export.unregister(self.bpy_type)
        # Unregister panel
        try: bpy.utils.unregister_class(self._b_panel)
        except: pass

    def check(self, value):
        """Check value. Raise BFError if not good."""
        pass    

    def value(self, context, element):
        """Get self value. Raise BFError if not good."""
        value = None
        self.check(value)
        return value

    def msgs(self, context, element):
        """Get one or more self msgs. Raise BFError if not good."""
        return "Test msgs from {}".format(self.name)

    def evaluate(self, context, element):
        """Check self for errors and return a BFResult or None.
        Raise a BFError exception if self cannot return a valid result.
        """
        return BFResult(sender=self, value=self.value(context,element), msgs=self.msgs(context,element))

    def format(self, context, element, values, msgs):
        """Format self for to_fds, return a string"""
        # Extract one multivalues from values
        multivalues = None
        for index, value in enumerate(values):
            if _isiterable(value):
                if multivalues: raise Exception("Double multivalues not previously detected in element", element.name)
                multivalues = values.pop(index)
        # Format fds_name, eg &OBST
        # It no fds_name available, use the first bf_prop
        if self.fds_name is None:
            self.fds_name = values[0]
            values = values[1:]
        namelist = "".join(("&",self.fds_name))
        # Format namelist body, eg FYI=... ID=...
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
        if not self.is_exported_to_fds(context,element): return None
        # Export
        res = self.evaluate(context,element) # Do not trap exceptions, pass them upward
        children_values, children_msgs = self.to_fds_children(children=self.bf_props, context=context, element=element)
        # res.value from self.evaluate() is used then replaced with new content
        res.value = self.format(context, element, children_values, children_msgs)
        return res

    def draw_header(self,context,element,layout):
        """Draw Blender panel header for element in the provided layout."""
        if self.bf_prop_export: layout.prop(element, self.bf_prop_export.bpy_name, text="")
        if self.description: return "BlenderFDS {} ({})".format(self.label, self.description)
        return "BlenderFDS {}".format(self.label)
    
    def get_layout_export(self, context, element, layout):
        """Prepare Blender panel layout if self has a bf_prop_export."""
        if self.bf_prop_export: layout.active = self.bf_prop_export.value(context, element)
        return layout

    def draw_extra(self, context, element, layout):
        """Draw extra customized widgets for element in the layout"""
        pass

    def draw_bf_props(self, context, element, layout):
        """Draw self bf_props for element in the layout."""
        for bf_prop in self.bf_props: bf_prop.draw(context, element, layout)

    def draw(self, context, element, layout):
        """Draw Blender panel for element in the layout"""
        layout = self.get_layout_export(context, element, layout)
        self.draw_extra(context, element, layout)
        self.draw_bf_props(context, element, layout)
        self.draw_error(context, element, layout)
        # TODO copy FDS properties (except ID?) to other elements
        # FIXME error, extra?

class BFSection(BFListItem,BFHavingChildren):
    """BlenderFDS section, used for grouping FDS namelists.

    name -- unique name, eg "Geometry"
    bf_namelists -- Collection of at least one BFNamelist
    
    bf_list -- (class attribute) a BFList containing all created BFSection objects
    """
    bf_list = BFList() # new class BFList, not that from BFListItem

    def __init__(self, name, bf_namelists=None):
        print("BlenderFDS: BFSection.init: {}".format(name))
        BFListItem.__init__(self,name=name)
        self.bf_namelists = _check_items(bf_namelists,BFNamelist)

    def is_exported_to_fds(self): # *args and **kwargs are for compatibility with other classes
        """If self is going to be exported return True, else False"""
        return True

    def evaluate(self):
        """Check self for errors and return a BFResult or None.
        Raise a BFError exception if self cannot return a valid result.
        """
        return BFResult(sender=self, value=None, msgs="Test msg from section " + self.name)

    def format(self, values, msgs):
        """Format self, return a string"""
        return "".join((bf_format.format_section_title(self.name), bf_format.format_comment(msgs), "\n" ,bf_format.format_body(values), "\n"))

    def to_fds(self, context, *args, **kwargs): # *args and **kwargs are for compatibility with to_fds_children() method
        """Export self in FDS notation. Return a BFResult() or None."""
        print("BlenderFDS: > BFSection.to_fds: {}".format(self.name))
        # Check
        if not self.is_exported_to_fds(): return None
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
        children_values, children_msgs = self.to_fds_children(children=children,context=context)
        # res.value from self.evaluate() is used then replaced with new content
        res.value = self.format(children_values,children_msgs)
        return res

class BFFile(BFListItem, BFHavingChildren):
    """BlenderFDS file, used for generating FDS file."""
    bf_list = BFList() # new class BFList, not that from BFListItem

    def __init__(self, name):
        print("BlenderFDS: BFFile.init: {}".format(name))
        BFListItem.__init__(self, name=name)

    def is_exported_to_fds(self): # *args and **kwargs are for compatibility with other classes
        """If self is going to be exported return True, else False"""
        return True

    def evaluate(self):
        """Check self for errors and return a BFResult or None.
        Raise a BFError exception if self cannot return a valid result.
        """
        return BFResult(sender=self, value=None, msgs="Test msg from file" + self.name)

    def format(self, values, msgs, error=False):
        """Format self, return a string"""
        if error: return "".join((bf_format.format_file_title(), "\n", bf_format.format_section_title("ERRORS"), bf_format.format_comment(msgs),))
        else: return "".join((bf_format.format_file_title(), bf_format.format_comment(msgs), "\n", bf_format.format_body(values), "\n&TAIL /"))

    def to_fds(self, context, *args, **kwargs): # *args and **kwargs are for compatibility with to_fds_children() method
        """Export self in FDS notation. Return a BFResult() or None."""
        print("BlenderFDS: BFFile.to_fds")
        # Check
        if not self.is_exported_to_fds(): return None
        # Manage error to produce output file
        try:
            res = self.evaluate()
            children_values, children_msgs = self.to_fds_children(children=bf_sections, context=context) 
        except BFError as err:
            return BFResult(sender=self, value=self.format(None, err.labels, True))
        else:
            # res.value from self.evaluate() is used then replaced with new content
            res.value = self._format(children_values, children_msgs)
            return res

### Global names

bf_props = BFProp.bf_list
bf_namelists = BFNamelist.bf_list
bf_sections = BFSection.bf_list
bf_file = BFFile("Case")

### Class bpy.types.*, add methods

def bpy_types_get_bf_namelists(self):
    """Get BFNamelist related to self. Return a tuple() of BFNamelist."""
    if hasattr(self, "bf_namelist"):
        # This is an element that has an only BFNamelist
        return tuple((bf_namelists[self.bf_namelist],))
    else:
        # This is an object that has many BFNamelist
        return tuple(bf_namelist for bf_namelist in bf_namelists if bf_namelist.bpy_type == self.__class__)

def bpy_types_get_bf_props(self):
    """Get BFProp related to self. Return a tuple() of BFProp."""
    return BFList(set(bf_prop for bf_namelist in self.get_bf_namelists() for bf_prop in bf_namelist.bf_props))

def bpy_types_has_bf_prop(self, bf_prop):
    """Check if self has bf_prop parameter"""
    return bf_prop in self.get_bf_props()

def bpy_types_to_fds(self, context=None, element=None): # element is kept for compatibility, self is the element itself.
    """Export self to FDS notation. Return a BFResult() or None."""
    print("BlenderFDS: > > {}.to_fds: {}".format(self.__class__.__name__, self.name))
    bf_osd.show("BlenderFDS exporting {} {}".format(self.__class__.__name__, self.name))
    if not context: context = bpy.context
    # Get children: get values and msgs for each of my BFNamelist
    values, msgs = self.to_fds_children(children=self.get_bf_namelists(), context=context, element=self)
    return BFResult(sender=self, value=bf_format.format_body(values), msgs=msgs)

# Assign methods to Scene, Object, and Material
bpy.types.Scene.to_fds_children = BFHavingChildren.to_fds_children
bpy.types.Scene.get_bf_namelists = bpy_types_get_bf_namelists
bpy.types.Scene.get_bf_props = bpy_types_get_bf_props
bpy.types.Scene.has_bf_prop = bpy_types_has_bf_prop
bpy.types.Scene.to_fds = bpy_types_to_fds

bpy.types.Object.to_fds_children = BFHavingChildren.to_fds_children
bpy.types.Object.get_bf_namelists = bpy_types_get_bf_namelists
bpy.types.Object.get_bf_props = bpy_types_get_bf_props
bpy.types.Object.has_bf_prop = bpy_types_has_bf_prop
bpy.types.Object.to_fds = bpy_types_to_fds

bpy.types.Material.to_fds_children = BFHavingChildren.to_fds_children
bpy.types.Material.get_bf_namelists = bpy_types_get_bf_namelists
bpy.types.Material.get_bf_props = bpy_types_get_bf_props
bpy.types.Material.has_bf_prop = bpy_types_has_bf_prop
bpy.types.Material.to_fds = bpy_types_to_fds

### Default mods

# TODO

### Default BFNamelist and BFProp

class BFPropNamelist(BFProp): # FIXME
    """Self has a special method to update self menus"""
    def _update_bf_namelist(self,bpy_type,default):
        items = list()
        for bf_namelist in bf_namelists:
            if not bf_namelist.bpy_type == bpy_type: continue
            if bf_namelist.description: description = "{} ({})".format(bf_namelist.label,bf_namelist.description)
            else: description = bf_namelist.label
            items.append((bf_namelist.name,description,description,bf_namelist.unique_id,))
        items.sort()
        bpy_type.bf_namelist = bpy.props.EnumProperty(
            name="Namelist",
            description="Type of FDS namelist",
            items=items,
            default=default,
            )
            
    def update_bf_namelist_items(self):
        print("BlenderFDS: update_bf_namelist_items")
        self._update_bf_namelist(bpy.types.Object,"OBST")
        self._update_bf_namelist(bpy.types.Material,"SURF")

BFPropNamelist(
    name = "Namelist",
    label = "Namelist",
    description = "Type of FDS namelist",
    bpy_name = "bf_namelist",
    bpy_prop = bpy.props.StringProperty,
)



class BFNamelist_TMP(BFNamelist):
    def draw_header(self,context,element,layout):
        return "BlenderFDS Temporary Object"
    
    def draw(self,context,element,layout):
        row = layout.row()
        row.operator("object.bf_hide_voxels")

BFNamelist_TMP(
    name = "TMP",
    label = "TMP",
    description = "Temporary object",
    unique_id = 0,
    bpy_type = bpy.types.Object,
)

### Generic panels

# Scene panel

class ScenePanel():
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "scene"
    bl_label = "BlenderFDS Scene"

    def draw_header(self,context):
        layout = self.layout
        element = context.scene
        bf_namelist = bf_namelists[type(self).bf_namelist] # access Class variable and get self bf_namelist object
        self.bl_label = bf_namelist.draw_header(context,element,layout)

    def draw(self,context):
        layout = self.layout
        element = context.scene
        bf_namelist = bf_namelists[type(self).bf_namelist] # access Class variable and get self bf_namelist object
        bf_namelist.draw(context,element,layout)

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
        bf_namelist = bf_namelists[element.bf_namelist] # get self bf_namelist object from element
        self.bl_label = bf_namelist.draw_header(context,element,layout)

    def draw(self,context):
        layout = self.layout
        element = context.active_object
        bf_namelist = bf_namelists[element.bf_namelist] # get self bf_namelist object from element
        bf_namelist.draw(context,element,layout)

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
        return ma and ob and ob.type == "MESH" and "SURF_ID" in ob.get_bf_props() and not ob.bf_is_voxels

    def draw_header(self,context):
        layout = self.layout
        element = context.material
        bf_namelist = bf_namelists[element.bf_namelist] # get self bf_namelist object from element
        self.bl_label = bf_namelist.draw_header(context,element,layout)

    def draw(self,context):
        layout = self.layout
        element = context.material
        bf_namelist = bf_namelists[element.bf_namelist] # get self bf_namelist object from element
        bf_namelist.draw(context,element,layout)

# Doctest
def test():
    """Doctest function.
    
    Open a Blender example, open a Python console, type "import blenderfds",
    type "blenderfds.bf_types.test()", check test failures (if any ;-)
    """
    import doctest
    from . import bf_types as module
    return doctest.testmod(module,verbose=False).failed
