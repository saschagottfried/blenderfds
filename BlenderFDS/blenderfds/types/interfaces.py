"""BlenderFDS, interface types"""

import bpy, sys, time
from blenderfds.types.results import BFResult, BFException
from blenderfds.types.collections import BFList, BFAutoItem
from blenderfds.types.flags import *
from blenderfds.lib import geometry, fds_surf, fds_to_py, fds_format
from blenderfds.lib.utilities import isiterable

DEBUG = False

# FIXME
# read_factory_settings()
#bpy.ops.wm.save_homefile()
# bpy.ops.wm.open_mainfile(filepath="", filter_blender=True, filter_backup=False, filter_image=False, filter_movie=False, filter_python=False, filter_font=False, filter_sound=False, filter_text=False, filter_btx=False, filter_collada=False, filter_folder=True, filemode=8, display_type='FILE_DEFAULTDISPLAY', load_ui=True, use_scripts=True)


### BFCommon

class BFCommon(BFAutoItem):
    """Common attributes and methods for all BF classes.
    
    idname -- Instance unique identificator. Type: string, never None. Eg: "bf_fyi"
    label -- UI label. Type: string, if None idname is used. Eg: "FYI"
    description -- UI help text. Type: string, None. Eg: "For your information"
    enum_id -- Instance unique numeric id for EnumProperty item. Type: int, never None. Eg: "1023"    
    flags -- define specific behaviours, see types.flags. Eg: NOUI | NOEXPORT
    fds_label -- FDS label. Type: string, if None this is a custom property. Eg: "ID"
    bf_props -- List of related BFProp idnames. Type: tuple of strings, None.
    bf_prop_export -- idname of related export BFProp. Type: string, None. Eg: "bf_fyi_export"
    bf_prop_custom -- idname of related custom BFProp. Type: string, None. Eg: "bf_fyi_custom"
    bf_other -- Other optional BlenderFDS parameters. Type: dict, None. Eg: "{'prop1': value1, 'prop2': value2}"
    bpy_type -- Blender type of self. Eg: bpy.types.Scene, bpy.types.Object, bpy.types.Material
    bpy_idname -- idname of related Blender property. Type: string, None. Eg: "bf_fyi" or "name".
    bpy_prop -- Blender property type. Type: bpy.props.*Property, if None refer to existing Blender property.
    **kwargs -- Other optional Blender property parameters. Eg: default = "NONE".
    """
    bf_list = BFList() # contains all instances of this class

    def __init__(self, idname, label=None, description=None, enum_id=0, flags=0, \
        fds_label=None, bf_props=None, bf_prop_export=None, bf_prop_custom=None, bf_other=None, \
        bpy_type=None, bpy_idname=None, bpy_prop=None, **kwargs):
        # Check enum_id unicity
        if enum_id and enum_id in (item.enum_id for item in self.bf_list):
            raise ValueError("BFDS: Duplicated enum_id '{}' in '{}' and '{}'".format(enum_id, item.idname, idname))
        # Parent class
        BFAutoItem.__init__(self, idname=idname)
        # Identification
        self.label = label or idname
        self.description = description
        self.enum_id = enum_id
        self.flags = flags
        # Others
        self.fds_label = fds_label
        self.bpy_type = bpy_type
        # Set related bf_props instances: transform BFList of idname in BFList of BFProp
        if bf_props: self.bf_props = BFProp.bf_list[bf_props]
        else: self.bf_props = None
        # Set export BFProp: transform idname in BFProp or create new BFProp
        if bf_prop_export:
            try: self.bf_prop_export = BFProp.bf_list[bf_prop_export] # Does it esist?
            except KeyError: # No, create a default one
                self.bf_prop_export = BFProp(
                    idname = bf_prop_export,
                    label = "Export",
                    description = "Export to FDS",
                    bpy_idname = bf_prop_export,
                    bpy_prop = bpy.props.BoolProperty,
                    default = False,
                    )
        else: self.bf_prop_export = None
        # Set custom BFProp: transform idname in BFProp, do not create default it shall exist
        if bf_prop_custom: self.bf_prop_custom = BFProp.bf_list[bf_prop_custom]
        else: self.bf_prop_custom = None
        # Set bf_other
        self.bf_other = bf_other
        # Set related Blender property
        self.bpy_idname = bpy_idname
        self.bpy_prop = bpy_prop
        self.bpy_other = kwargs

    # Register/Unregister (me, each from self.bf_props, self.bf_prop_export, self.bf_prop_custom)

    def register(self, bpy_type=None):
        """Register all related Blender properties."""
        # Get bpy_type: mine or that sent as parameter from caller object
        if not bpy_type: bpy_type = self.bpy_type
        # Register children, export and custom
        for child in self.children: child.register(bpy_type)
        if self.bf_prop_export: self.bf_prop_export.register(bpy_type)
        if self.bf_prop_custom: self.bf_prop_custom.register(bpy_type)
        # Register my own Blender property
        if self.bpy_prop and self.bpy_idname:
            if DEBUG: print("BFDS: BFCommon.register:", bpy_type, self.bpy_idname) 
            setattr(bpy_type, self.bpy_idname, \
                self.bpy_prop(name=self.label, description=self.description, **self.bpy_other))

    def unregister(self, bpy_type=None):
        """Unregister all related Blender properties."""
        # Get bpy_type: mine or that sent as parameter from caller object
        if not bpy_type: bpy_type = self.bpy_type
        # Unregister children, export and custom
        for child in self.children: child.unregister(bpy_type)
        if self.bf_prop_export: self.bf_prop_export.unregister(bpy_type)
        if self.bf_prop_custom: self.bf_prop_custom.unregister(bpy_type)
        # and unregister my own Blender property
        if self.bpy_idname:
            if DEBUG: print("BFDS: BFCommon.unregister:", bpy_type, self.bpy_idname) 
            try: delattr(bpy_type, self.bpy_idname)
            except: pass

    # UI
    
    def has_ui(self, context, element) -> "bool":
        """Return True if self has an user interface."""
        if self.flags & NOUI: return False
        return True

    def has_active_ui(self, context, element) -> "bool":
        """Return True if self has an active user interface."""
        if self.flags & ACTIVEUI: return True
        return self.get_exported(context, element) or False

    def _draw_extra(self, layout, context, element):
        """Draw extra customized widgets."""
        pass

    # Export

    def get_exported(self, context, element) -> "bool":
        """Return True if self is exported to FDS."""
        if self.flags & NOEXPORT: return False
        try: return self.bf_prop_export.get_value(context, element)
        except: return True

    def _get_children(self) -> "BFList of BFProps, never None":
        """Get my children, bf_prop_export is not a children."""
        children = BFList()
        if self.bf_props: children.extend(self.bf_props) # self.bf_props could be None
        if self.bf_prop_custom: children.append(self.bf_prop_custom) # self.bf_prop_custom could be None
        return children
    
    children = property(_get_children)

    def _get_children_res(self, context, element, ui=False, progress=False) -> "BFList of BFResult, never None":
        """Return a BFlist of children BFResult. On error raise joined BFException."""
        if DEBUG: print("BFDS: BFCommon._get_children_res:", self.idname, element.idname) 
        # Init
        children, children_res, err_msgs = self.children, BFList(), list()
        if progress:
            wm = context.window_manager
            wm.progress_begin(0, 100)
        index_max = len(children)
        # Get children res, manage exceptions
        for index, child in enumerate(children):
            if progress: wm.progress_update(int(index/index_max*100))
            try: child_res = child.get_res(context, element, ui=False)
            except BFException as child_err:
                err_msgs.extend(child_err.labels) # The child sends exceptions, take note. Labels attach sender name to msgs.
            else:
                if child_res is None: continue # The child did not send a result, continue
                children_res.append(child_res) # The child result is appended to the BFList
        # Return
        if progress: wm.progress_end()
        if err_msgs: raise BFException(sender=self, msgs=err_msgs) # Raise all piled exceptions to parent
        return children_res

    def _format(self, context, element, my_res, children_res) -> "str or None":
        """Format to FDS notation. On error raise BFException."""
        # Expected output:
        #   ! Me: Message               < my_res.labels
        #   ! Me: Children1: Message    < body < children_res values
        #   Children1 body
        #   ! Me: Children2: Message
        #   Children2 body
        #   My value                    < my_res.value
        return "".join((
            fds_format.to_comment(my_res.labels),
            "".join(child_res.value or str() for child_res in children_res), # child_res.value could be None
            my_res.value or str(), # my_res.value could be None
        ))

    def get_my_res(self, context, element, ui=False) -> "BFResult or None":
        """Get my BFResult. On error raise BFException."""
        # Check me exported 
        if not self.get_exported(context, element): return None
        # Here you can set a value, append msgs, append relevant operators.
        # When something is not good you should raise a BFException.
        # When ui is calling, do not perform heavy or useless calculations.
        return BFResult(
            sender = self,
            value = DEBUG and "! Debug value from BFCommon.get_my_res of '{}'\n".format(self.idname) or None, 
            msg = DEBUG and "Debug message from BFCommon.get_my_res of '{}'".format(self.idname) or None, 
        )

    def get_res(self, context, element, ui=False) -> "BFResult or None":
        """Get full BFResult (children and mine). On error raise BFException."""
        # Init
        my_res = self.get_my_res(context, element, ui)
        if not my_res: return None
        children_res = self._get_children_res(context, element, ui)
        # Format value and return
        my_res.value = self._format(context, element, my_res, children_res)
        return my_res
   
    # Import

    def set_exported(self, context, element, value=True) -> "bool":
        """Set if self is exported to FDS."""
        if element and hasattr(self, "bf_prop_export") and self.bf_prop_export: self.bf_prop_export.set_value(context, element, value)

    # Other

    def _get_descendants(self) -> "Same as self.children":
        """Get my children and the children of my children..."""
        children = descendants = self.children # self.children is never None
        for child in children or list(): descendants.extend(child.descendants)
        return descendants
    
    descendants = property(_get_descendants)

    # Precision for float types
    
    def _get_precision(self) -> "int":
        """Get my precision for element or default to 2."""
        return self.bpy_other.get("precision", 2)

    precision = property(_get_precision)

    # Menu item for EnumProperty

    def _get_enumproperty_item(self) -> "tuple":
        """Get item for EnumProperty items."""
        if self.flags & NOUI: return None
        description = self.description and "{} ({})".format(self.label, self.description) or self.label
        return (self.idname, description, description, self.enum_id)

    enumproperty_item = property(_get_enumproperty_item)

### Blender property <-> BFProp <-> FDS parameter

class BFProp(BFCommon):
    """BlenderFDS property, interface between a Blender property and an FDS parameter. See BFCommon."""
    bf_list = BFList() # contains all instances of this class

    def __init__(self, idname, label=None, description=None, flags=0, \
        fds_label=None, bf_props=None, bf_prop_export=None, bf_other=None, \
        bpy_idname=None, bpy_prop=None, **kwargs):
        # Parent class, partially use its features
        BFCommon.__init__(self, idname=idname, label=label, description=description, flags=flags,\
            fds_label=fds_label, bf_props=bf_props, bf_prop_export=bf_prop_export, bf_other=bf_other, \
            bpy_idname=bpy_idname, bpy_prop=bpy_prop, **kwargs)

    # UI: draw panel (self.bf_prop_export, me)
    # Override methods for custom panel

    def _get_layout(self, layout, context, element) -> "layout":
        """If self has a bf_prop_export, prepare double-column Blender panel layout."""
        layout = layout.row()
        if self.bf_prop_export:
            # Set two distinct colums: layout_export and layout_ui
            layout_export, layout = layout.column(), layout.column()
            layout_export.prop(element, self.bf_prop_export.bpy_idname, text="")
        else:
            # Set one only column
            layout = layout.column()
        layout.active = bool(self.has_active_ui(context, element)) # bool( protects from None
        return layout

    def _draw_body(self, layout, context, element):
        """Draw my part of panel body."""
        if not self.bpy_idname: return
        row = layout.row()
        row.prop(element, self.bpy_idname, text=self.label)
    
    def _draw_messages(self, layout, context, element):
        """Draw messages and exceptions."""
        try: res = self.get_res(context, element, ui=True)
        except BFException as err: err.draw(layout)
        else: res and res.draw(layout) # check res and res.msgs existence before...    
        
    def draw(self, layout, context, element):
        """Draw my part of Blender panel."""
        if self.has_ui(context, element):
            layout = self._get_layout(layout, context, element)
            self._draw_body(layout, context, element)
            self._draw_extra(layout, context, element)
            self._draw_messages(layout, context, element)

    # Export (only me, not self.bf_props)
    # get_my_res and _get_children_res are not used
    
    # Override the get_exported() method for special exporting logic.
    # Override the get_value() method for specific calculations on my value or raise special BFExceptions
    # Override the _format_value() method for specific formatting
    # Override the get_res() method to send informative msgs or raise special BFExceptions
    # The get_res() method is also used to draw the same messages and exceptions on the UI panel

    def get_value(self, context, element) -> "any or None":
        """Get my Blender property value for element. On error raise BFException."""
        return getattr(element, self.bpy_idname or str(), None) # None is not accepted as attribute name, replaced with str()
        
    def _format_value(self, context, element, value) -> "str or None":
        """Format value in FDS notation for me. On error raise BFException."""
        # Expected output:
        #   ID='example' or PI=3.14 or COLOR=3,4,5
        # Custom parameters (no self.fds_label) needs special treatment in fds.props or fds.props_geometry
        if value is None: return None
        # If value is not an iterable, then put it in a tuple
        if not isiterable(value): values = tuple((value,)) 
        else: values = value
        # Check first element of the iterable and choose formatting
        if   isinstance(values[0], bool):  value = ",".join(value and ".TRUE." or ".FALSE." for value in values)
        elif isinstance(values[0], int):   value = ",".join(str(value) for value in values)
        elif isinstance(values[0], float): value = ",".join("{:.{}f}".format(value, self.precision) for value in values)
        else: value = ",".join("'{}'".format(value) for value in values)
        return "=".join((self.fds_label, value))

    # There is no get_my_res

    def get_res(self, context, element, ui=False) -> "BFResult or None":
        """Get BFResult. On error raise BFException."""
        if not self.get_exported(context, element): return None
        res = BFResult(sender=self,)
        value = self.get_value(context, element)
        if not ui: res.value = self._format_value(context, element, value)
        return res

    # Import

    def set_value(self, context, element, value):
        """Set my Blender property value for element. On error raise BFException.
        Value is any type of data compatible with bpy_prop
        Eg: "String", (0.2,3.4,1.2), ...
        """
        try: setattr(element, self.bpy_idname, value)
        except: raise BFException(
            sender = self,
            msg = "Error setting '{}' to '{}.{}'".format(value, element.name, str(self.bpy_idname)),
        )

    def from_fds(self, context, element, value):
        """Set my value from value in FDS notation for element. On error raise BFException.
        Value is any type of data compatible with bpy_prop
        Eg: "String", (0.2,3.4,1.2), ...
        """
        self.set_exported(context, element, True)
        self.set_value(context, element, value)

### Blender group of variables or panel <-> BFNamelist <-> FDS namelist

class BFNamelist(BFCommon):
    """BlenderFDS namelist, interface between a Blender object and an FDS namelist. See BFCommon."""
    bf_list = BFList() # contains all instances of this class

    def __init__(self, idname, label=None, description=None, enum_id=0, flags=0, \
        fds_label=None, bf_props=None, bf_prop_export=None, bf_prop_custom=None, bf_other=None, \
        bpy_type=None):
        # Parent class, partially use its features
        BFCommon.__init__(self, idname=idname, label=label, description=description, enum_id=enum_id, flags=flags, \
            fds_label=fds_label, bf_props=bf_props, bf_prop_export=bf_prop_export, bf_prop_custom=bf_prop_custom, bf_other=bf_other, \
            bpy_type=bpy_type)

    # UI: draw panel (me, self.bf_prop_export, self.bf_props, self.bf_prop_custom)
    # Override methods for custom panel
    
    def draw_header(self, layout, context, element):
        """Draw Blender panel header."""
        if self.bf_prop_export: layout.prop(element, self.bf_prop_export.bpy_idname, text="")
        if self.description: return "BlenderFDS {} ({})".format(self.label, self.description)
        return "BlenderFDS {}".format(self.label)
    
    def _get_layout(self, layout, context, element) -> "layout":
        """If self has a bf_prop_export, prepare Blender panel layout."""
        layout.active = bool(self.has_active_ui(context, element)) # bool( protects from None
        return layout

    def _draw_body(self, layout, context, element):
        """Draw panel body."""
        for bf_prop in self.bf_props or tuple(): bf_prop.draw(layout, context, element)
        if self.bf_prop_custom: self.bf_prop_custom.draw(layout, context, element)

    def _draw_messages(self, layout, context, element):
        """Draw messages and exceptions."""
        try: res = self.get_my_res(context, element, ui=True)
        except BFException as err: err.draw(layout, box=True)
        else: res and res.draw(layout, box=True) # check res existence before...    

    def draw(self, layout, context, element):
        """Draw Blender panel."""
        if self.has_ui(context, element):
            layout = self._get_layout(layout, context, element)
            self._draw_messages(layout, context, element)
            self._draw_body(layout, context, element)
            self._draw_extra(layout, context, element)

    # Export (me and children)
    # Override the get_exported() method for special exporting logic.
    # Override the _format() method for specific formatting
    # Override the get_my_res() method to send informative msgs, special values or raise special BFExceptions
    # The get_my_res() method is also used to draw the same messages and exceptions on the UI panel
    # Override the get_res() method to send informative msgs or raise special BFExceptions

    # Single ID is sent from bf_id BFProp,
    # multiple ID is embedded in multivalues coming from geometric BFProp

    def _format(self, context, element, my_res, children_res) -> "str or None":
        """Format to FDS notation. On error raise BFException."""
        # Expected output:
        #   ! OBST: Message                 < my_res.labels
        #   ! OBST: ID: Message               + children labels (from self.bf_props)
        #   ! OBST: XB: Message
        #   &OBST ID='example' XB=... /\n   < body < c_multivalue[0] + c_value
        #   &OBST ID='example' XB=... /\n            c_multivalue[1] + c_value
        #   &XXXX P1=... /\n                < my_res.value
        # Append children messages to my messages (children are self.bf_props)
        my_res.msgs.extend(label for child_res in children_res for label in child_res.labels)
        # Search and extract the first (and should be only) multivalue from children values     
        child_multivalues = None
        for child_res in children_res:
            if isiterable(child_res.value):
                # Set the multivalue and remove multivalue child_res from single value children_res
                child_multivalues = child_res.value
                children_res.remove(child_res)
                # Each multivalue contains its multi ID, then remove ordinary single ID sent by "bf_id"
                children_res.remove(children_res["bf_id"])
                break
        # Set fds_label: use self.fds_label or children_res[0].value
        # When using first children value, remove child from BFList 
        if self.fds_label: fds_label = self.fds_label
        else: fds_label = children_res.pop(0).value
        # Join children values
        children_value = " ".join(child_res.value for child_res in children_res if child_res.value)
        # Build body: When multivalue exists, ID is embedded into each child_multivalue;
        # else ID is embedded into children_value
        if child_multivalues: body = "".join("&{} {} {} /\n".format(
            fds_label, child_multivalue, children_value,
            ) for child_multivalue in child_multivalues
        )
        else: body = "&{} {} /\n".format(fds_label, children_value)
        # Return
        return "".join((
            fds_format.to_comment(my_res.labels),
            body,
            my_res.value or str(), # my_res.value could be None
        ))

    # Import

    def from_fds(self, context, element, value) -> "None":
        """Translate and set FDS value to my value for element. On error raise BFException (trapped by parent from_fds method).
        Value is: ((fds_label, value, original string), ...)
        Eg: (("ID", "example", "ID='example'"), ("XB", (1., 2., 3., 4., 5., 6.,), "XB=..."), ("SURF_ID", "material"),...)
        """
        if DEBUG: print("BFDS: BFNamelist.from_fds:", self.idname, element.name, "\n", value) 
        # Set myself
        self.set_exported(context, element, True)
        if "draw_type" in (self.bf_other or tuple()): element.draw_type = self.bf_other["draw_type"]
        if "hide" in (self.bf_other or tuple()): element.hide = self.bf_other["hide"]
        # For each token set corresponding descendant or custom_value
        err_msgs, custom_value = list(), list()
        for token in value or tuple():
            # Unpack
            fds_original, fds_label, fds_value = token
            # Search the corresponding descendant by fds_label and try to set its value
            is_imported = False
            for descendant in self.descendants:
                if descendant.fds_label == fds_label:            
                    # fds_label corresponding
                    try: descendant.from_fds(context, element, fds_value)
                    except BFException as descendant_err:
                        err_msgs.extend(descendant_err.labels) # The descendant sends exceptions, take note. Labels attach sender name to msgs.
                    else: is_imported = True # succesful import
                    break # end the search for descendants
            # Check if import was succesful
            if not is_imported:
                # The token could not be imported because of a raised exception or corresponding descendant not found,
                # so pile the original token into custom_value.
                custom_value.append(fds_original)
                print("BFDS: BFNamelist.from_fds: to custom param:\n ", fds_original) 
        # Set final bf_prop_custom. If no self.bf_prop_custom, then the saved fds_origins are lost
        if custom_value and self.bf_prop_custom:
            self.bf_prop_custom.set_value(context, element, " ".join(custom_value))
        # Raise all piled exceptions to parent
        if err_msgs: raise BFException(sender=self, msgs=err_msgs)

### Blender Object <-> BFObject <-> FDS geometric entity (eg. OBST, VENT, HOLE...)

class BFObject(BFCommon):
    """Extend Blender object type"""

    def _get_idname(self) -> "str":
        """Generate idname from self.name."""
        return self.name

    idname = property(_get_idname)

    # UI: draw panel
    # Blender objects have one only panel
    
    def draw_header(self, layout, context, element=None): # 'element' kept for polymorphism
        """Draw Blender panel header."""
        # Header for temporary object
        if self.bf_is_tmp: return "BlenderFDS Temporary Object"
        # Header for EMPTY object
        if self.type == "EMPTY":
            layout.prop(self, "bf_export", text="")
            return "BlenderFDS Empty (Group of namelists)"
        # Header for MESH object
        return BFNamelist.bf_list[self.bf_namelist].draw_header(layout, context, self)

    def _draw_messages(self, layout, context, element=None): # 'element' kept for polymorphism
        """Draw messages and exceptions."""
        try: res = self.get_my_res(context, self, ui=True)
        except BFException as err: err.draw(layout, box=True)
        else: res and res.draw(layout, box=True) # check res existence before...    

    def _draw_body(self, layout, context, element=None): # FIXME
        """Draw panel body."""
        # Panel for temporary object
        if self.bf_is_tmp:
            layout.operator("scene.bf_del_all_tmp_objects")
            return
        # Panel for EMPTY object
        if self.type == "EMPTY":
            BFProp.bf_list["bf_id"].draw(layout, context, self) # FIXME why index?
            BFProp.bf_list["bf_fyi"].draw(layout, context, self)
            return
        # Panel for MESH object
        if self.type == "MESH":
            split = layout.split(.6)  # namelist
            split.prop(self, "bf_namelist", text="")
            row = split.row(align=True)  # aspect
            row.prop(self, "show_transparent", icon="GHOST", text="")
            row.prop(self, "draw_type", text="")
            row.prop(self, "hide", text="")
            row.prop(self, "hide_select", text="")
            row.prop(self, "hide_render", text="")
            BFNamelist.bf_list[self.bf_namelist].draw(layout, context, self)
            row = layout.row()
            if self.bf_has_tmp: row.operator("scene.bf_del_all_tmp_objects")
            else: row.operator("object.bf_show_fds_geometries")
            row.operator("object.bf_props_to_sel_obs")

    def _draw_extra(self, layout, context, element=None): # FIXME
        """Draw extra customized widgets."""
        pass
    
    def draw(self, layout, context, element=None): # 'element' kept for polymorphism
        """Draw Blender panel."""
        self._draw_messages(layout, context)
        self._draw_body(layout, context)
        self._draw_extra(layout, context)

    # Export (me and children)
    # Override the _format() method for specific formatting
    # Override the get_my_res() method to send informative msgs, special values or raise special BFExceptions
    # The get_my_res() method is also used to draw the same messages and exceptions on the UI panel
    # Override the get_res() method to send informative msgs or raise special BFExceptions

    def _get_children(self) -> "BFList of BFNamelist and Blender objects, never None":
        """Get children: bf_namelist related to self, children objects."""
        # Init
        children = list()
        context = bpy.context
        # Get my bf_namelist, if self is a MESH
        if self.type == "MESH":
            children.append(BFNamelist.bf_list[self.bf_namelist]) # This is an element that has one BFNamelist: Object, Material
        # Get children objects
        obs = list(ob for ob in context.scene.objects \
            if ob.type in ("MESH", "EMPTY",) and ob.parent == self and ob.bf_export)
        obs.sort(key=lambda k:k.name) # Alphabetic order by element name
        children.extend(obs)
        # Return
        return BFList(children)

    children = property(_get_children)

    def get_exported(self, context, element=None) -> "bool": # 'element' kept for polymorphism
        """Return True if self is exported to FDS."""
        return True

    def get_my_res(self, context, element, ui=False) -> "BFResult or None":
        """Get my BFResult. On error raise BFException."""
        if not self.get_exported(context, element): return None
        if self.type == "EMPTY" and not ui: msg = "{}".format(self.bf_fyi or "group of namelists")
        else: msg = None
        return BFResult(sender=self, msg=msg)

    def get_res(self, context, element=None, ui=False) -> "BFResult or None": # 'element' kept for polymorphism
        """Get full BFResult (children and mine). On error raise BFException."""
        if DEBUG: print("BFDS: BFObject.get_res:", self.idname)
        return BFCommon.get_res(self, context, self, ui) # 'self' replaces 'element' as reference
    
    def to_fds(self, context=None) -> "str or None":
        """Export me in FDS notation, on error raise BFException."""
        if not context: context = bpy.context
        res = self.get_res(context, self)
        if res: return res.value

def update_ob_bf_namelist(self, context):
    """Update function for object.bf_namelist bpy_prop"""
    # Del all tmp_objects, if self has one
    if self.bf_has_tmp: geometry.del_all_tmp_objects(context)
    # Set all geometries to NONE, as different namelists have different geometric possibilities
    self.bf_xb, self.bf_xyz, self.bf_pb = "NONE", "NONE", "NONE"

# System properties

bpy.types.Object.bf_export = bpy.props.BoolProperty(
    name="Export", description="Set if object is exported to FDS", default=True)
    
bpy.types.Object.bf_namelist = bpy.props.EnumProperty( # link to related BFNamelist
    name="Namelist", description="Type of FDS namelist",
    items=(("bf_obst","OBST","OBST",1000),), default="bf_obst", update=update_ob_bf_namelist) # items are updated later

bpy.types.Object.bf_is_tmp = bpy.props.BoolProperty(
    name="Is Tmp", description="Set if this element is tmp", default=False)

bpy.types.Object.bf_has_tmp = bpy.props.BoolProperty(
    name="Has Tmp", description="Set if this element has a visible tmp element companion", default=False)

bpy.types.Object.bf_fyi = bpy.props.StringProperty(
    name="FYI", description="Object description", maxlen = 128)

## Added methods
# Base    
bpy.types.Object.idname = BFObject.idname
# UI
bpy.types.Object.draw_header = BFObject.draw_header
bpy.types.Object._draw_messages = BFObject._draw_messages
bpy.types.Object._draw_body = BFObject._draw_body
bpy.types.Object._draw_extra = BFObject._draw_extra
bpy.types.Object.draw = BFObject.draw
# Export
bpy.types.Object.get_exported = BFObject.get_exported
bpy.types.Object.children = BFObject.children
bpy.types.Object.descendants = BFObject.descendants
bpy.types.Object._format = BFObject._format
bpy.types.Object._get_children_res = BFObject._get_children_res
bpy.types.Object.get_my_res = BFObject.get_my_res
bpy.types.Object.get_res = BFObject.get_res
bpy.types.Object.to_fds = BFObject.to_fds

### Blender Material <-> BFMaterial <-> FDS SURF

class BFMaterial(BFObject):
    """Extend Blender material type"""
# FIXME def draw...
    def _get_children(self) -> "BFList of Blender elements, never None":
        """Get children: bf_namelist related to self."""
        # Init
        children = list()
        # Get my bf_namelist
        children.append(BFNamelist.bf_list[self.bf_namelist]) # This is an element that has one BFNamelist: Object, Material
        print("Material children:", children) # FIXME
        # Return
        return BFList(children)

    children = property(_get_children)

# System properties

bpy.types.Material.bf_export = bpy.props.BoolProperty(
    name="Export", description="Set if material is exported to FDS", default=True)

bpy.types.Material.bf_namelist = bpy.props.EnumProperty( # link to related BFNamelist
    name="Namelist", description="Type of FDS namelist",
    items=(("bf_surf","SURF","SURF",2000),), default="bf_surf") # items are updated later

bpy.types.Material.bf_fyi = bpy.props.StringProperty(
    name="FYI", description="Material description", maxlen = 128)

# Added methods

bpy.types.Material.idname = BFMaterial.idname
bpy.types.Material.get_exported = BFMaterial.get_exported
bpy.types.Material.children = BFMaterial.children
bpy.types.Material.descendants = BFMaterial.descendants
bpy.types.Material._format = BFMaterial._format
bpy.types.Material._get_children_res = BFMaterial._get_children_res
bpy.types.Material.get_my_res = BFMaterial.get_my_res
bpy.types.Material.get_res = BFMaterial.get_res
bpy.types.Material.to_fds = BFMaterial.to_fds

### Blender Scene <-> BFScene <-> FDS Case

class BFScene(BFObject):
    """Extend bpy.types.scene"""

    # Export (me and children)
    # Override the _format() method for specific formatting
    # Override the get_my_res() method to send informative msgs, special values or raise special BFExceptions
    # The get_my_res() method is also used to draw the same messages and exceptions on the UI panel
    # Override the get_res() method to send informative msgs or raise special BFExceptions
# FIXME def draw...
    def _get_children(self) -> "BFList of Blender elements, never None":
        """Get children: bf_namelists related to Scene, objects in Scene, materials in Scene."""
        # Init
        children = list()
        context = bpy.context
        # Get my bf_namelists
        children.extend([bf_namelist for bf_namelist in BFNamelist.bf_list if bf_namelist.bpy_type == bpy.types.Scene])
        # Get materials (export all not only referenced materials as before)
        mas = list(ma for ma in bpy.data.materials \
            if ma.bf_export and \
            (ma.name not in fds_surf.predefined))
        mas.sort(key=lambda k:k.name) # Alphabetic order by element name
        children.extend(mas)
        # Get objects
        obs = list(ob for ob in context.scene.objects \
            if ob.type in ("MESH", "EMPTY",) and ob.parent == None and ob.bf_export)
        obs.sort(key=lambda k:k.name) # Alphabetic order by element name
        children.extend(obs)
        # Return
        return BFList(children)

    children = property(_get_children)

    def get_my_res(self, context, element, ui=False) -> "BFResult or None":
        """Get my BFResult. On error raise BFException."""
        return BFResult(
            sender = self,
            value = "&TAIL /\n".format(self.idname), # closing namelist
            msgs = (
                "Generated by BlenderFDS {} on Blender {}".format(
                    ".".join(str(x) for x in sys.modules.get("blenderfds").bl_info["version"]),
                    bpy.app.version_string
                ),
                "File: {}".format(bpy.data.filepath),
                "Time: {}".format(time.strftime("%a, %d %b %Y %H:%M:%S", time.localtime())),
            ),
        )

    # FIXME get_res is repeated here only for progress. Find a better solution!

    def get_res(self, context, element, ui=False) -> "BFResult or None":
        """Get full BFResult (children and mine). On error raise BFException."""
        if DEBUG: print("BFDS: BFScene.get_res:", self.idname, element)
        # Init
        my_res = self.get_my_res(context, element, ui)
        if not my_res: return None
        children_res = self._get_children_res(context, element, ui, progress=True)
        # Format value and return
        my_res.value = self._format(context, element, my_res, children_res)
        return my_res

    def to_ge1(self, context):
        """Export my geometry in FDS GE1 notation, on error raise BFException."""
        # File format:
        # [APPEARANCE]  < immutable title
        # 2             < number of appearances (from materials)
        # INERT                     < appearance name
        # 0 200 200 50 0.0 0.0 0.5  < index, red, green, blue, twidth, theight, alpha, tx0, ty0, tz0 (t is texture)
        #                           < texture file, blank if None
        # Burner
        # 1 255 0 0 0.0 0.0 0.5
        #
        # [FACES]       < immutable title
        # 2             < number of quad faces (from OBST and SURF objects tessfaces)
        # 6.0 3.9 0.5 6.0 1.9 0.5 6.0 1.9 1.9 6.0 3.9 1.9 0 < x0, y0, z0, x1, y1, z1, ..., ref to appearance index
        # 6.0 3.9 0.5 6.0 1.9 0.5 6.0 1.9 1.9 6.0 3.9 1.9 0
        # EOF
        # Progress
        progress = True
        if progress:
            wm = context.window_manager
            wm.progress_begin(0, 100)
        # Get GE1 appearances from materials
        appearances = list()
        ma_to_appearance = dict()
        for index, ma in enumerate(bpy.data.materials):
            ma_to_appearance[ma.name] = index
            appearances.append(
                "{desc}\n{i} {r} {g} {b} 0. 0. {alpha:.6f}\n\n".format(
                    desc=ma.name, i=index,
                    r=int(ma.diffuse_color[0]*255), g=int(ma.diffuse_color[1]*255), b=int(ma.diffuse_color[2]*255), 
                    alpha=ma.alpha,
                )
            )
        # Get GE1 gefaces from objects
        if progress: wm.progress_update(20)
        obs = (ob for ob in context.scene.objects if ob.type == "MESH"
            and not ob.hide_render # hide some objects if requested
            and ob.bf_export
            and ob.bf_namelist in ("bf_obst", "bf_vent")
        ) # FIXME bf_hole?
        gefaces = list()
        for ob in obs:
            me = geometry.get_global_mesh(context, ob)
            tessfaces = geometry.get_tessfaces(context, me)
            # Transform ob tessfaces in GE1 gefaces
            appearance_index = str(ma_to_appearance.get(ob.active_material.name, 0)) + "\n"
            for tessface in tessfaces:
                # Get tessface vertices: (x0, y0, z0), (x1, y1, z1), (x2, y2, z2), ... tri or quad
                verts = list(me.vertices[vertex] for vertex in tessface.vertices)
                # Transform tri in quad
                if len(verts) == 3: verts.append(verts[-1])
                # Unzip and format verts, append ref to appearance
                items = ["{:.6f}".format(co) for vert in verts for co in vert.co]
                items.append(appearance_index)
                # Append GE1 face
                gefaces.append(" ".join(items))
        # Prepare GE1 file and return
        if progress: wm.progress_update(90)
        ge1_file_a = "[APPEARANCE]\n{}\n{}".format(len(appearances), "".join(appearances))
        ge1_file_f = "[FACES]\n{}\n{}".format(len(gefaces), "".join(gefaces))
        if progress: wm.progress_end()
        return "".join((ge1_file_a, ge1_file_f))

    # Import

    def from_fds(self, context, value=None, progress=False) -> "None":
        """Import a text in FDS notation into self. On error raise BFException.
        Value is any text in good FDS notation.
        """
        if DEBUG: print("BFDS: BFScene.from_fds:", self.idname, "\n", value) 
        # Init
        from_fds_error = False
        # Progress
        if progress:
            wm = context.window_manager
            wm.progress_begin(0, 100)
        # Try to tokenize value
        try: tokens = fds_to_py.tokenize(value)
        except Exception as err:
            tokens, from_fds_error = list(), True
            custom_text.write(str(err))
        # Prepare custom_text
        custom_text = list()
        # Prepare progress for namelists import
        index_max = len(tokens)
        # Create elements, set their namelists, import parameters
        for index, token in enumerate(tokens):
            # Update progress
            if progress: wm.progress_update(int(index/index_max))
            # Unpack token
            fds_original, fds_label, fds_value = token
            # Search corresponding bf_namelist by name and create related new object
            is_imported = False
            for bf_namelist in BFNamelist.bf_list:
                if bf_namelist.fds_label == fds_label:
                    # Try anticipate element name from ID
                    name = dict((prop[0], prop[1]) for prop in fds_value).get("ID", False)
                    # Namelist found, create or get element depending on the type: Scene, Object, Material
                    if bf_namelist.bpy_type == bpy.types.Scene:
                        element = self
                    elif bf_namelist.bpy_type == bpy.types.Object:
                        if name: element = geometry.get_object(context, name=name) # Try to get existing object
                        else:    element = geometry.get_new_object(context, name="new {}".format(fds_label))
                        element.bf_namelist = bf_namelist.idname # link to found namelist
                    elif bf_namelist.bpy_type == bpy.types.Material:
                        if name: element = geometry.get_material(name=name) # Try to get existing
                        else:    element = geometry.get_material(name="new {}".format(fds_label))
                        element.bf_namelist = "bf_surf" # link to generic SURF namelist
                        element.use_fake_user = True # Blender saves it even if it has no users
                    else: raise ValueError("BFDS: BFScene.from_fds: Unrecognized namelist type '{}'".format(bf_namelist.idname))
                    # Element namelist is still empty, try to call its from_fds() and set its parameters
                    try: bf_namelist.from_fds(context, element, fds_value)
                    except BFException as child_err:
                        from_fds_error = True
                        # Write error from bf_namelist to custom_text
                        custom_text.append("".join(("! ERROR: {}\n".format(msg) for msg in child_err.labels)))
                        # Delete unfinished element, but not myself if element is the scene
                        if bf_namelist.bpy_type == bpy.types.Object:
                            context.scene.objects.unlink(element)
                            bpy.data.objects.remove(element)
                        elif bf_namelist.bpy_type == bpy.types.Material:
                            bpy.data.materials.remove(element)
                    else:
                        if DEBUG: print("BFDS: BFScene.from_fds: imported:\n", fds_original) 
                        is_imported = True # all ok, object created and parameters set
                    break # bf_namelist already found, stop searching by exiting loop
            # If after searching, namelist was not found or parameters could not be set
            if not is_imported:
                # Write original item to custom_text
                custom_text.append(fds_original + "\n")
                if DEBUG: print("BFDS: BFScene.from_fds: to custom text:\n", fds_original) 
        # Write custom_text to self.bf_head_custom_text FIXME clean up
        if custom_text:
            self.bf_head_custom_text = "Imported text"
            bpy.data.texts.new(self.bf_head_custom_text)
            bpy.data.texts[self.bf_head_custom_text].from_string("".join(custom_text))
        # If any exception was raised, inform the parent
        if progress: wm.progress_end()
        if from_fds_error: raise BFException(sender=self)

# System properties: None

# Add methods

bpy.types.Scene.idname = BFScene.idname
bpy.types.Scene.get_exported = BFScene.get_exported
bpy.types.Scene.children = BFScene.children
bpy.types.Scene.descendants = BFScene.descendants
bpy.types.Scene._format = BFScene._format
bpy.types.Scene._get_children_res = BFScene._get_children_res
bpy.types.Scene.get_my_res = BFScene.get_my_res
bpy.types.Scene.get_res = BFScene.get_res
bpy.types.Scene.to_fds = BFScene.to_fds
bpy.types.Scene.to_ge1 = BFScene.to_ge1
bpy.types.Scene.from_fds = BFScene.from_fds
