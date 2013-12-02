"""BlenderFDS, extended Blender types"""

import bpy
from blenderfds.types.results import BFResult, BFException
from blenderfds.types.collections import BFList, BFAutoItem
from blenderfds.types.interfaces import BFCommon, BFNamelist
from blenderfds.lib import geometry, fds_surf, fds_to_py

DEBUG = True

### Blender Object <-> BFObject <-> FDS geometric entity (eg. OBST, VENT, HOLE...)

class BFObject(BFCommon):
    """Extend Blender object type"""

    def _get_idname(self) -> "str": return self.name

    idname = property(_get_idname)

    # UI
    # Panel generation is handled in ui/panels.py
    # draw_messages() is inherited

    # Export (me and children)

    def _get_children(self) -> "BFList of BFNamelist and Blender objects, never None":
        """Get children: bf_namelist related to self, children objects."""
        # Init
        children = list()
        context = bpy.context
        # Get my bf_namelist, if self is a MESH
        if self.type == "MESH":
            # This is an element that has one BFNamelist: Object, Material
            children.append(BFNamelist.bf_list[self.bf_namelist]) 
        # Get children objects
        obs = list(ob for ob in context.scene.objects \
            if ob.type in ("MESH", "EMPTY",) and ob.parent == self and ob.bf_export)
        # Alphabetic order by element name
        obs.sort(key=lambda k:k.name)
        children.extend(obs)
        return BFList(children)

    children = property(_get_children)

    def get_exported(self, context, element=None) -> "bool": # 'element' kept for polymorphism
        """Return True if self is exported to FDS."""
        return True

    def get_my_res(self, context, element, ui=False) -> "BFResult or None": # 'element' kept for polymorphism
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

# Add methods to original Blender type

bpy.types.Object.idname = BFObject.idname
bpy.types.Object.draw_messages = BFObject.draw_messages
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

    # UI
    # Panel generation is handled in ui/panels.py
    # draw_messages() is inherited

    # Export

    def _get_children(self) -> "BFList of Blender elements, never None":
        """Get children: bf_namelist related to self."""
        children = list()
        # This is an element that has one BFNamelist: Object, Material
        children.append(BFNamelist.bf_list[self.bf_namelist]) 
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

# Add methods to original Blender type

bpy.types.Material.idname = BFMaterial.idname
bpy.types.Material.draw_messages = BFMaterial.draw_messages
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

    # UI
    # Panel generation is handled in ui/panels.py
    # draw_messages() is inherited

    # Export

    def _get_children(self) -> "BFList of Blender elements, never None":
        """Get children: bf_namelists related to Scene, objects in Scene, materials in Scene."""
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

    def _get_children_res(self, context, element, ui=False, progress=False) -> "BFList of BFResult, never None":
        return BFObject._get_children_res(self, context, element, ui, True) # set progress to True
            
    def get_my_res(self, context, element, ui=False) -> "BFResult or None":
        """Get my BFResult. On error raise BFException."""
        if ui: return None # No msg
        return BFResult(sender=self, value="&TAIL /\n") # closing namelist

    def to_ge1(self, context):
        """Export my geometry in FDS GE1 notation, on error raise BFException."""
        # File format:
        # [APPEARANCE]  < immutable title
        # 2             < number of appearances (from Blender materials)
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
        obs = (ob for ob in context.scene.objects if ob.type == "MESH"
            and not ob.hide_render # hide some objects if requested
            and ob.bf_export
            and ob.bf_namelist in ("bf_obst", "bf_vent")
        ) # FIXME bf_hole is not cut
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
        ge1_file_a = "[APPEARANCE]\n{}\n{}".format(len(appearances), "".join(appearances))
        ge1_file_f = "[FACES]\n{}\n{}".format(len(gefaces), "".join(gefaces))
        return "".join((ge1_file_a, ge1_file_f))

    # Import

# FIXME import unknown namelists that have XB, XYZ or PB to custom namelist
# FIXME cleanup

    def from_fds(self, context, value=None, progress=False) -> "None":
        """Import a text in FDS notation into self. On error raise BFException.
        Value is any text in good FDS notation.
        """
        if DEBUG: print("BFDS: BFScene.from_fds:", self.idname, "\n", value)
        # Init
        custom_texts = list()
        is_error = False
        # Tokenize value and manage exception
        try: tokens = fds_to_py.tokenize(value)
        except Exception as err:
            tokens, is_error = list(), True
            custom_texts.append(str(err))
        # Handle tokens: create corresponding elements
        for token in tokens:
            # Unpack
            fds_original, fds_label, fds_value = token
            fds_props = dict((prop[1], prop[2]) for prop in fds_value)
            print(fds_props) # FIXME
            fds_id = fds_props.get("ID", "new {}".format(fds_label))
            # Search right bf_namelist
            for bf_namelist in BFNamelist.bf_list:
                if bf_namelist.fds_label == fds_label:
                    # Create Scene, Object, or Material?
                    if bf_namelist.bpy_type == bpy.types.Scene:
                        element = self
                    elif bf_namelist.bpy_type == bpy.types.Object:
                        element = geometry.get_object(context, name=fds_id)
                        element.bf_namelist = bf_namelist.idname # link to found namelist
                    elif bf_namelist.bpy_type == bpy.types.Material:
                        element = geometry.get_material(name=fds_id)
                        element.bf_namelist = "bf_surf" # link to generic SURF namelist
                        element.use_fake_user = True # Blender saves it even if it has no users
                    else: raise ValueError("BFDS: BFScene.from_fds: Unrecognized namelist type")
                    # Once found, stop searching
                    break
            # If still no element, check if custom namelist is applicable
            # check if token is geometric and deserves a custom namelist
            if not element and ("XB" in fds_props or "XYZ" in fds_props or "PBX" in fds_props or "PBY" in fds_props or "PBZ" in fds_props):
                element = geometry.get_object(context, name=fds_id)
                element.bf_namelist = "bf_custom"
                setattr(element, bf_namelist.bf_prop_custom.bpy_idname, fds_label) # FIXME simplify
            # If element
            if element:
                try: bf_namelist.from_fds(context, element, fds_value)
                except BFException as child_err:
                    if DEBUG: print("BFDS: BFScene.from_fds: to custom text:\n", fds_original)
                    custom_texts.append("".join(("! ERROR: {}\n".format(msg) for msg in child_err.labels)))
                    # Delete unfinished element, but not myself if element is the scene
                    if bf_namelist.bpy_type == bpy.types.Object:
                        context.scene.objects.unlink(element)
                        bpy.data.objects.remove(element)
                    elif bf_namelist.bpy_type == bpy.types.Material:
                        bpy.data.materials.remove(element)                    
                    continue # to next token


            # Element namelist is still empty, try to call its from_fds() and set its parameters
            try: bf_namelist.from_fds(context, element, fds_value)
            except BFException as child_err:
                from_fds_error = True
                # Write error from bf_namelist to custom_texts
                custom_texts.append("".join(("! ERROR: {}\n".format(msg) for msg in child_err.labels)))

            else:
                if DEBUG: print("BFDS: BFScene.from_fds: imported:\n", fds_original) 
                is_token_imported = True # all ok, object created and parameters set
            break # bf_namelist already found, stop searching by exiting loop



        # for token in tokens:
        # prepare parameter dict 
        # If token has not XB, XYZ, PB: put in custom text, continue
        # else:
        # get ID
        #   create geometric element
        # else put in custom_texts
        # 
        
        
        
        # Init
        from_fds_error = False
        custom_texts = list()
        # Progress
        if progress:
            wm = context.window_manager
            wm.progress_begin(0, 100)
        # Try to tokenize value
        try: tokens = fds_to_py.tokenize(value)
        except Exception as err:
            tokens, from_fds_error = list(), True
            custom_texts.append(str(err))
        # Prepare progress for namelists import
        if progress: index_max = len(tokens)
        # Create elements, set their namelists, import parameters
        for index, token in enumerate(tokens):
            if progress: wm.progress_update(int(index/index_max))
            fds_original, fds_label, fds_value = token
            # Search corresponding bf_namelist by name and create related new object
            is_token_imported = False
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
                        # Write error from bf_namelist to custom_texts
                        custom_texts.append("".join(("! ERROR: {}\n".format(msg) for msg in child_err.labels)))
                        # Delete unfinished element, but not myself if element is the scene
                        if bf_namelist.bpy_type == bpy.types.Object:
                            context.scene.objects.unlink(element)
                            bpy.data.objects.remove(element)
                        elif bf_namelist.bpy_type == bpy.types.Material:
                            bpy.data.materials.remove(element)
                    else:
                        if DEBUG: print("BFDS: BFScene.from_fds: imported:\n", fds_original) 
                        is_token_imported = True # all ok, object created and parameters set
                    break # bf_namelist already found, stop searching by exiting loop
            # If after searching, namelist was not found or parameters could not be set
            if not is_token_imported:
                # Write original item to custom_texts
                custom_texts.append(fds_original + "\n")
                if DEBUG: print("BFDS: BFScene.from_fds: to custom text:\n", fds_original) 
        # Write custom_texts to self.bf_head_custom_text FIXME clean up
        if custom_texts:
            self.bf_head_custom_text = "Imported text"
            bpy.data.texts.new(self.bf_head_custom_text)
            bpy.data.texts[self.bf_head_custom_text].from_string("".join(custom_texts))
        # If any exception was raised, inform the parent
        if progress: wm.progress_end()
        if from_fds_error: raise BFException(sender=self)


    def from_fds_orig(self, context, value=None, progress=False) -> "None":
        """Import a text in FDS notation into self. On error raise BFException.
        Value is any text in good FDS notation.
        """
        if DEBUG: print("BFDS: BFScene.from_fds:", self.idname, "\n", value) 
        # Init
        from_fds_error = False
        custom_texts = list()
        # Progress
        if progress:
            wm = context.window_manager
            wm.progress_begin(0, 100)
        # Try to tokenize value
        try: tokens = fds_to_py.tokenize(value)
        except Exception as err:
            tokens, from_fds_error = list(), True
            custom_texts.append(str(err))
        # Prepare progress for namelists import
        if progress: index_max = len(tokens)
        # Create elements, set their namelists, import parameters
        for index, token in enumerate(tokens):
            if progress: wm.progress_update(int(index/index_max))
            fds_original, fds_label, fds_value = token
            # Search corresponding bf_namelist by name and create related new object
            is_token_imported = False
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
                        # Write error from bf_namelist to custom_texts
                        custom_texts.append("".join(("! ERROR: {}\n".format(msg) for msg in child_err.labels)))
                        # Delete unfinished element, but not myself if element is the scene
                        if bf_namelist.bpy_type == bpy.types.Object:
                            context.scene.objects.unlink(element)
                            bpy.data.objects.remove(element)
                        elif bf_namelist.bpy_type == bpy.types.Material:
                            bpy.data.materials.remove(element)
                    else:
                        if DEBUG: print("BFDS: BFScene.from_fds: imported:\n", fds_original) 
                        is_token_imported = True # all ok, object created and parameters set
                    break # bf_namelist already found, stop searching by exiting loop
            # If after searching, namelist was not found or parameters could not be set
            if not is_token_imported:
                # Write original item to custom_texts
                custom_texts.append(fds_original + "\n")
                if DEBUG: print("BFDS: BFScene.from_fds: to custom text:\n", fds_original) 
        # Write custom_texts to self.bf_head_custom_text FIXME clean up
        if custom_texts:
            self.bf_head_custom_text = "Imported text"
            bpy.data.texts.new(self.bf_head_custom_text)
            bpy.data.texts[self.bf_head_custom_text].from_string("".join(custom_texts))
        # If any exception was raised, inform the parent
        if progress: wm.progress_end()
        if from_fds_error: raise BFException(sender=self)

# System properties: None

# Add methods to original Blender type

bpy.types.Scene.idname = BFScene.idname
bpy.types.Scene.draw_messages = BFScene.draw_messages
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
