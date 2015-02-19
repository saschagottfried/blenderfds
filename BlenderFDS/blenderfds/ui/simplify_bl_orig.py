"""BlenderFDS, simplify Blender interface at start"""

import bpy
from bpy.types import Panel, Header

DEBUG = False

def less_space_properties():
    """Simplify SpaceProperties header (less contexts)"""

    print("BFDS: Simplify SpaceProperties header")

    sp_items = (
        ('SCENE','Scene','Scene','SCENE_DATA',1),
        ('OBJECT','Object','Object','OBJECT_DATA',3),
        ('MATERIAL','Material','Material','MATERIAL',5),
        ('MODIFIER','Modifiers','Object modifiers','MODIFIER',10),
    )

    def sp_item_update(self, context):
        if self.bf_sp_context == context.space_data.context: return # no update needed, also for second update
        # print(self.bf_sp_context, context.space_data.context)
        for trial in (self.bf_sp_context,'OBJECT','MATERIAL','SCENE'):  # try several ordered choices
            try: context.space_data.context = trial
            except TypeError: continue
            else:
                self.bf_sp_context = trial
                break

    # Add bf_sp_context 
    bpy.types.WindowManager.bf_sp_context = bpy.props.EnumProperty(items=sp_items, update=sp_item_update)

    # Rewrite the PROPERTIES_HT_header draw method
    def PROPERTIES_HT_header_draw(self, context):
        layout = self.layout
        row = layout.row()
        row.template_header()
        row.prop(context.window_manager, "bf_sp_context", expand=True, icon_only=True)

    bpy.types.PROPERTIES_HT_header.draw = PROPERTIES_HT_header_draw

def low_draw(self, context):
    layout = self.layout
    row = layout.row(align=True)
    row.template_header()

def unregister_unused_classes():
    """Unregister unused classes, kill unused headers (eg. panels)"""

    print("BFDS: Unregister unused classes")
 
    # Init
    always_allowed_bl_space_type = "TEXTEDITOR", "TEXT_EDITOR", "USER_PREFERENCES", "CONSOLE", "FILE_BROWSER", "INFO", "OUTLINER"
    allowed_panels = 'Panel', 'SCENE_PT_BF_DUMP', 'SCENE_PT_BF_HEAD', 'SCENE_PT_BF_MISC', 'SCENE_PT_BF_REAC', 'SCENE_PT_BF_TIME', 'OBJECT_PT_BF', 'OBJECT_PT_context_object', 'MATERIAL_PT_BF', 'MATERIAL_PT_context_material', 'DATA_PT_modifiers'
    allowed_headers = 'Header', 'PROPERTIES_HT_header', 'VIEW3D_HT_header'

    allowed_bl_space_type = "PROPERTIES", "VIEW_3D"
    allowed_bl_category = "Tools", "Create", "Relations", "Options", "Grease Pencil"
    allowed_bl_region_type = "UI"

    # Search and unregister
    for bpy_type in dir(bpy.types):
        bpy_type_class = getattr(bpy.types, bpy_type)

        # Get class attributes
        bl_space_type = getattr(bpy_type_class, "bl_space_type", None)    
        bl_context = getattr(bpy_type_class, "bl_context", None)
        bl_category = getattr(bpy_type_class, "bl_category", None)
        bl_region_type = getattr(bpy_type_class, "bl_region_type", None)

        # Always ok
        if bl_space_type in always_allowed_bl_space_type: continue

        # Is it a panel?
        if issubclass(bpy_type_class, Panel):
            # Is it allowed?
            if bpy_type in allowed_panels: continue
            if bl_space_type in allowed_bl_space_type:
                if bl_category and bl_category in allowed_bl_category: continue
                if bl_region_type and bl_region_type in allowed_bl_region_type: continue
            if DEBUG: print("BFDS: Panel:", bl_space_type, bl_region_type, bl_context, bl_category, bpy_type)
            bpy.utils.unregister_class(bpy_type_class)
            
        # Is it a menu?
        elif issubclass(bpy_type_class, Header):
            if bpy_type in allowed_headers: continue
            if DEBUG: print("BFDS: Header:", bl_space_type, bl_region_type, bl_context, bl_category, bpy_type)
            bpy_type_class.draw = low_draw




















    unused_panels = ["SCENE_PT_scene", "SCENE_PT_keying_sets", "SCENE_PT_keying_set_paths", "SCENE_PT_color_management", "SCENE_PT_audio", "SCENE_PT_physics", "SCENE_PT_rigid_body_world", "SCENE_PT_rigid_body_cache", "SCENE_PT_rigid_body_field_weights", "SCENE_PT_simplify","SCENE_PT_custom_props",]
    unused_panels += ["OBJECT_PT_transform", "OBJECT_PT_delta_transform", "OBJECT_PT_transform_locks", "OBJECT_PT_relations", "GROUP_MT_specials", "OBJECT_PT_groups", "OBJECT_PT_display", "OBJECT_PT_duplication", "OBJECT_PT_relations_extras", "OBJECT_PT_motion_paths", "OBJECT_PT_onion_skinning", "OBJECT_PT_custom_props", ]
    unused_panels += ["MATERIAL_PT_preview", "MATERIAL_PT_diffuse", "MATERIAL_PT_specular", "MATERIAL_PT_shading", "MATERIAL_PT_transp", "MATERIAL_PT_mirror", "MATERIAL_PT_sss", "MATERIAL_PT_halo", "MATERIAL_PT_flare", "MATERIAL_PT_game_settings", "MATERIAL_PT_physics", "MATERIAL_PT_strand", "MATERIAL_PT_options", "MATERIAL_PT_shadow", "MATERIAL_PT_transp_game", "MATERIAL_PT_volume_density", "MATERIAL_PT_volume_shading", "MATERIAL_PT_volume_lighting", "MATERIAL_PT_volume_transp", "MATERIAL_PT_volume_integration", "MATERIAL_PT_volume_options", "MATERIAL_PT_custom_props"]

    for panel in unused_panels:
        try: bpy.utils.unregister_class(getattr(bpy.types,panel))
        except: pass

