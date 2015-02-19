import bpy
from bpy.types import Header

ctx_items = (
    ('SCENE','Scene','Scene','SCENE_DATA',1),
    ('OBJECT','Object','Object','OBJECT_DATA',3),
    ('MATERIAL','Material','Material','MATERIAL',5),
    ('MODIFIER','Modifiers','Object modifiers','MODIFIER',10),
)

def ctx_update_cb(self, context):
    if self.my_context == context.space_data.context: return # no update needed, also for second update
    for trial in (self.my_context,'OBJECT','MATERIAL','SCENE'):  # try several ordered choices
        try: context.space_data.context = trial
        except TypeError: continue
        else:
            self.my_context = trial
            break

def new_properties_draw(self, context):
    layout = self.layout
    row = layout.row()
    row.template_header()
    row.prop(context.window_manager, "my_context", expand=True, icon_only=True)

bpy.types.PROPERTIES_HT_header.draw = new_properties_draw

def new_mat_draw(self, context):
    layout = self.layout
    mat = context.material
    ob = context.object
    space = context.space_data
    if ob:
        row = layout.row()
        row.template_ID(ob, "active_material", new="material.new")
    elif mat:
        row = layout.row()
        row.template_ID(space, "pin_id")

bpy.types.MATERIAL_PT_context_material.draw = new_mat_draw

unused_panels = ("SCENE_PT_scene", "SCENE_PT_keying_sets", "SCENE_PT_keying_set_paths", "SCENE_PT_color_management", "SCENE_PT_audio", "SCENE_PT_physics", "SCENE_PT_rigid_body_world", "SCENE_PT_rigid_body_cache", "SCENE_PT_rigid_body_field_weights", "SCENE_PT_simplify","SCENE_PT_custom_props", "OBJECT_PT_motion_paths", "OBJECT_PT_custom_props", "MATERIAL_PT_preview", "MATERIAL_PT_diffuse", "MATERIAL_PT_specular", "MATERIAL_PT_shading", "MATERIAL_PT_transp", "MATERIAL_PT_mirror", "MATERIAL_PT_sss", "MATERIAL_PT_halo", "MATERIAL_PT_flare", "MATERIAL_PT_game_settings", "MATERIAL_PT_physics", "MATERIAL_PT_strand", "MATERIAL_PT_options", "MATERIAL_PT_shadow", "MATERIAL_PT_transp_game", "MATERIAL_PT_volume_density", "MATERIAL_PT_volume_shading", "MATERIAL_PT_volume_lighting", "MATERIAL_PT_volume_transp", "MATERIAL_PT_volume_integration", "MATERIAL_PT_volume_options", "MATERIAL_PT_custom_props")

def register():
    # Unregister unused panels
    for panel in unused_panels:
        try: bpy.utils.unregister_class(getattr(bpy.types,panel))
        except: pass
    # Add my_context    
    bpy.types.WindowManager.my_context = bpy.props.EnumProperty(items=ctx_items, update=ctx_update_cb)

def unregister():
    # Show unused panels need reboot
    # Del my_context
    del bpy.types.WindowManager.my_context

if __name__ == "__main__":
    register()
