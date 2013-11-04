"""BlenderFDS, preferences panel"""

import bpy

# Get preference value like this:
# bpy.context.user_preferences.addons["blenderfds"].preferences.bf_debug

class BFPreferences(bpy.types.AddonPreferences):
    bl_idname = "blenderfds"

    bf_pref_show_info = bpy.props.BoolProperty(
            name="Info Messages",
            description="Print info messages on the console",
            default=True,
            )

    def draw(self, context):
        layout = self.layout
        layout.prop(self, "bf_pref_show_info")
