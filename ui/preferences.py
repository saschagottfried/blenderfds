"""BlenderFDS, preferences panel"""

import bpy

# Get preference value like this:
# context.user_preferences.addons["blenderfds"].preferences.bf_debug

class BFPreferences(bpy.types.AddonPreferences):
    bl_idname = "blenderfds"

    bf_pref_show_debug = bpy.props.BoolProperty(
            name="Debug Messages",
            description="Print debug messages on the console",
            default=False,
            )

    bf_pref_show_info = bpy.props.BoolProperty(
            name="Info Messages",
            description="Print info messages on the console",
            default=True,
            )

    def draw(self, context):
        layout = self.layout
        layout.prop(self, "bf_pref_show_debug")
        layout.prop(self, "bf_pref_show_info")
