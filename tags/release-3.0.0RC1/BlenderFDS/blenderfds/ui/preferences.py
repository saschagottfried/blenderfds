"""BlenderFDS, preferences panel"""

import bpy

# Get preference value like this:
# bpy.context.user_preferences.addons["blenderfds"].preferences.bf_debug

# FUTURE: to be used with Blender 2.70

#class BFPreferences(bpy.types.AddonPreferences):
#    bl_idname = "blenderfds"

#    bf_pref_show_info = bpy.props.BoolProperty(
#            name="Info Messages",
#            description="Print info messages on the console",
#            default=True,
#            )

#    def draw(self, context):
#        layout = self.layout
#        row = layout.row()
#        row.prop(self, "bf_pref_show_info")
#        row = layout.row()
#        row.operator("wm.bf_set_bf_homefile")
#        row.label(" ")

