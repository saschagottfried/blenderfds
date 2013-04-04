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
"""BlenderFDS, an open tool for the NIST Fire Dynamics Simulator"""

import bpy, blf

class BFOsd():
    """Hackish on screen display"""
    _msg = ""
        
    def __init__(self):
        self._handle = bpy.types.SpaceView3D.draw_handler_add(self._draw_handler, tuple(), 'WINDOW', 'POST_PIXEL')
        
    def remove(self):
        """Remove osd"""
        if not self._handle: return
        bpy.types.SpaceView3D.draw_handler_remove(self._handle, 'WINDOW')
        bpy.ops.wm.redraw_timer(type='DRAW_WIN_SWAP', iterations=1)

    def _draw_handler(self):
        """Draw on screen handler"""
        blf.position(0,70,30,0)
        blf.size(0,20,72)
        blf.draw(0,self._msg)
          
    def show(self,msg):
        """Show message"""
        self._msg = str(msg)
        bpy.ops.wm.redraw_timer(type='DRAW_WIN_SWAP', iterations=1)

    def clean(self):
        """Clean osd"""
        self._msg = str()
        bpy.ops.wm.redraw_timer(type='DRAW_WIN_SWAP', iterations=1)
        
# Global name
bf_osd = BFOsd()
