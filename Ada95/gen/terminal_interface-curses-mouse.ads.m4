--  -*- ada -*-
define(`HTMLNAME',`terminal_interface-curses-mouse_s.html')dnl
include(M4MACRO)dnl
------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                      Terminal_Interface.Curses.Mouse                     --
--                                                                          --
--                                 S P E C                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright (c) 1998 Free Software Foundation, Inc.                        --
--                                                                          --
-- Permission is hereby granted, free of charge, to any person obtaining a  --
-- copy of this software and associated documentation files (the            --
-- "Software"), to deal in the Software without restriction, including      --
-- without limitation the rights to use, copy, modify, merge, publish,      --
-- distribute, distribute with modifications, sublicense, and/or sell       --
-- copies of the Software, and to permit persons to whom the Software is    --
-- furnished to do so, subject to the following conditions:                 --
--                                                                          --
-- The above copyright notice and this permission notice shall be included  --
-- in all copies or substantial portions of the Software.                   --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS  --
-- OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF               --
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   --
-- IN NO EVENT SHALL THE ABOVE COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,   --
-- DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR    --
-- OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR    --
-- THE USE OR OTHER DEALINGS IN THE SOFTWARE.                               --
--                                                                          --
-- Except as contained in this notice, the name(s) of the above copyright   --
-- holders shall not be used in advertising or otherwise to promote the     --
-- sale, use or other dealings in this Software without prior written       --
-- authorization.                                                           --
------------------------------------------------------------------------------
--  Author: Juergen Pfeifer <Juergen.Pfeifer@T-Online.de> 1996
--  Version Control:
--  $Revision: 1.12 $
--  Binding Version 00.93
------------------------------------------------------------------------------
include(`Mouse_Base_Defs')
with System;

package Terminal_Interface.Curses.Mouse is
   pragma Preelaborate (Mouse);

   --  MANPAGE(`curs_mouse.3x')
   --  Please note, that in ncurses-1.9.9e documentation mouse support
   --  is still marked as experimental. So also this binding will change
   --  if the ncurses methods change.
   --
   type Event_Mask is private;
   No_Events  : constant Event_Mask;
   All_Events : constant Event_Mask;

   type Mouse_Button is (Left,     -- aka: Button 1
                         Middle,   -- aka: Button 2
                         Right,    -- aka: Button 3
                         Button4,  -- aka: Button 4
                         Control,  -- Control Key
                         Shift,    -- Shift Key
                         Alt);     -- ALT Key

   type Button_State is (Released,
                         Pressed,
                         Clicked,
                         Double_Clicked,
                         Triple_Clicked);

   type Mouse_Event is private;

   --  MANPAGE(`curs_mouse.3x')

   function Has_Mouse return Boolean;
   --  Return true if a mouse device is supported, false otherwise.

   procedure Register_Reportable_Event
     (B    : in Mouse_Button;
      S    : in Button_State;
      Mask : in out Event_Mask);
   --  Stores the event described by the button and the state in the mask.
   --  Before you call this the first time, you should init the mask
   --  with the Empty_Mask constant
   pragma Inline (Register_Reportable_Event);

   --  ANCHOR(`mousemask()',`Start_Mouse')
   function Start_Mouse (Mask : Event_Mask := All_Events)
                         return Event_Mask;
   --  AKA
   pragma Inline (Start_Mouse);

   procedure End_Mouse;
   --  Terminates the mouse
   pragma Inline (End_Mouse);

   --  ANCHOR(`getmouse()',`Get_Mouse')
   function Get_Mouse return Mouse_Event;
   --  AKA
   pragma Inline (Get_Mouse);

   procedure Get_Event (Event  : in  Mouse_Event;
                        Y      : out Line_Position;
                        X      : out Column_Position;
                        Button : out Mouse_Button;
                        State  : out Button_State);
   --  !!! Warning: X and Y are screen coordinates. Due to ripped of lines they
   --  may not be identical to window coordinates.
   pragma Inline (Get_Event);

   --  ANCHOR(`ungetmouse()',`Unget_Mouse')
   procedure Unget_Mouse (Event : in Mouse_Event);
   --  AKA
   pragma Inline (Unget_Mouse);

   --  ANCHOR(`wenclose()',`Enclosed_In_Window')
   function Enclosed_In_Window (Win    : Window := Standard_Window;
                                Event  : Mouse_Event) return Boolean;
   --  AKA
   --  But : use event instead of screen coordinates.
   pragma Inline (Enclosed_In_Window);

   --  ANCHOR(`mouseinterval()',`Mouse_Interval')
   function Mouse_Interval (Msec : Natural := 200) return Natural;
   --  AKA
   pragma Inline (Mouse_Interval);

private
   type Event_Mask is new Interfaces.C.int;
   No_Events  : constant Event_Mask := 0;
   All_Events : constant Event_Mask := -1;

   type Mouse_Event is
      record
         Id      : Integer range Integer (Interfaces.C.short'First) ..
                                 Integer (Interfaces.C.Short'Last);
         X, Y, Z : Integer range Integer (Interfaces.C.int'First) ..
                                 Integer (Interfaces.C.int'Last);
         Bstate  : Event_Mask;
      end record;
   pragma Convention (C, Mouse_Event);
   pragma Pack (Mouse_Event);

include(`Mouse_Event_Rep')
   Generation_Bit_Order : constant System.Bit_Order := System.M4_BIT_ORDER;
   --  This constant may be different on your system.

end Terminal_Interface.Curses.Mouse;
