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
--  Version 00.92                                                           --
--                                                                          --
--  The ncurses Ada95 binding is copyrighted 1996 by                        --
--  Juergen Pfeifer, Email: Juergen.Pfeifer@T-Online.de                     --
--                                                                          --
--  Permission is hereby granted to reproduce and distribute this           --
--  binding by any means and for any fee, whether alone or as part          --
--  of a larger distribution, in source or in binary form, PROVIDED         --
--  this notice is included with any such distribution, and is not          --
--  removed from any of its header files. Mention of ncurses and the        --
--  author of this binding in any applications linked with it is            --
--  highly appreciated.                                                     --
--                                                                          --
--  This binding comes AS IS with no warranty, implied or expressed.        --
------------------------------------------------------------------------------
--  Version Control:
--  $Revision: 1.6 $
------------------------------------------------------------------------------
include(`Mouse_Base_Defs')
with System;

package Terminal_Interface.Curses.Mouse is

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

   procedure Register_Reportable_Event
     (B    : in Mouse_Button;
      S    : in Button_State;
      Mask : in out Event_Mask);
   --  Stores the event described by the button and the state in the mask.
   --  Before you call this the first time, you should init the mask
   --  with the Empty_Mask constant

   --  ANCHOR(`mousemask()',`Start_Mouse')
   function Start_Mouse (Mask : Event_Mask := All_Events)
                         return Event_Mask;
   --  AKA

   procedure End_Mouse;
   pragma Import (C, End_Mouse, "_nc_ada_unregister_mouse");
   --  Terminates the mouse

   --  ANCHOR(`getmouse()',`Get_Mouse')
   function Get_Mouse return Mouse_Event;
   --  AKA

   procedure Get_Event (Event  : in  Mouse_Event;
                        Y      : out Line_Position;
                        X      : out Column_Position;
                        Button : out Mouse_Button;
                        State  : out Button_State);
   --  !!! Warning: X and Y are screen coordinates. Due to ripped of lines they
   --  may not be identical to window coordinates.

   --  ANCHOR(`ungetmouse()',`Unget_Mouse')
   procedure Unget_Mouse (Event : in Mouse_Event);
   --  AKA

   --  ANCHOR(`wenclose()',`Enclosed_In_Window')
   function Enclosed_In_Window (Win    : Window := Standard_Window;
                                Event  : Mouse_Event) return Boolean;
   --  AKA
   --  But : use event instead of screen coordinates.

   --  ANCHOR(`mouseinterval()',`Mouse_Interval')
   function Mouse_Interval (Msec : Natural := 200) return Natural;
   --  AKA

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
