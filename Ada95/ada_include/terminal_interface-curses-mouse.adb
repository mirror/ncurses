------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                     Terminal_Interface.Curses.Mouse                      --
--                                                                          --
--                                 B O D Y                                  --
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
--  $Revision: 1.9 $
--  Binding Version 00.93
------------------------------------------------------------------------------
with System;

with Terminal_Interface.Curses.Aux; use Terminal_Interface.Curses.Aux;
with Interfaces;
with Interfaces.C;
with Unchecked_Conversion;

package body Terminal_Interface.Curses.Mouse is

   use type System.Bit_Order;
   use type Interfaces.C.int;

   function CInt_To_Mask is new
     Unchecked_Conversion (Source => C_Int,
                           Target => Event_Mask);

   function Mask_To_CInt is new
     Unchecked_Conversion (Source => Event_Mask,
                           Target => C_Int);

   function Has_Mouse return Boolean
   is
      function Mouse_Avail return C_Int;
      pragma Import (C, Mouse_Avail, "_nc_has_mouse");
   begin
      if Has_Key (Key_Mouse) or else Mouse_Avail /= 0 then
         return True;
      else
         return False;
      end if;
   end Has_Mouse;

   function Get_Mouse return Mouse_Event
   is
      type Event_Access is access all Mouse_Event;

      function Getmouse (Ev : Event_Access) return C_Int;
      pragma Import (C, Getmouse, "getmouse");

      Event : aliased Mouse_Event;
   begin
      if Getmouse (Event'Access) = Curses_Err then
         raise Curses_Exception;
      end if;
      return Event;
   end Get_Mouse;

   procedure Register_Reportable_Event (B    : in Mouse_Button;
                                        S    : in Button_State;
                                        Mask : in out Event_Mask)
   is
      type Evt_Access is access all Event_Mask;
      function Register (B : C_Int;
                         S : C_Int;
                         M : Evt_Access) return C_Int;
      pragma Import (C, Register, "_nc_ada_mouse_mask");

      T : aliased Event_Mask := Mask;
      M : Evt_Access := T'Access;
      R : constant C_Int := Register (C_Int (Mouse_Button'Pos (B)),
                                      C_Int (Button_State'Pos (S)),
                                      M);
   begin
      if R = Curses_Err then
         raise Curses_Exception;
      end if;
      Mask := T;
   end Register_Reportable_Event;

   function Start_Mouse (Mask : Event_Mask := All_Events)
                         return Event_Mask
   is
      type Int_Access is access all C_Int;
      function MMask (M : C_Int; O : Int_Access := null) return C_Int;
      pragma Import (C, MMask, "mousemask");
      R : C_Int;
   begin
      R := MMask (Mask_To_CInt (Mask));
      return CInt_To_Mask (R);
   end Start_Mouse;

   procedure End_Mouse
   is
      Old : constant Event_Mask := Start_Mouse (No_Events);
   begin
      null;
   end End_Mouse;

   procedure Get_Event (Event  : in  Mouse_Event;
                        Y      : out Line_Position;
                        X      : out Column_Position;
                        Button : out Mouse_Button;
                        State  : out Button_State)
   is
      procedure Dispatch_Event (M : in C_Int;
                                B : out C_Int;
                                S : out C_Int);
      pragma Import (C, Dispatch_Event, "_nc_ada_mouse_event");

      Mask  : constant Interfaces.C.int := Mask_To_CInt (Event.Bstate);
      B, S  : C_Int;
   begin
      X := Column_Position (Event.X);
      Y := Line_Position   (Event.Y);
      Dispatch_Event (Mask, B, S);
      Button := Mouse_Button'Val (B);
      State  := Button_State'Val (S);
   end Get_Event;

   procedure Unget_Mouse (Event : in Mouse_Event)
   is
      function Ungetmouse (Ev : Mouse_Event) return C_Int;
      pragma Import (C, Ungetmouse, "ungetmouse");
   begin
      if Ungetmouse (Event) = Curses_Err then
         raise Curses_Exception;
      end if;
   end Unget_Mouse;

   function Enclosed_In_Window (Win    : Window := Standard_Window;
                                Event  : Mouse_Event) return Boolean
   is
      function Wenclose (Win : Window; Y : C_Int; X : C_Int) return C_Int;
      pragma Import (C, Wenclose, "wenclose");
   begin
      if Wenclose (Win, C_Int (Event.Y), C_Int (Event.X)) = Curses_False then
         return False;
      else
         return True;
      end if;
   end Enclosed_In_Window;

   function Mouse_Interval (Msec : Natural := 200) return Natural
   is
      function Mouseinterval (Msec : C_Int) return C_Int;
      pragma Import (C, Mouseinterval, "mouseinterval");
   begin
      return Natural (Mouseinterval (C_Int (Msec)));
   end Mouse_Interval;

end Terminal_Interface.Curses.Mouse;
