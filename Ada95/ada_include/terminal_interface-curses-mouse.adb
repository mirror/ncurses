------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                     Terminal_Interface.Curses.Mouse                      --
--                                                                          --
--                                 B O D Y                                  --
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
--  $Revision: 1.3 $
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

begin
   if Generation_Bit_Order /= System.Default_Bit_Order then
      raise Constraint_Error;
   end if;
end Terminal_Interface.Curses.Mouse;
