------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                           Sample.My_Field_Type                           --
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
--  Version Control
--  $Revision: 1.2 $
------------------------------------------------------------------------------
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;

--  This is a very simple user defined field type. It accepts only a
--  defined character as input into the field.
--
package body Sample.My_Field_Type is

   --  That's simple. There are no field validity checks.
   function F_Check (Fld : Field; Info : My_Access) return Boolean
   is
   begin
      return True;
   end F_Check;

   --  Check exactly against the specified character.
   function C_Check (Ch : Character; Info : My_Access) return Boolean
   is
      C : constant Character := Info.Ch;
   begin
      return Ch = C;
   end C_Check;

end Sample.My_Field_Type;
