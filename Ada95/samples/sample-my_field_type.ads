------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                           Sample.My_Field_Type                           --
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
--  Version Control
--  $Revision: 1.3 $
------------------------------------------------------------------------------
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Terminal_Interface.Curses.Forms.Field_Types;

--  This is a very simple user defined field type. It accepts only a
--  defined character as input into the field.
--
package Sample.My_Field_Type is

   type My_Data is new Ada_Defined_Field_Type with
      record
        Ch : Character;
      end record;
   type My_Access is access My_Data;

private

   function F_Check (Fld : Field;     Info : My_Access) return Boolean;
   function C_Check (Ch  : Character; Info : My_Access) return Boolean;

   package One_Character_Only is new
     Terminal_Interface.Curses.Forms.Field_Types
     (My_Data, My_Access, F_Check, C_Check);

end Sample.My_Field_Type;

