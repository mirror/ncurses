------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--              Terminal_Interface.Curses.Forms.Choice_Field_Types          --
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
--  $Revision: 1.5 $
------------------------------------------------------------------------------
with Interfaces.C;
with Terminal_Interface.Curses.Aux; use Terminal_Interface.Curses.Aux;
with Terminal_Interface.Curses.Forms.Field_Types;

--  |
--  |=====================================================================
--  | man page form_fieldtype.3x
--  |=====================================================================
--  |
package body Terminal_Interface.Curses.Forms.Choice_Field_Types is

   use type Interfaces.C.int;

   package Ft is new Terminal_Interface.Curses.Forms.Field_Types
     (User, User_Access, Field_Check, Character_Check);

   type N_Check is access
      function (Fld : Field; Info : User_Access) return Boolean;
   pragma Convention (C, N_Check);

   type P_Check is access
      function (Fld : Field; Info : User_Access) return Boolean;
   pragma Convention (C, P_Check);

   function Nc (Fld : Field; Info : User_Access) return Boolean;
   pragma Convention (C, Nc);

   function Pc (Fld : Field; Info : User_Access) return Boolean;
   pragma Convention (C, Pc);

   function Nc (Fld : Field; Info : User_Access) return Boolean
   is
   begin
      return Next_Choice (Fld, Info);
   end Nc;

   function Pc (Fld : Field; Info : User_Access) return Boolean
   is
   begin
      return Prev_Choice (Fld, Info);
   end Pc;
   --  |
   --  |
   --  |
   function Set_Choice (Ft : C_Field_Type;
                        Nc : N_Check;
                        Pc : P_Check) return C_Int;
   pragma Import (C, Set_Choice, "set_fieldtype_choice");

   procedure Define_Choices
   is
      R : Eti_Error;
   begin
      R := Set_Choice (Search_Type (User'Tag), Nc'Access, Pc'Access);
      if (R /= E_OK) then
         Eti_Exception (R);
      end if;
   end Define_Choices;

begin
   Define_Choices;
end Terminal_Interface.Curses.Forms.Choice_Field_Types;
