------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                 Terminal_Interface.Curses.Forms.Field_Types              --
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
--  $Revision: 1.4 $
------------------------------------------------------------------------------
with Interfaces.C;
with Terminal_Interface.Curses.Aux; use Terminal_Interface.Curses.Aux;
with Unchecked_Deallocation;

--  |
--  |=====================================================================
--  | man page form_fieldtype.3x
--  |=====================================================================
--  |
package body Terminal_Interface.Curses.Forms.Field_Types is

   use type Interfaces.C.int;

   type F_Check is access
      function (Fld : Field; Info : User_Access) return C_Int;
   pragma Convention (C, F_Check);

   type C_Check is access
      function (Ch : Character; Info : User_Access) return C_Int;
   pragma Convention (C, C_Check);

   procedure Free is new
     Unchecked_Deallocation (User, User_Access);

   --  Forward decls.
   procedure Register_Field_Type;
   procedure Unregister_Field_Type;

   procedure Initialize (Obj : in out Tracker)
   is
   begin
      Register_Field_Type;
   end Initialize;

   procedure Finalize (Obj : in out Tracker)
   is
   begin
      Unregister_Field_Type;
   end Finalize;

   function Fc (Fld : Field; Info : User_Access) return C_Int;
   pragma Convention (C, Fc);

   function Cc (Ch : Character; Info : User_Access) return C_Int;
   pragma Convention (C, Cc);

   function Make_Arg (U : User_Access) return User_Access;
   pragma Convention (C, Make_Arg);

   function Copy_Arg (U : User_Access) return User_Access;
   pragma Convention (C, Copy_Arg);

   procedure Free_Arg (U : User_Access);
   pragma Convention (C, Free_Arg);

   function New_Fieldtype (Fc : F_Check;
                           Cc : C_Check) return C_Field_Type;
   pragma Import (C, New_Fieldtype, "new_fieldtype");

   function Fc (Fld : Field; Info : User_Access) return C_Int
   is
   begin
      return C_Int (Boolean'Pos (Field_Check (Fld, Info)));
   end Fc;

   function Cc (Ch : Character; Info : User_Access) return C_Int
   is
   begin
      return C_Int (Boolean'Pos (Character_Check (Ch, Info)));
   end Cc;

   function Make_Arg (U : User_Access) return User_Access
   is
      function Fixme (U : User_Access) return User_Access;
      pragma Import (C, Fixme, "_nc_ada_getvarg");
      V : constant User_Access := Fixme (U);
      I : constant User_Access := new User'(V.all);
   begin
      return I;
   end Make_Arg;

   function Copy_Arg (U : User_Access) return User_Access
   is
      I : constant User_Access := new User'(U.all);
   begin
      return I;
   end Copy_Arg;

   procedure Free_Arg (U : User_Access)
   is
   begin
      null;
   end Free_Arg;

   type M_Arg is access function (U : User_Access) return User_Access;
   pragma Convention (C, M_Arg);

   type C_Arg is access function (U : User_Access) return User_Access;
   pragma Convention (C, C_Arg);

   type F_Arg is access procedure (U : User_Access);
   pragma Convention (C, F_Arg);

   function Set_Fieldtype_Arg (Typ : C_Field_Type;
                               Ma  : M_Arg;
                               Ca  : C_Arg;
                               Fa  : F_Arg) return C_Int;
   pragma Import (C, Set_Fieldtype_Arg, "set_fieldtype_arg");
   --  |
   --  |
   --  |

   procedure Register_Field_Type
   is
      Res : Eti_Error;
      Cft : C_Field_Type;
      P   : User_Access := new User;
      --  we need an instance to call
      --  the Register_Type procedure
   begin
      Cft := New_Fieldtype (Fc'Access,
                            Cc'Access);
      if Cft = Null_Field_Type then
         raise Form_Exception;
      end if;
      Res := Set_Fieldtype_Arg (Cft,
                                Make_Arg'Access,
                                Copy_Arg'Access,
                                Free_Arg'Access);
      if Res /= E_Ok then
         Eti_Exception (Res);
      end if;

      Register_Type (P.all, Cft);
      Free (P);
   end Register_Field_Type;
   --  |
   --  |
   --  |
   procedure Unregister_Field_Type
   is
      P : User_Access := new User;
      --  we need an instance to call
      --  the Unregister_Type procedure
   begin
      Unregister_Type (P.all);
      Free (P);
   end Unregister_Field_Type;

   Hook : Tracker;
end Terminal_Interface.Curses.Forms.Field_Types;





