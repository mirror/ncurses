------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--               Terminal_Interface.Curses.Menus.Menu_User_Data             --
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
with Unchecked_Conversion;

package body Terminal_Interface.Curses.Menus.Menu_User_Data is

   function To_Address is new
     Unchecked_Conversion (User_Access,
                           System.Address);
   function To_Pointer is new
     Unchecked_Conversion (System.Address,
                           User_Access);

   procedure Set_User_Data (Men  : in Menu;
                            Data : in User_Access)
   is
      function Menu_Userptr (Men : Menu) return Ada_User_Wrapper_Access;
      pragma Import (C, Menu_Userptr, "menu_userptr");

      U : Ada_User_Wrapper_Access := Menu_Userptr (Men);
   begin
      if U = null or else U.I = null then
         raise Menu_Exception;
      end if;
      U.U := To_Address (Data);
   end Set_User_Data;

   procedure Get_User_Data (Men  : in  Menu;
                            Data : out User_Access)
   is
      function Menu_Userptr (Men : Menu) return Ada_User_Wrapper_Access;
      pragma Import (C, Menu_Userptr, "menu_userptr");

      U : Ada_User_Wrapper_Access := Menu_Userptr (Men);
   begin
      if U = null then
         raise Menu_Exception;
      else
         Data := To_Pointer (U.U);
      end if;
   end Get_User_Data;

end Terminal_Interface.Curses.Menus.Menu_User_Data;
