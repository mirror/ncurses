------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--               Terminal_Interface.Curses.Menus.Item_User_Data             --
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
with Interfaces.C;
with Terminal_Interface.Curses.Aux; use Terminal_Interface.Curses.Aux;

package body Terminal_Interface.Curses.Menus.Item_User_Data is

   use type Interfaces.C.int;

   procedure Set_User_Data (Itm  : in Item;
                            Data : in User_Access)
   is
      function Set_Item_Userptr (Itm  : Item;
                                 Addr : User_Access)  return C_Int;
      pragma Import (C, Set_Item_Userptr, "set_item_userptr");

      Res : constant Eti_Error := Set_Item_Userptr (Itm, Data);
   begin
      if  Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_User_Data;

   procedure Get_User_Data (Itm  : in  Item;
                            Data : out User_Access)
   is
      function Item_Userptr (Itm : Item) return User_Access;
      pragma Import (C, Item_Userptr, "item_userptr");
   begin
      Data := Item_Userptr (Itm);
   end Get_User_Data;

end Terminal_Interface.Curses.Menus.Item_User_Data;
