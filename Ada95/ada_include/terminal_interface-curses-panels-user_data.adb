------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                 Terminal_Interface.Curses.Panels.User_Data               --
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
with Terminal_Interface.Curses.Aux;
use  Terminal_Interface.Curses.Aux;
with Terminal_Interface.Curses.Panels;
use  Terminal_Interface.Curses.Panels;

package body Terminal_Interface.Curses.Panels.User_Data is

   use type Interfaces.C.int;

   procedure Set_User_Data (Pan  : in Panel;
                            Data : in User_Access)
   is
      function Set_Panel_Userptr (Pan  : Panel;
                                  Addr : User_Access) return C_Int;
      pragma Import (C, Set_Panel_Userptr, "set_panel_userptr");
   begin
      if Set_Panel_Userptr (Pan, Data) = Curses_Err then
         raise Panel_Exception;
      end if;
   end Set_User_Data;

   procedure Get_User_Data (Pan  : in  Panel;
                            Data : out User_Access)
   is
      function Panel_Userptr (Pan : Panel) return User_Access;
      pragma Import (C, Panel_Userptr, "panel_userptr");
   begin
      Data := Panel_Userptr (Pan);
   end Get_User_Data;

end Terminal_Interface.Curses.Panels.User_Data;
