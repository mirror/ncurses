--  -*- ada -*-
define(`HTMLNAME',`terminal_interface-curses-menus-menu_user_data_s.html')dnl
include(M4MACRO)dnl
------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--               Terminal_Interface.Curses.Menus.Menu_User_Data             --
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
--  $Revision: 1.2 $
------------------------------------------------------------------------------

generic
   type User is limited private;
   type User_Access is access User;
package Terminal_Interface.Curses.Menus.Menu_User_Data is

   --  The binding uses the C level user pointer already for its own
   --  internal purposes. So you can´t easily manipulate the user pointer
   --  with the low level C routines for this menu without taking care of
   --  this special situation. If you want to read or write with C routines
   --  the user pointer of this menu, you should get first the low level
   --  user pointer. This points to a record, that always has as its first
   --  member the Ada95 user pointer for this menu. You should never change
   --  the low level user pointer of an Ada created menu.
   --
   --  MANPAGE(`menu_userptr.3x')

   --  ANCHOR(`set_menu_userptr',`Set_User_Data')
   procedure Set_User_Data (Men  : in Menu;
                            Data : in User_Access);
   --  AKA
   pragma Convention (C, Set_User_Data);

   --  ANCHOR(`menu_userptr',`Get_User_Data')
   procedure Get_User_Data (Men  : in  Menu;
                            Data : out User_Access);
   --  AKA
   pragma Convention (C, Get_User_Data);

end Terminal_Interface.Curses.Menus.Menu_User_Data;
