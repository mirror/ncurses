------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                            Sample.Curses_Demo                            --
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
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with Terminal_Interface.Curses.Mouse;  use Terminal_Interface.Curses.Mouse;
with Terminal_Interface.Curses.Panels;  use Terminal_Interface.Curses.Panels;
with Terminal_Interface.Curses.Panels.User_Data;

with Sample.Manifest; use Sample.Manifest;
with Sample.Helpers; use Sample.Helpers;
with Sample.Function_Key_Setting; use Sample.Function_Key_Setting;
with Sample.Keyboard_Handler; use Sample.Keyboard_Handler;
with Sample.Header_Handler; use Sample.Header_Handler;
with Sample.Explanation; use Sample.Explanation;

with Sample.Menu_Demo.Handler;
with Sample.Curses_Demo.Mouse;
with Sample.Curses_Demo.Attributes;

package body Sample.Curses_Demo is

   procedure Demo
   is
      function My_Driver (M : Menu;
                          K : Key_Code;
                          Pan : Panel) return Boolean;
      package Mh is new Sample.Menu_Demo.Handler (My_Driver);

      Itm : constant Item_Array (1 .. 2) :=
        (New_Item ("Attributes Demo"),
         New_Item ("Mouse Demo"));
      M : Menu := New_Menu (Itm);

      function My_Driver (M : Menu;
                          K : Key_Code;
                          Pan : Panel) return Boolean
      is
         Idx : constant Positive := Get_Index (Current (M));
      begin
         if K in User_Key_Code'Range then
            if K = QUIT then
               return True;
            elsif K = SELECT_ITEM then
               if Idx in Itm'Range then
                  Hide (Pan);
                  Update_Panels;
               end if;
               case Idx is
                  when 1 => Sample.Curses_Demo.Attributes.Demo;
                  when 2 => Sample.Curses_Demo.Mouse.Demo;
                  when others => Not_Implemented;
               end case;
               if Idx in Itm'Range then
                  Top (Pan);
                  Show (Pan);
                  Update_Panels;
                  Update_Screen;
               end if;
            end if;
         end if;
         return False;
      end My_Driver;

   begin

      if Item_Count (M) /= Itm'Length then
         raise Constraint_Error;
      end if;

      if not Has_Key (Key_Mouse) then
         declare
            O : Item_Option_Set;
         begin
            Get_Options (Itm (2), O);
            O.Selectable := False;
            Set_Options (Itm (2), O);
         end;
      end if;

      Push_Environment ("CURSES00");
      Notepad ("CURSES-PAD00");
      Default_Labels;
      Refresh_Soft_Label_Keys_Without_Update;

      Mh.Drive_Me (M, " Demo ");
      Pop_Environment;

      Delete (M);

   end Demo;

end Sample.Curses_Demo;
