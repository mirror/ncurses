------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--             Terminal_Interface.Curses.Text_IO.Enumeration_IO             --
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
with Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Terminal_Interface.Curses.Text_IO.Aux;

package body Terminal_Interface.Curses.Text_IO.Enumeration_IO is

   package Aux renames Terminal_Interface.Curses.Text_IO.Aux;
   package EIO is new Ada.Text_IO.Enumeration_IO (Enum);

   procedure Put
     (Win   : in Window;
      Item  : in Enum;
      Width : in Field := Default_Width;
      Set   : in Type_Set := Default_Setting)
   is
      Buf  : String (1 .. Field'Last);
      Tset : Ada.Text_IO.Type_Set;
   begin
      if Set /= Mixed_Case then
         Tset := Ada.Text_IO.Type_Set'Val (Type_Set'Pos (Set));
      else
         Tset := Ada.Text_IO.Lower_Case;
      end if;
      EIO.Put (Buf, Item, Tset);
      if Set = Mixed_Case then
         Buf (Buf'First) := To_Upper (Buf (Buf'First));
      end if;
      Aux.Put_Buf (Win, Buf, Width, True, True);
   end Put;

   procedure Put
     (Item  : in Enum;
      Width : in Field := Default_Width;
      Set   : in Type_Set := Default_Setting)
   is
   begin
      Put (Get_Window, Item, Width, Set);
   end Put;

end Terminal_Interface.Curses.Text_IO.Enumeration_IO;
