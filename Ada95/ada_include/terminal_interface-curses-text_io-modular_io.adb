------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--               Terminal_Interface.Curses.Text_IO.Modular_IO               --
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
with Terminal_Interface.Curses.Text_IO.Aux;

package body Terminal_Interface.Curses.Text_IO.Modular_IO is

   package Aux renames Terminal_Interface.Curses.Text_IO.Aux;
   package MIO is new Ada.Text_IO.Modular_IO (Num);

   procedure Put
     (Win   : in Window;
      Item  : in Num;
      Width : in Field := Default_Width;
      Base  : in Number_Base := Default_Base)
   is
      Buf : String (1 .. Field'Last);
   begin
      MIO.Put (Buf, Item, Base);
      Aux.Put_Buf (Win, Buf, Width);
   end Put;

   procedure Put
     (Item  : in Num;
      Width : in Field := Default_Width;
      Base  : in Number_Base := Default_Base)
   is
   begin
      Put (Get_Window, Item, Width, Base);
   end Put;

end Terminal_Interface.Curses.Text_IO.Modular_IO;
