------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--               Terminal_Interface.Curses.Text_IO.Decimal_IO               --
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

package body Terminal_Interface.Curses.Text_IO.Decimal_IO is

   package Aux renames Terminal_Interface.Curses.Text_IO.Aux;
   package DIO is new Ada.Text_IO.Decimal_IO (Num);

   procedure Put
     (Win  : in Window;
      Item : in Num;
      Fore : in Field := Default_Fore;
      Aft  : in Field := Default_Aft;
      Exp  : in Field := Default_Exp)
   is
      Buf : String (1 .. Field'Last);
      Len : Field := Fore + 1 + Aft;
   begin
      if Exp > 0 then
         Len := Len + 1 + Exp;
      end if;
      DIO.Put (Buf, Item, Aft, Exp);
      Aux.Put_Buf (Win, Buf, Len, False);
   end Put;

   procedure Put
     (Item  : in Num;
      Fore : in Field := Default_Fore;
      Aft  : in Field := Default_Aft;
      Exp  : in Field := Default_Exp) is
   begin
      Put (Get_Window, Item, Fore, Aft, Exp);
   end Put;

end Terminal_Interface.Curses.Text_IO.Decimal_IO;
