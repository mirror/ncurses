------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--               Terminal_Interface.Curses.Text_IO.Complex_IO               --
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
with Terminal_Interface.Curses.Text_IO.Float_IO;

package body Terminal_Interface.Curses.Text_IO.Complex_IO is

   package FIO is new
     Terminal_Interface.Curses.Text_IO.Float_IO (Complex_Types.Real'Base);

   procedure Put
     (Win  : in Window;
      Item : in Complex;
      Fore : in Field := Default_Fore;
      Aft  : in Field := Default_Aft;
      Exp  : in Field := Default_Exp)
   is
   begin
      Put (Win, '(');
      FIO.Put (Win, Item.Re, Fore, Aft, Exp);
      Put (Win, ',');
      FIO.Put (Win, Item.Im, Fore, Aft, Exp);
      Put (Win, ')');
   end Put;

   procedure Put
     (Item : in Complex;
      Fore : in Field := Default_Fore;
      Aft  : in Field := Default_Aft;
      Exp  : in Field := Default_Exp)
   is
   begin
      Put (Get_Window, Item, Fore, Aft, Exp);
   end Put;

end Terminal_Interface.Curses.Text_IO.Complex_IO;
