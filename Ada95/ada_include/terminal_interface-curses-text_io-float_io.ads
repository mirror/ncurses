------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                Terminal_Interface.Curses.Text_IO.Float_IO                --
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
--  $Revision: 1.4 $
------------------------------------------------------------------------------
generic
   type Num is digits <>;

package Terminal_Interface.Curses.Text_IO.Float_IO is

   Default_Fore : Field := 2;
   Default_Aft  : Field := Num'Digits - 1;
   Default_Exp  : Field := 3;

   procedure Put
     (Win  : in Window;
      Item : in Num;
      Fore : in Field := Default_Fore;
      Aft  : in Field := Default_Aft;
      Exp  : in Field := Default_Exp);

   procedure Put
     (Item  : in Num;
      Fore : in Field := Default_Fore;
      Aft  : in Field := Default_Aft;
      Exp  : in Field := Default_Exp);

private
   pragma Inline (Put);

end Terminal_Interface.Curses.Text_IO.Float_IO;
