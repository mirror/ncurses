------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--               Terminal_Interface.Curses.Text_IO.Integer_IO               --
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
   type Num is range <>;

package Terminal_Interface.Curses.Text_IO.Integer_IO is

   Default_Width : Field := Num'Width;
   Default_Base  : Number_Base := 10;

   procedure Put
     (Win   : in Window;
      Item  : in Num;
      Width : in Field := Default_Width;
      Base  : in Number_Base := Default_Base);

   procedure Put
     (Item  : in Num;
      Width : in Field := Default_Width;
      Base  : in Number_Base := Default_Base);

private
   pragma Inline (Put);

end Terminal_Interface.Curses.Text_IO.Integer_IO;
