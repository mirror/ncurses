------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--             Terminal_Interface.Curses.Text_IO.Enumeration_IO             --
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
   type Enum is (<>);

package Terminal_Interface.Curses.Text_IO.Enumeration_IO is

   Default_Width : Field := 0;
   Default_Setting : Type_Set := Mixed_Case;

   procedure Put
     (Win   : in Window;
      Item  : in Enum;
      Width : in Field := Default_Width;
      Set   : in Type_Set := Default_Setting);

   procedure Put
     (Item  : in Enum;
      Width : in Field := Default_Width;
      Set   : in Type_Set := Default_Setting);

private
   pragma Inline (Put);

end Terminal_Interface.Curses.Text_IO.Enumeration_IO;
