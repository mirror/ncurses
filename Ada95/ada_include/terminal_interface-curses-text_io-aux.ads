------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                   Terminal_Interface.Curses.Text_IO.Aux                  --
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
private package Terminal_Interface.Curses.Text_IO.Aux is

   --  This routine is called from the Text_IO output routines for numeric
   --  and enumeration types.
   --
   procedure Put_Buf
     (Win    : in Window;               -- The output window
      Buf    : in String;               -- The buffer containing the text
      Width  : in Field;                -- The width of the output field
      Signal : in Boolean := True;      -- If true, we raise Layout_Error
      Ljust  : in Boolean := False);    -- The Buf is left justified

end Terminal_Interface.Curses.Text_IO.Aux;

