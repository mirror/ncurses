------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                              Sample.Helpers                              --
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

with Sample.Explanation; use Sample.Explanation;

--  This package contains some conveniant helper routines used throughout
--  this example.
--
package body Sample.Helpers is

   procedure Window_Title (Win   : in Window;
                           Title : in String)
   is
      Height : Line_Count;
      Width  : Column_Count;
      Pos    : Column_Position := 0;
   begin
      Get_Size (Win, Height, Width);
      if Title'Length < Width then
         Pos := (Width - Title'Length) / 2;
      end if;
      Add (Win, 0, Pos, Title);
   end Window_Title;

   procedure Not_Implemented is
   begin
      Explain ("NOTIMPL");
   end Not_Implemented;

end Sample.Helpers;
