------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                           Sample.Explanation                             --
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
--  Version Control
--  $Revision: 1.2 $
------------------------------------------------------------------------------
--  Poor mans help system. This scans a sequential file for key lines and
--  then reads the lines up to the next key. Those lines are presented in
--  a window as help or explanation.
--
with Terminal_Interface.Curses;

package Sample.Explanation is

   package Curses renames Terminal_Interface.Curses;

   procedure Explain (Key : in String);
   --  Retrieve the text associated with this key and display it.

   procedure Explain_Context;
   --  Explain the current context.

   procedure Notepad (Key : in String);
   --  Put a note on the screen and maintain it with the context

   Explanation_Not_Found : exception;
   Explanation_Error     : exception;

end Sample.Explanation;
