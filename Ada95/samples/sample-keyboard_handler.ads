------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                           Sample.Keyboard_Handler                        --
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
with Terminal_Interface.Curses; use Terminal_Interface.Curses;

--  This package contains a centralized keyboard handler used throughout
--  this example. The handler establishes a timeout mechanism that provides
--  periodical updates of the common header lines used in this example.
--
package Sample.Keyboard_Handler is

   function Get_Key (Win : Window := Standard_Window) return Real_Key_Code;
   --  The central routine for handling keystrokes.

   procedure Init_Keyboard_Handler;
   --  Initialize the keyboard

end Sample.Keyboard_Handler;
