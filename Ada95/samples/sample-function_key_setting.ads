------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                         Sample.Function_Key_Setting                      --
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
with Terminal_Interface.Curses.Panels; use Terminal_Interface.Curses.Panels;

--  This package implements a simple stack of function key label environments.
--
package Sample.Function_Key_Setting is

   procedure Push_Environment (Key   : in String;
                               Reset : in Boolean := True);
   --  Push the definition of the current function keys on an internal
   --  stack. If the reset flag is true, all labels are reset while
   --  pushed, so the new environment can assume a tabula rasa.
   --  The Key defines the new Help Context associated with the new
   --  Environment. This saves also the currently active Notepad.

   procedure Pop_Environment;
   --  Pop the Definitions from the stack and make them the current ones.
   --  This also restores the Help context and the previous Notepad.

   procedure Initialize (Mode : Soft_Label_Key_Format := PC_Style;
                         Just : Label_Justification := Left);
   --  Initialize the environment

   function Context return String;
   --  Return the current context identitfier

   function Find_Context (Key : String) return Boolean;
   --  Look for a context, return true if it is in the stack,
   --  false otherwise.

   procedure Notepad_To_Context (Pan : in Panel);
   --  Add a panel representing a notepad to the current context.

   Function_Key_Stack_Error : exception;

   procedure Default_Labels;
   --  Set the default labels used in all environments

   function Notepad_Window return Window;
   --  Return the current notepad window or Null_Window if there is none.

end Sample.Function_Key_Setting;
