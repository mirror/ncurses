------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                          Sample.Form_Demo.Handler                        --
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
with Terminal_Interface.Curses;
use  Terminal_Interface.Curses;
with Terminal_Interface.Curses.Panels;
use  Terminal_Interface.Curses.Panels;
with Terminal_Interface.Curses.Forms;
use  Terminal_Interface.Curses.Forms;

generic
   with function  My_Driver (Frm : Form;
                             K   : Key_Code;
                             Pan : Panel) return Boolean;
package Sample.Form_Demo.Handler is

   procedure Drive_Me (F     : in Form;
                       Lin   : in Line_Position;
                       Col   : in Column_Position;
                       Title : in String := "");
   --  Position the menu at the given point and drive it.

   procedure Drive_Me (F     : in Form;
                       Title : in String := "");
   --  Center menu and drive it.

end Sample.Form_Demo.Handler;
