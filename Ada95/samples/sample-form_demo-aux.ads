------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                            Sample.Form_Demo.Aux                          --
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
with Terminal_Interface.Curses; use  Terminal_Interface.Curses;
with Terminal_Interface.Curses.Panels; use  Terminal_Interface.Curses.Panels;
with Terminal_Interface.Curses.Forms;  use  Terminal_Interface.Curses.Forms;

package Sample.Form_Demo.Aux is

   procedure Geometry (F  : in  Form;
                       L  : out Line_Count;
                       C  : out Column_Count;
                       Y  : out Line_Position;
                       X  : out Column_Position);
   --  Calculate the geometry for a panel beeing able to be used to display
   --  the menu.

   function Create (F     : Form;
                    Title : String;
                    Lin   : Line_Position;
                    Col   : Column_Position) return Panel;
   --  Create a panel decorated with a frame and the title at the specified
   --  position. The dimension of the panel is derived from the menus layout.

   procedure Destroy (F : in Form;
                      P : in out Panel);
   --  Destroy all the windowing structures associated with this menu and
   --  panel.

   function Get_Request (F           : Form;
                         P           : Panel;
                         Handle_CRLF : Boolean := True) return Key_Code;
   --  Centralized request driver for all menus in this sample. This
   --  gives us a common key binding for all menus.

   function Make (Top         : Line_Position;
                  Left        : Column_Position;
                  Text        : String) return Field;
   --  create a label

   function Make  (Height      : Line_Count := 1;
                   Width       : Column_Count;
                   Top         : Line_Position;
                   Left        : Column_Position;
                   Off_Screen  : Natural := 0) return Field;
   --  create a editable field

   function Default_Driver (F : Form;
                            K : Key_Code;
                            P : Panel) return Boolean;

   function Count_Active (F : Form) return Natural;
   --  Count the number of active fields in the form

end Sample.Form_Demo.Aux;
