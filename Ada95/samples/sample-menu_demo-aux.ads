------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                            Sample.Menu_Demo.Aux                          --
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
with Terminal_Interface.Curses.Menus; use  Terminal_Interface.Curses.Menus;

package Sample.Menu_Demo.Aux is

   procedure Geometry (M  : in  Menu;
                       L  : out Line_Count;
                       C  : out Column_Count;
                       Y  : out Line_Position;
                       X  : out Column_Position);
   --  Calculate the geometry for a panel beeing able to be used to display
   --  the menu.

   function Create (M     : Menu;
                    Title : String;
                    Lin   : Line_Position;
                    Col   : Column_Position) return Panel;
   --  Create a panel decorated with a frame and the title at the specified
   --  position. The dimension of the panel is derived from the menus layout.

   procedure Destroy (M : in Menu;
                      P : in out Panel);
   --  Destroy all the windowing structures associated with this menu and
   --  panel.

   function Get_Request (M : Menu; P : Panel) return Key_Code;
   --  Centralized request driver for all menus in this sample. This
   --  gives us a common key binding for all menus.

end Sample.Menu_Demo.Aux;
