--  -*- ada -*-
define(`HTMLNAME',`terminal_interface-curses-panels_s.html')dnl
include(M4MACRO)dnl
------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                      Terminal_Interface.Curses.Panels                    --
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
--  $Revision: 1.2 $
------------------------------------------------------------------------------
with System;

package Terminal_Interface.Curses.Panels is

include(`Panel_Linker_Options')

   type Panel is private;

   ---------------------------
   --  Interface constants  --
   ---------------------------
   Null_Panel : constant Panel;

   -------------------
   --  Exceptions   --
   -------------------

   Panel_Exception : exception;

   --  MANPAGE(`panel.3x')

   --  ANCHOR(`new_panel()',`Create')
   function Create (Win : Window) return Panel;
   --  AKA

   --  ANCHOR(`new_panel()',`New_Panel')
   function New_Panel (Win : Window) return Panel renames Create;
   --  AKA

   --  ANCHOR(`bottom_panel()',`Bottom')
   procedure Bottom (Pan : in Panel);
   --  AKA

   --  ANCHOR(`top_panel()',`Top')
   procedure Top (Pan : in Panel);
   --  AKA

   --  ANCHOR(`show_panel()',`Show')
   procedure Show (Pan : in Panel);
   --  AKA

   --  ANCHOR(`update_panels()',`Update_Panels')
   procedure Update_Panels;
   --  AKA
   pragma Import (C, Update_Panels, "update_panels");

   --  ANCHOR(`hide_panel()',`Hide')
   procedure Hide (Pan : in Panel);
   --  AKA

   --  ANCHOR(`panel_window()',`Get_Window')
   function Get_Window (Pan : Panel) return Window;
   --  AKA

   --  ANCHOR(`panel_window()',`Panel_Window')
   function Panel_Window (Pan : Panel) return Window renames Get_Window;

   --  ANCHOR(`replace_panel()',`Replace')
   procedure Replace (Pan : in Panel;
                      Win : in Window);
   --  AKA

   --  ANCHOR(`move_panel()',`Move')
   procedure Move (Pan    : in Panel;
                   Line   : in Line_Position;
                   Column : in Column_Position);
   --  AKA

   --  ANCHOR(`panel_hidden()',`Is_Hidden')
   function Is_Hidden (Pan : Panel) return Boolean;
   --  AKA

   --  ANCHOR(`panel_above()',`Above')
   function Above (Pan : Panel) return Panel;
   --  AKA
   pragma Import (C, Above, "panel_above");

   --  ANCHOR(`panel_below()',`Below')
   function Below (Pan : Panel) return Panel;
   --  AKA
   pragma Import (C, Below, "panel_below");

   --  ANCHOR(`del_panel()',`Delete')
   procedure Delete (Pan : in out Panel);
   --  AKA

   private
      type Panel is new System.Address;
      Null_Panel : constant Panel := Panel (System.Null_Address);

end Terminal_Interface.Curses.Panels;
