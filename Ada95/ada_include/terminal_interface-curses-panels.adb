------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                      Terminal_Interface.Curses.Panels                    --
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
--  Version Control:
--  $Revision: 1.3 $
------------------------------------------------------------------------------
with Terminal_Interface.Curses.Aux; use Terminal_Interface.Curses.Aux;
with Interfaces.C;

package body Terminal_Interface.Curses.Panels is

   use type Interfaces.C.int;

   function Create (Win : Window) return Panel
   is
      function Newpanel (Win : Window) return Panel;
      pragma Import (C, Newpanel, "new_panel");

      Pan : Panel;
   begin
      Pan := Newpanel (Win);
      if Pan = Null_Panel then
         raise Panel_Exception;
      end if;
      return Pan;
   end Create;

   procedure Bottom (Pan : in Panel)
   is
      function Bottompanel (Pan : Panel) return C_Int;
      pragma Import (C, Bottompanel, "bottom_panel");
   begin
      if Bottompanel (Pan) = Curses_Err then
         raise Panel_Exception;
      end if;
   end Bottom;

   procedure Top (Pan : in Panel)
   is
      function Toppanel (Pan : Panel) return C_Int;
      pragma Import (C, Toppanel, "top_panel");
   begin
      if Toppanel (Pan) = Curses_Err then
         raise Panel_Exception;
      end if;
   end Top;

   procedure Show (Pan : in Panel)
   is
      function Showpanel (Pan : Panel) return C_Int;
      pragma Import (C, Showpanel, "show_panel");
   begin
      if Showpanel (Pan) = Curses_Err then
         raise Panel_Exception;
      end if;
   end Show;

   procedure Hide (Pan : in Panel)
   is
      function Hidepanel (Pan : Panel) return C_Int;
      pragma Import (C, Hidepanel, "hide_panel");
   begin
      if Hidepanel (Pan) = Curses_Err then
         raise Panel_Exception;
      end if;
   end Hide;

   function Get_Window (Pan : Panel) return Window
   is
      function Panel_Win (Pan : Panel) return Window;
      pragma Import (C, Panel_Win, "panel_window");

      Win : Window := Panel_Win (Pan);
   begin
      if Win = Null_Window then
         raise Panel_Exception;
      end if;
      return Win;
   end Get_Window;

   procedure Replace (Pan : in Panel;
                      Win : in Window)
   is
      function Replace_Pan (Pan : Panel;
                            Win : Window) return C_Int;
      pragma Import (C, Replace_Pan, "replace_panel");
   begin
      if Replace_Pan (Pan, Win) = Curses_Err then
         raise Panel_Exception;
      end if;
   end Replace;

   procedure Move (Pan    : in Panel;
                   Line   : in Line_Position;
                   Column : in Column_Position)
   is
      function Move (Pan    : Panel;
                     Line   : C_Int;
                     Column : C_Int) return C_Int;
      pragma Import (C, Move, "move_panel");
   begin
      if Move (Pan, C_Int (Line), C_Int (Column)) = Curses_Err then
         raise Panel_Exception;
      end if;
   end Move;

   function Is_Hidden (Pan : Panel) return Boolean
   is
      function Panel_Hidden (Pan : Panel) return C_Int;
      pragma Import (C, Panel_Hidden, "panel_hidden");
   begin
      if Panel_Hidden (Pan) = Curses_False then
         return False;
      else
         return True;
      end if;
   end Is_Hidden;

   procedure Delete (Pan : in out Panel)
   is
      function Del_Panel (Pan : Panel) return C_Int;
      pragma Import (C, Del_Panel, "del_panel");
   begin
      if Del_Panel (Pan) = Curses_Err then
         raise Panel_Exception;
      end if;
      Pan := Null_Panel;
   end Delete;

end Terminal_Interface.Curses.Panels;
