------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                       Sample.Curses_Demo.Attributes                      --
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
--  $Revision: 1.4 $
------------------------------------------------------------------------------
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Terminal_Interface.Curses.Panels;  use Terminal_Interface.Curses.Panels;

with Sample.Manifest; use Sample.Manifest;
with Sample.Helpers; use Sample.Helpers;
with Sample.Function_Key_Setting; use Sample.Function_Key_Setting;
with Sample.Keyboard_Handler; use Sample.Keyboard_Handler;
with Sample.Header_Handler; use Sample.Header_Handler;
with Sample.Explanation; use Sample.Explanation;

with Sample.Menu_Demo.Handler;
with Sample.Curses_Demo.Mouse;

package body Sample.Curses_Demo.Attributes is

   procedure Demo
   is
      P : Panel := Create (Standard_Window);
      K : Real_Key_Code;
   begin
      Set_Meta_Mode;
      Set_KeyPad_Mode;

      Top (P);

      Push_Environment ("ATTRIBDEMO");
      Default_Labels;
      Notepad ("ATTRIB-PAD00");

      Set_Character_Attributes (Attr => (others => False));
      Add (Line => 1, Column => Columns / 2 - 10,
           Str => "This is NORMAL");

      Set_Character_Attributes (Attr => (Stand_Out => True,
                                          others => False));
      Add (Line => 2, Column => Columns / 2 - 10,
           Str => "This is Stand_Out");

      Set_Character_Attributes (Attr => (Under_Line => True,
                                          others => False));
      Add (Line => 3, Column => Columns / 2 - 10,
           Str => "This is Under_Line");

      Set_Character_Attributes (Attr => (Reverse_Video => True,
                                          others => False));
      Add (Line => 4, Column => Columns / 2 - 10,
           Str => "This is Reverse_Video");

      Set_Character_Attributes (Attr => (Blink => True,
                                          others => False));
      Add (Line => 5, Column => Columns / 2 - 10,
           Str => "This is Blink");

      Set_Character_Attributes (Attr => (Dim_Character => True,
                                          others => False));
      Add (Line => 6, Column => Columns / 2 - 10,
           Str => "This is Dim_Character");

      Set_Character_Attributes (Attr => (Bold_Character => True,
                                          others => False));
      Add (Line => 7, Column => Columns / 2 - 10,
           Str => "This is Bold_Character");

      Refresh_Without_Update;
      Update_Panels; Update_Screen;

      loop
         K := Get_Key;
         if K in Special_Key_Code'Range then
            case K is
               when QUIT_CODE     => exit;
               when HELP_CODE     => Explain_Context;
               when EXPLAIN_CODE  => Explain ("ATTRIBKEYS");
               when others        => null;
            end case;
         end if;
      end loop;

      Pop_Environment;
      Clear;
      Refresh_Without_Update;
      Delete (P);
      Update_Panels; Update_Screen;

   end Demo;

end Sample.Curses_Demo.Attributes;
