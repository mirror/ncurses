------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                            Sample.Menu_Demo.Aux                          --
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
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

with Sample.Manifest; use Sample.Manifest;
with Sample.Helpers; use Sample.Helpers;
with Sample.Keyboard_Handler; use Sample.Keyboard_Handler;
with Sample.Explanation; use Sample.Explanation;

package body Sample.Menu_Demo.Aux is

   procedure Geometry (M  : in  Menu;
                       L  : out Line_Count;
                       C  : out Column_Count;
                       Y  : out Line_Position;
                       X  : out Column_Position;
                       Fy : out Line_Position;
                       Fx : out Column_Position);

   procedure Geometry (M  : in  Menu;
                       L  : out Line_Count;        -- Lines used for menu
                       C  : out Column_Count;      -- Columns used for menu
                       Y  : out Line_Position;     -- Proposed Line for menu
                       X  : out Column_Position;   -- Proposed Column for menu
                       Fy : out Line_Position;     -- Vertical inner frame
                       Fx : out Column_Position)   -- Horiz. inner frame
   is
      Spc_Desc : Column_Position; -- spaces between description and item
   begin
      Set_Mark (M, Menu_Marker);

      Spacing (M, Spc_Desc, Fy, Fx);
      Scale (M, L, C);

      Fx := Fx + Column_Position (Fy - 1); -- looks a bit nicer

      L := L + 2 * Fy;  -- count for frame at top and bottom
      C := C + 2 * Fx;  -- "

      --  Calculate horizontal coordinate at the screen center
      X := (Columns - C) / 2;
      Y := 1;  -- always startin line 1

   end Geometry;

   procedure Geometry (M : in  Menu;
                       L : out Line_Count;        -- Lines used for menu
                       C : out Column_Count;      -- Columns used for menu
                       Y : out Line_Position;     -- Proposed Line for menu
                       X  : out Column_Position)  -- Proposed Column for menu
   is
      Fy : Line_Position;
      Fx : Column_Position;
   begin
      Geometry (M, L, C, Y, X, Fy, Fx);
   end Geometry;

   function Create (M     : Menu;
                    Title : String;
                    Lin   : Line_Position;
                    Col   : Column_Position) return Panel
   is
      W, S : Window;
      L : Line_Count;
      C : Column_Count;
      Y, Fy : Line_Position;
      X, Fx : Column_Position;
      Pan : Panel;
   begin
      Geometry (M, L, C, Y, X, Fy, Fx);
      W := New_Window (L, C, Lin, Col);
      Set_Meta_Mode (W);
      Set_KeyPad_Mode (W);
      if Has_Colors then
         Set_Background (Win => W,
                         Ch  => (Ch    => ' ',
                                 Color => Menu_Back_Color,
                                 Attr  => Normal_Video));
         Set_Foreground (Men => M, Color => Menu_Fore_Color);
         Set_Background (Men => M, Color => Menu_Back_Color);
         Set_Grey (Men => M, Color => Menu_Grey_Color);
         Erase (W);
      end if;
      S := Derived_Window (W, L - Fy, C - Fx, Fy, Fx);
      Set_Meta_Mode (S);
      Set_KeyPad_Mode (S);
      Box (W);
      Set_Window (M, W);
      Set_Sub_Window (M, S);
      if Title'Length > 0 then
         Window_Title (W, Title);
      end if;
      Pan := New_Panel (W);
      Post (M);
      return Pan;
   end Create;

   procedure Destroy (M : in Menu;
                      P : in out Panel)
   is
      W, S : Window;
   begin
      W := Get_Window (M);
      S := Get_Sub_Window (M);
      Post (M, False);
      Erase (W);
      Delete (P);
      Set_Window (M, Null_Window);
      Set_Sub_Window (M, Null_Window);
      Delete (S);
      Delete (W);
      Update_Panels;
   end Destroy;

   function Get_Request (M : Menu; P : Panel) return Key_Code
   is
      W  : constant Window := Get_Window (M);
      K  : Real_Key_Code;
      Ch : Character;
   begin
      Top (P);
      loop
         K := Get_Key (W);
         if K in Special_Key_Code'Range then
            case K is
               when HELP_CODE           => Explain_Context;
               when EXPLAIN_CODE        => Explain ("MENUKEYS");
               when Key_Home            => return REQ_FIRST_ITEM;
               when QUIT_CODE           => return QUIT;
               when Key_Cursor_Down     => return REQ_DOWN_ITEM;
               when Key_Cursor_Up       => return REQ_UP_ITEM;
               when Key_Cursor_Left     => return REQ_LEFT_ITEM;
               when Key_Cursor_Right    => return REQ_RIGHT_ITEM;
               when Key_End             => return REQ_LAST_ITEM;
               when Key_Backspace       => return REQ_BACK_PATTERN;
               when Key_Next_Page       => return REQ_SCR_DPAGE;
               when Key_Previous_Page   => return REQ_SCR_UPAGE;
               when others              => return K;
            end case;
         elsif K in Normal_Key_Code'Range then
            Ch := Character'Val (K);
            case Ch is
               when DC1 => return QUIT;                  --  CTRL-Q
               when SO  => return REQ_NEXT_ITEM;         --  CTRL-N
               when DLE => return REQ_PREV_ITEM;         --  CTRL-P
               when NAK => return REQ_SCR_ULINE;         --  CTRL-U
               when EOT => return REQ_SCR_DLINE;         --  CTRL-D
               when ACK => return REQ_SCR_DPAGE;         --  CTRL-F
               when STX => return REQ_SCR_UPAGE;         --  CTRL-B
               when CAN => return REQ_CLEAR_PATTERN;     --  CTRL-X
               when BS  => return REQ_BACK_PATTERN;      --  CTRL-H
               when SOH => return REQ_NEXT_MATCH;        --  CTRL-A
               when SUB => return REQ_PREV_MATCH;        --  CTRL-Z
               when DC4 => return REQ_TOGGLE_ITEM;       --  CTRL-T
               when CR  => return SELECT_ITEM;           --  CTRL-M
               when LF  => return SELECT_ITEM;           --  CTRL-J
               when others => return K;
            end case;
         else
            return K;
         end if;
      end loop;
   end Get_Request;

end Sample.Menu_Demo.Aux;

