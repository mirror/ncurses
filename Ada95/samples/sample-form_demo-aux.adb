------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                            Sample.Form_Demo.Aux                          --
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
--  $Revision: 1.5 $
------------------------------------------------------------------------------
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

with Sample.Manifest; use Sample.Manifest;
with Sample.Helpers; use Sample.Helpers;
with Sample.Keyboard_Handler; use Sample.Keyboard_Handler;
with Sample.Explanation; use Sample.Explanation;

package body Sample.Form_Demo.Aux is

   procedure Geometry (F  : in  Form;
                       L  : out Line_Count;        -- Lines used for menu
                       C  : out Column_Count;      -- Columns used for menu
                       Y  : out Line_Position;     -- Proposed Line for menu
                       X  : out Column_Position)   -- Proposed Column for menu
   is
   begin
      Scale (F, L, C);

      L := L + 2;  -- count for frame at top and bottom
      C := C + 2;  -- "

      --  Calculate horizontal coordinate at the screen center
      X := (Columns - C) / 2;
      Y := 1; -- start always in line 1
   end Geometry;

   function Create (F     : Form;
                    Title : String;
                    Lin   : Line_Position;
                    Col   : Column_Position) return Panel
   is
      W, S : Window;
      L : Line_Count;
      C : Column_Count;
      Y : Line_Position;
      X : Column_Position;
      Pan : Panel;
   begin
      Geometry (F, L, C, Y, X);
      W := New_Window (L, C, Lin, Col);
      Set_Meta_Mode (W);
      Set_KeyPad_Mode (W);
      if Has_Colors then
         Set_Background (Win => W,
                         Ch  => (Ch    => ' ',
                                 Color => Default_Colors,
                                 Attr  => Normal_Video));
         Set_Character_Attributes (Win => W,
                                   Color => Default_Colors,
                                   Attr  => Normal_Video);
         Erase (W);
      end if;
      S := Derived_Window (W, L - 2, C - 2, 1, 1);
      Set_Meta_Mode (S);
      Set_KeyPad_Mode (S);
      Box (W);
      Set_Window (F, W);
      Set_Sub_Window (F, S);
      if Title'Length > 0 then
         Window_Title (W, Title);
      end if;
      Pan := New_Panel (W);
      Post (F);
      return Pan;
   end Create;

   procedure Destroy (F : in Form;
                      P : in out Panel)
   is
      W, S : Window;
   begin
      W := Get_Window (F);
      S := Get_Sub_Window (F);
      Post (F, False);
      Erase (W);
      Delete (P);
      Set_Window (F, Null_Window);
      Set_Sub_Window (F, Null_Window);
      Delete (S);
      Delete (W);
      Update_Panels;
   end Destroy;

   function Get_Request (F           : Form;
                         P           : Panel;
                         Handle_CRLF : Boolean := True) return Key_Code
   is
      W  : constant Window := Get_Window (F);
      K  : Real_Key_Code;
      Ch : Character;
   begin
      Top (P);
      loop
         K := Get_Key (W);
         if K in Special_Key_Code'Range then
            case K is
               when HELP_CODE           => Explain_Context;
               when EXPLAIN_CODE        => Explain ("FORMKEYS");
               when Key_Home            => return F_First_Field;
               when Key_End             => return F_Last_Field;
               when QUIT_CODE           => return QUIT;
               when Key_Cursor_Down     => return F_Down_Char;
               when Key_Cursor_Up       => return F_Up_Char;
               when Key_Cursor_Left     => return F_Left_Char;
               when Key_Cursor_Right    => return F_Right_Char;
               when Key_Next_Page       => return F_ScrollForward_Line;
               when Key_Previous_Page   => return F_ScrollBackward_Line;
               when Key_Backspace       => return F_Delete_Previous;
               when others              => return K;
            end case;
         elsif K in Normal_Key_Code'Range then
            Ch := Character'Val (K);
            case Ch is
               when DC1 => return QUIT;                  -- CTRL-Q
               when ACK => return F_Next_Page;           -- CTRL-F
               when STX => return F_Previous_Page;       -- CTRL-B
               when SO  => return F_Next_Field;          -- CTRL-N
               when DLE => return F_Previous_Field;      -- CTRL-P
               when FF  => return F_Left_Field;          -- CTRL-L
               when DC2 => return F_Right_Field;         -- CTRL-R
               when NAK => return F_Up_Field;            -- CTRL-U
               when EOT => return F_Down_Field;          -- CTRL-D
               when ETB => return F_Next_Word;           -- CTRL-W
               when DC4 => return F_Previous_Word;       -- CTRL-T
               when DC3 => return F_Begin_Field;         -- CTRL-S
               when ENQ => return F_End_Field;           -- CTRL-E
               when HT  => return F_Insert_Char;         -- CTRL-I
               when SI  => return F_Insert_Line;         -- CTRL-O
               when SYN => return F_Delete_Char;         -- CTRL-V
               when BS  => return F_Delete_Previous;     -- CTRL-H
               when EM  => return F_Delete_Line;         -- CTRL-Y
               when BEL => return F_Delete_Word;         -- CTRL-G
               when VT  => return F_Clear_EOF;           -- CTRL-K
               when CAN => return F_Clear_Field;         -- CTRL-X
               when SOH => return F_Next_Choice;         -- CTRL-A
               when SUB => return F_Previous_Choice;     -- CTRL-Z
               when CR | LF  =>
                  if Handle_CRLF then
                     return F_New_Line;
                  else
                     return K;
                  end if;
               when others => return K;
            end case;
         else
            return K;
         end if;
      end loop;
   end Get_Request;

   function Make (Top         : Line_Position;
                  Left        : Column_Position;
                  Text        : String) return Field
   is
      Fld : Field;
      C : Column_Count := Column_Count (Text'Length);
   begin
      Fld := New_Field (1, C, Top, Left);
      Set_Buffer (Fld, 0, Text);
      Switch_Options (Fld, (Active => True, others => False), False);
      if Has_Colors then
         Set_Background (Fld => Fld, Color => Default_Colors);
      end if;
      return Fld;
   end Make;

   function Make  (Height      : Line_Count := 1;
                   Width       : Column_Count;
                   Top         : Line_Position;
                   Left        : Column_Position;
                   Off_Screen  : Natural := 0) return Field
   is
      Fld : Field := New_Field (Height, Width, Top, Left, Off_Screen);
   begin
      if Has_Colors then
         Set_Foreground (Fld => Fld, Color => Form_Fore_Color);
         Set_Background (Fld => Fld, Color => Form_Back_Color);
      else
         Set_Background (Fld, (Reverse_Video => True, others => False));
      end if;
      return Fld;
   end Make;

   function Default_Driver (F : Form;
                            K : Key_Code;
                            P : Panel) return Boolean
   is
   begin
      if K in User_Key_Code'Range and then K = QUIT then
         if Driver (F, F_Validate_Field) = Form_Ok  then
            return True;
         end if;
      end if;
      return False;
   end Default_Driver;

   function Count_Active (F : Form) return Natural
   is
      N : Natural := 0;
      O : Field_Option_Set;
      A : constant Field_Array_Access := Fields (F);
      H : constant Natural := Field_Count (F);
   begin
      if H > 0 then
         for I in 1 .. H loop
            Get_Options (A.all (I), O);
            if O.Active then
               N := N + 1;
            end if;
         end loop;
      end if;
      return N;
   end Count_Active;

end Sample.Form_Demo.Aux;
