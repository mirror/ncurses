------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                            Sample.Keyboard_Handler                       --
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
--  $Revision: 1.2 $
------------------------------------------------------------------------------
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

with Terminal_Interface.Curses.Panels; use Terminal_Interface.Curses.Panels;
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;

with Sample.Header_Handler; use Sample.Header_Handler;
with Sample.Form_Demo.Aux; use Sample.Form_Demo.Aux;
with Sample.Manifest; use Sample.Manifest;
with Sample.Form_Demo.Handler;

--  This package contains a centralized keyboard handler used throughout
--  this example. The handler establishes a timeout mechanism that provides
--  periodical updates of the common header lines used in this example.
--

package body Sample.Keyboard_Handler is

   In_Command : Boolean := False;

   function Get_Key (Win : Window := Standard_Window) return Real_Key_Code
   is
      K : Real_Key_Code;

      function Command return Real_Key_Code;


      function Command return Real_Key_Code
      is
         function My_Driver (F : Form;
                             C : Key_Code;
                             P : Panel) return Boolean;
         package Fh is new Sample.Form_Demo.Handler (My_Driver);

         type Label_Array is array (Label_Number) of String (1 .. 8);

         Labels : Label_Array;

         FA : Field_Array (1 .. 2) := (Make (0, 0, "Command:"),
                                       Make (Top => 0, Left => 9,
                                             Width => Columns - 11));

         K  : Real_Key_Code := Key_None;
         N  : Natural := 0;

         function My_Driver (F : Form;
                             C : Key_Code;
                             P : Panel) return Boolean
         is
            Ch : Character;
         begin
            if C in User_Key_Code'Range and then C = QUIT then
               if Driver (F, F_Validate_Field) = Form_Ok  then
                  K := Key_None;
                  return True;
               end if;
            elsif C in Normal_Key_Code'Range then
               Ch := Character'Val (C);
               if (Ch = LF or else Ch = CR) then
                  if Driver (F, F_Validate_Field) = Form_Ok  then
                     declare
                        Buffer : String (1 .. Positive (Columns - 11));
                        Cmdc : String (1 .. 8);
                     begin
                        Get_Buffer (Fld => FA (2), Str => Buffer);
                        Trim (Buffer, Left);
                        if Buffer (1) /= ' ' then
                           Cmdc := Buffer (Cmdc'Range);
                           for I in Labels'Range loop
                              if Cmdc = Labels (I) then
                                 K := Function_Key_Code
                                   (Function_Key_Number (I));
                                 exit;
                              end if;
                           end loop;
                        end if;
                        return True;
                     end;
                  end if;
               end if;
            end if;
            return False;
         end My_Driver;

      begin
         In_Command := True;
         for I in Label_Number'Range loop
            Get_Soft_Label_Key (I, Labels (I));
            Trim (Labels (I), Left);
            Translate (Labels (I), Upper_Case_Map);
            if Labels (I) (1) /= ' ' then
               N := N + 1;
            end if;
         end loop;
         if N > 0 then --  some labels were really set
            declare
               Enum_Info    : Enumeration_Info (N);
               Enum_Field   : Enumeration_Field;
               J : Positive := Enum_Info.Names'First;

               Frm : Form := Create (FA);

            begin
               for I in Label_Number'Range loop
                  if Labels (I) (1) /= ' ' then
                     Enum_Info.Names (J) := new String'(Labels (I));
                     J := J + 1;
                  end if;
               end loop;
               Enum_Field := Create (Enum_Info, True);
               Set_Type (FA (2), Enum_Field);
               Set_Background (FA (2), Normal_Video);

               Fh.Drive_Me (Frm, Lines - 3, 0);
               Delete (Frm);
               Update_Panels; Update_Screen;
            end;
         end if;
         for I in FA'Range loop
            Delete (FA (I));
         end loop;
         In_Command := False;
         return K;
      end Command;

   begin
      Set_Timeout_Mode (Win, Delayed, 30000);
      loop
         K := Get_Keystroke (Win);
         if K = Key_None then  -- a timeout occured
            Update_Header_Window;
         elsif K = 3 and then not In_Command  then  -- CTRL-C
            K := Command;
            exit when K /= Key_None;
         else
            exit;
         end if;
      end loop;
      return K;
   end Get_Key;

   procedure Init_Keyboard_Handler is
   begin
      null;
   end Init_Keyboard_Handler;

end Sample.Keyboard_Handler;
