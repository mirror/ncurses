------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                          Sample.Form_Demo.Handler                        --
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
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Sample.Form_Demo.Aux;
with Sample.Explanation; use Sample.Explanation;

package body Sample.Form_Demo.Handler is

   package Aux renames Sample.Form_Demo.Aux;

   procedure Drive_Me (F     : in Form;
                       Title : in String := "")
   is
      L : Line_Count;
      C : Column_Count;
      Y : Line_Position;
      X : Column_Position;
   begin
      Aux.Geometry (F, L, C, Y, X);
      Drive_Me (F, Y, X, Title);
   end Drive_Me;

   procedure Drive_Me (F     : in Form;
                       Lin   : in Line_Position;
                       Col   : in Column_Position;
                       Title : in String := "")
   is
      Pan : Panel := Aux.Create (F, Title, Lin, Col);
      V   : Cursor_Visibility := Normal;
      Handle_CRLF : Boolean := True;

   begin
      Set_Cursor_Visibility (V);
      if Aux.Count_Active (F) = 1 then
         Handle_CRLF := False;
      end if;
      loop
         declare
            K : Key_Code := Aux.Get_Request (F, Pan, Handle_CRLF);
            R : Driver_Result;
         begin
            if (K = 13 or else K = 10) and then not Handle_CRLF then
               R := Unknown_Request;
            else
               R := Driver (F, K);
            end if;
            case R is
               when Form_Ok => null;
               when Unknown_Request =>
                  if My_Driver (F, K, Pan) then
                     exit;
                  end if;
               when others => Beep;
            end case;
         end;
      end loop;
      Set_Cursor_Visibility (V);
      Aux.Destroy (F, Pan);
   end Drive_Me;

end Sample.Form_Demo.Handler;
