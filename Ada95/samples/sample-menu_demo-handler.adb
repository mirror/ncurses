------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                          Sample.Menu_Demo.Handler                        --
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
with Sample.Menu_Demo.Aux;
with Sample.Explanation; use Sample.Explanation;

package body Sample.Menu_Demo.Handler is

   package Aux renames Sample.Menu_Demo.Aux;

   procedure Drive_Me (M     : in Menu;
                       Title : in String := "")
   is
      L : Line_Count;
      C : Column_Count;
      Y : Line_Position;
      X : Column_Position;
   begin
      Aux.Geometry (M, L, C, Y, X);
      Drive_Me (M, Y, X, Title);
   end Drive_Me;

   procedure Drive_Me (M     : in Menu;
                       Lin   : in Line_Position;
                       Col   : in Column_Position;
                       Title : in String := "")
   is
      Pan : Panel := Aux.Create (M, Title, Lin, Col);
      V   : Cursor_Visibility := Invisible;
   begin
      Set_Cursor_Visibility (V);
      loop
         declare
            K : Key_Code := Aux.Get_Request (M, Pan);
            R : Driver_Result := Driver (M, K);
         begin
            case R is
               when Menu_Ok => null;
               when Unknown_Request =>
                  if My_Driver (M, K, Pan) then
                     exit;
                  end if;
               when others => Beep;
            end case;
         end;
      end loop;
      Set_Cursor_Visibility (V);
      Aux.Destroy (M, Pan);
   end Drive_Me;

end Sample.Menu_Demo.Handler;
