------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                   Terminal_Interface.Curses.Text_IO.Aux                  --
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
--  $Revision: 1.4 $
------------------------------------------------------------------------------
package body Terminal_Interface.Curses.Text_IO.Aux is

   procedure Put_Buf
     (Win    : in Window;
      Buf    : in String;
      Width  : in Field;
      Signal : in Boolean := True;
      Ljust  : in Boolean := False)
   is
      L   : Field;
      Len : Field;
      W   : Field := Width;
      LC  : Line_Count;
      CC  : Column_Count;
      Y   : Line_Position;
      X   : Column_Position;

      procedure Output (From, To : Field);

      procedure Output (From, To : Field)
      is
      begin
         if Len > 0 then
            if W = 0 then
               W := Len;
            end if;
            if Len > W then
               --  LRM A10.6 (7) says this
               W := Len;
            end if;

            pragma Assert (Len <= W);
            Get_Size (Win, LC, CC);
            if Column_Count (Len) > CC then
               if Signal then
                  raise Layout_Error;
               else
                  return;
               end if;
            else
               if Len < W and then not Ljust then
                  declare
                     Filler : constant String (1 .. (W - Len))
                       := (others => ' ');
                  begin
                     Put (Win, Filler);
                  end;
               end if;
               Get_Cursor_Position (Win, Y, X);
               if (X + Column_Position (Len)) > CC then
                  New_Line (Win);
               end if;
               Put (Win, Buf (From .. To));
               if Len < W and then Ljust then
                  declare
                     Filler : constant String (1 .. (W - Len))
                       := (others => ' ');
                  begin
                     Put (Win, Filler);
                  end;
               end if;
            end if;
         end if;
      end Output;

   begin
      pragma Assert (Win /= Null_Window);
      if Ljust then
         L := 1;
         for I in 1 .. Buf'Length loop
            exit when Buf (L) = ' ';
            L := L + 1;
         end loop;
         Len := L - 1;
         Output (1, Len);
      else  -- input buffer is not left justified
         L := Buf'Length;
         for I in 1 .. Buf'Length loop
            exit when Buf (L) = ' ';
            L := L - 1;
         end loop;
         Len := Buf'Length - L;
         Output (L + 1, Buf'Length);
      end if;
   end Put_Buf;

end Terminal_Interface.Curses.Text_IO.Aux;

