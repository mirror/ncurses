------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                     Terminal_Interface.Curses.Text_IO                    --
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
with System;

package body Terminal_Interface.Curses.Text_IO is

   Default_Window : Window;

   procedure Set_Window (Win : in Window)
   is
   begin
      Default_Window := Win;
   end Set_Window;

   function Get_Window return Window
   is
   begin
      if Default_Window = Null_Window then
         return Standard_Window;
      else
         return Default_Window;
      end if;
   end Get_Window;
   pragma Inline (Get_Window);

   procedure Flush (Win : in Window)
   is
   begin
      Refresh (Win);
   end Flush;

   procedure Flush
   is
   begin
      Flush (Get_Window);
   end Flush;

   --------------------------------------------
   -- Specification of line and page lengths --
   --------------------------------------------

   --  There are no set routines in this package. I assume, that you allocate
   --  the window with an appropriate size.
   --  A scroll-window is interpreted as an page with unbounded page length,
   --  i.e. it returns the conventional 0 as page length.

   function Line_Length (Win : in Window) return Count
   is
      N_Lines : Line_Count;
      N_Cols  : Column_Count;
   begin
      Get_Size (Win, N_Lines, N_Cols);
      if Natural (N_Cols) > Natural (Count'Last) then
         raise Layout_Error;
      end if;
      return Count (N_Cols);
   end Line_Length;

   function Line_Length return Count
   is
   begin
      return Line_Length (Get_Window);
   end Line_Length;

   function Page_Length (Win : in Window) return Count
   is
      N_Lines : Line_Count;
      N_Cols  : Column_Count;
   begin
      if Scrolling_Allowed (Win) then
         return 0;
      else
         Get_Size (Win, N_Lines, N_Cols);
         if Natural (N_Lines) > Natural (Count'Last) then
            raise Layout_Error;
         end if;
         return Count (N_Lines);
      end if;
   end Page_Length;

   function Page_Length return Count
   is
   begin
      return Page_Length (Get_Window);
   end Page_Length;

   ------------------------------------
   -- Column, Line, and Page Control --
   ------------------------------------
   procedure New_Line (Win : in Window; Spacing : in Positive_Count := 1)
   is
      P_Size : constant Count := Page_Length (Win);
   begin
      if Spacing not in Positive_Count then
         raise Constraint_Error;
      end if;

      for I in 1 .. Spacing loop
         if P_Size > 0 and then Line (Win) >= P_Size then
            New_Page (Win);
         else
            Add (Win, Ascii.LF);
         end if;
      end loop;
   end New_Line;

   procedure New_Line (Spacing : in Positive_Count := 1)
   is
   begin
      New_Line (Get_Window, Spacing);
   end New_Line;

   procedure New_Page (Win : in Window)
   is
   begin
      Clear (Win);
   end New_Page;

   procedure New_Page
   is
   begin
      New_Page (Get_Window);
   end New_Page;

   procedure Set_Col (Win : in Window;  To : in Positive_Count)
   is
      Y  : Line_Position;
      X1 : Column_Position;
      X2 : Column_Position;
      N  : Natural;
   begin
      if To not in Positive_Count then
         raise Constraint_Error;
      end if;

      Get_Cursor_Position (Win, Y, X1);
      N  := Natural (To); N := N - 1;
      X2 := Column_Position (N);
      if X1 > X2 then
         New_Line (Win, 1);
         X1 := 0;
      end if;
      if X1 < X2 then
         declare
            Filler : constant String (Integer (X1) .. (Integer (X2) - 1))
              := (others => ' ');
         begin
            Put (Win, Filler);
         end;
      end if;
   end Set_Col;

   procedure Set_Col (To : in Positive_Count)
   is
   begin
      Set_Col (Get_Window, To);
   end Set_Col;

   procedure Set_Line (Win : in Window; To : in Positive_Count)
   is
      Y1 : Line_Position;
      Y2 : Line_Position;
      X  : Column_Position;
      N  : Natural;
   begin
      if To not in Positive_Count then
         raise Constraint_Error;
      end if;

      Get_Cursor_Position (Win, Y1, X);
      N  := Natural (To); N := N - 1;
      Y2 := Line_Position (N);
      if Y2 < Y1 then
         New_Page (Win);
         Y1 := 0;
      end if;
      if Y1 < Y2 then
         New_Line (Win, Positive_Count (Y2 - Y1));
      end if;
   end Set_Line;

   procedure Set_Line (To : in Positive_Count)
   is
   begin
      Set_Line (Get_Window, To);
   end Set_Line;

   function Col (Win : in Window) return Positive_Count
   is
      Y : Line_Position;
      X : Column_Position;
      N : Natural;
   begin
      Get_Cursor_Position (Win, Y, X);
      N := Natural (X); N := N + 1;
      if N > Natural (Count'Last) then
         raise Layout_Error;
      end if;
      return Positive_Count (N);
   end Col;

   function Col return Positive_Count
   is
   begin
      return Col (Get_Window);
   end Col;

   function Line (Win : in Window) return Positive_Count
   is
      Y : Line_Position;
      X : Column_Position;
      N : Natural;
   begin
      Get_Cursor_Position (Win, Y, X);
      N := Natural (Y); N := N + 1;
      if N > Natural (Count'Last) then
         raise Layout_Error;
      end if;
      return Positive_Count (N);
   end Line;

   function Line return Positive_Count
   is
   begin
      return Line (Get_Window);
   end Line;

   -----------------------
   -- Characters Output --
   -----------------------

   procedure Put (Win  : in Window; Item : in Character)
   is
      P_Size : constant Count := Page_Length (Win);
      Y : Line_Position;
      X : Column_Position;
      L : Line_Count;
      C : Column_Count;
   begin
      if P_Size > 0 then
         Get_Cursor_Position (Win, Y, X);
         Get_Size (Win, L, C);
         if (Y + 1) = L and then (X + 1) = C then
            New_Page (Win);
         end if;
      end if;
      Add (Win, Item);
   end Put;

   procedure Put (Item : in Character)
   is
   begin
      Put (Get_Window, Item);
   end Put;

   --------------------
   -- Strings-Output --
   --------------------

   procedure Put (Win  : in Window; Item : in String)
   is
      P_Size : constant Count := Page_Length (Win);
      Y : Line_Position;
      X : Column_Position;
      L : Line_Count;
      C : Column_Count;
   begin
      if P_Size > 0 then
         Get_Cursor_Position (Win, Y, X);
         Get_Size (Win, L, C);
         if (Y + 1) = L and then (X + 1 + Item'Length) >= C then
            New_Page (Win);
         end if;
      end if;
      Add (Win, Item);
   end Put;

   procedure Put (Item : in String)
   is
   begin
      Put (Get_Window, Item);
   end Put;

   procedure Put_Line
     (Win  : in Window;
      Item : in String)
   is
   begin
      Put (Win, Item);
      New_Line (Win, 1);
   end Put_Line;

   procedure Put_Line
     (Item : in String)
   is
   begin
      Put_Line (Get_Window, Item);
   end Put_Line;

begin
   Default_Window := Null_Window;

end Terminal_Interface.Curses.Text_IO;
