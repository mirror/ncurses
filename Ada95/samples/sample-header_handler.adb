------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                            Sample.Header_Handler                         --
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
with Ada.Calendar; use Ada.Calendar;
with Terminal_Interface.Curses.Text_IO.Integer_IO;
with Sample.Manifest; use Sample.Manifest;

--  This package handles the painting of the header line of the screen.
--
package body Sample.Header_Handler is

   package Int_IO is new
     Terminal_Interface.Curses.Text_IO.Integer_IO (Integer);
   use Int_IO;

   Header_Window : Window := Null_Window;

   Display_Hour  : Integer := -1; -- hour last displayed
   Display_Min   : Integer := -1; -- minute last displayed
   Display_Day   : Integer := -1; -- day last displayed
   Display_Month : Integer := -1; -- month last displayed

   --  This is the routine handed over to the curses library to be called
   --  as initialization routine when ripping of the header lines from
   --  the screen. This routine must follow C conventions.
   function Init_Header_Window (Win     : Window;
                                Columns : Column_Count) return Integer;
   pragma Convention (C, Init_Header_Window);

   procedure Internal_Update_Header_Window (Do_Update : in Boolean);


   --  The initialization must be called before Init_Screen. It steals two
   --  lines from the top of the screen.
   procedure Init_Header_Handler
   is
   begin
      Rip_Off_Lines (2, Init_Header_Window'Access);
   end Init_Header_Handler;

   procedure N_Out (N : in Integer);

   --  Emit a two digit number and ensure that a leading zero is generated if
   --  necessary.
   procedure N_Out (N : in Integer)
   is
   begin
      if N < 10 then
         Add (Header_Window, '0');
         Put (Header_Window, N, 1);
      else
         Put (Header_Window, N, 2);
      end if;
   end N_Out;

   --  Paint the header window. The input parameter is a flag indicating
   --  whether or not the screen should be updated physically after painting.
   procedure Internal_Update_Header_Window (Do_Update : in Boolean)
   is
      type Month_Name_Array is
         array (Month_Number'First .. Month_Number'Last) of String (1 .. 9);

      Month_Names : constant Month_Name_Array :=
        ("January  ",
         "February ",
         "March    ",
         "April    ",
         "May      ",
         "June     ",
         "July     ",
         "August   ",
         "September",
         "October  ",
         "November ",
         "December ");

      Now : Time := Clock;
      Sec : Integer := Integer (Seconds (Now));
      Hour   : Integer := Sec / 3600;
      Minute : Integer := (Sec - Hour * 3600) / 60;
      Mon    : Month_Number := Month (Now);
      D      : Day_Number   := Day (Now);
   begin
      if Header_Window /= Null_Window then
         if Minute /= Display_Min or else Hour /= Display_Hour
           or else Display_Day /= D or else Display_Month /= Mon then
            Move_Cursor (Header_Window, 0, 0);
            N_Out (D); Add (Header_Window, '.');
            Add (Header_Window, Month_Names (Mon));
            Move_Cursor (Header_Window, 1, 0);
            N_Out (Hour); Add (Header_Window, ':');
            N_Out (Minute);
            Display_Min   := Minute;
            Display_Hour  := Hour;
            Display_Month := Mon;
            Display_Day   := D;
            Refresh_Without_Update (Header_Window);
            if Do_Update then
               Update_Screen;
            end if;
         end if;
      end if;
   end Internal_Update_Header_Window;

   --  This routine is called in the keyboard input timeout handler. So it will
   --  periodically update the header line of the screen.
   procedure Update_Header_Window
   is
   begin
      Internal_Update_Header_Window (True);
   end Update_Header_Window;

   function Init_Header_Window (Win     : Window;
                                Columns : Column_Count) return Integer
   is
      Title  : constant String := "Ada 95 ncurses Binding Sample";
      Pos    : Column_Position;
   begin
      Header_Window := Win;
      if Win /= Null_Window then
         if Has_Colors then
            Set_Background (Win => Win,
                            Ch  => (Ch    => ' ',
                                    Color => Header_Color,
                                    Attr  => Normal_Video));
            Set_Character_Attributes (Win   => Win,
                                      Attr  => Normal_Video,
                                      Color => Header_Color);
            Erase (Win);
         end if;
         Leave_Cursor_After_Update (Win, True);
         Pos := Columns - Column_Position (Title'Length);
         Add (Win, 0, Pos / 2, Title);
         --  In this phase we must not allow a physical update, because
         --  ncurses isn´t properly initialized at this point.
         Internal_Update_Header_Window (False);
         return 0;
      else
         return -1;
      end if;
   end Init_Header_Window;

end Sample.Header_Handler;
