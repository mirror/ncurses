------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                     Terminal_Interface.Curses.Text_IO                    --
--                                                                          --
--                                 S P E C                                  --
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
--  $Revision: 1.5 $
------------------------------------------------------------------------------
with System;
with System.Parameters;
with Ada.Text_IO;
with Ada.IO_Exceptions;

package Terminal_Interface.Curses.Text_IO is

   use type Ada.Text_IO.Count;
   subtype Count is Ada.Text_IO.Count;
   subtype Positive_Count is Count range 1 .. Count'Last;

   subtype Field is Integer range 0 .. System.Parameters.Field_Max;
   subtype Number_Base is Integer range 2 .. 16;

   type Type_Set is (Lower_Case, Upper_Case, Mixed_Case);

   --  For most of the routines you will see a version without a Window
   --  type parameter. They will operate on a default window, which can
   --  be set by the user. It is initially equal to Standard_Window.

   procedure Set_Window (Win : in Window);
   --  Set Win as the default window

   function Get_Window return Window;
   --  Get the current default window

   procedure Flush (Win : in Window);
   procedure Flush;

   --------------------------------------------
   -- Specification of line and page lengths --
   --------------------------------------------

   --  There are no set routines in this package. I assume, that you allocate
   --  the window with an appropriate size.
   --  A scroll-window is interpreted as an page with unbounded page length,
   --  i.e. it returns the conventional 0 as page length.

   function Line_Length (Win : in Window) return Count;
   function Line_Length return Count;

   function Page_Length (Win : in Window) return Count;
   function Page_Length return Count;

   ------------------------------------
   -- Column, Line, and Page Control --
   ------------------------------------
   procedure New_Line (Win : in Window; Spacing : in Positive_Count := 1);
   procedure New_Line (Spacing : in Positive_Count := 1);

   procedure New_Page (Win : in Window);
   procedure New_Page;

   procedure Set_Col (Win : in Window;  To : in Positive_Count);
   procedure Set_Col (To : in Positive_Count);

   procedure Set_Line (Win : in Window; To : in Positive_Count);
   procedure Set_Line (To : in Positive_Count);

   function Col (Win : in Window) return Positive_Count;
   function Col return Positive_Count;

   function Line (Win : in Window) return Positive_Count;
   function Line return Positive_Count;

   -----------------------
   -- Characters-Output --
   -----------------------

   procedure Put (Win  : in Window; Item : in Character);
   procedure Put (Item : in Character);

   --------------------
   -- Strings-Output --
   --------------------

   procedure Put (Win  : in Window; Item : in String);
   procedure Put (Item : in String);

   procedure Put_Line
     (Win  : in Window;
      Item : in String);

   procedure Put_Line
     (Item : in String);

   --  Exceptions

   Status_Error : exception renames Ada.IO_Exceptions.Status_Error;
   Mode_Error   : exception renames Ada.IO_Exceptions.Mode_Error;
   Name_Error   : exception renames Ada.IO_Exceptions.Name_Error;
   Use_Error    : exception renames Ada.IO_Exceptions.Use_Error;
   Device_Error : exception renames Ada.IO_Exceptions.Device_Error;
   End_Error    : exception renames Ada.IO_Exceptions.End_Error;
   Data_Error   : exception renames Ada.IO_Exceptions.Data_Error;
   Layout_Error : exception renames Ada.IO_Exceptions.Layout_Error;

end Terminal_Interface.Curses.Text_IO;
