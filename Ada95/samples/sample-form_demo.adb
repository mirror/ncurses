------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                             Sample.Form_Demo                             --
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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Terminal_Interface.Curses.Panels; use Terminal_Interface.Curses.Panels;
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Terminal_Interface.Curses.Forms.Field_User_Data;
with Terminal_Interface.Curses.Forms.Form_User_Data;

with Sample.Keyboard_Handler; use Sample.Keyboard_Handler;
with Sample.My_Field_Type; use Sample.My_Field_Type;
with Sample.Manifest; use Sample.Manifest;
with Sample.Explanation; use Sample.Explanation;
with Sample.Form_Demo.Aux; use Sample.Form_Demo.Aux;
with Sample.Function_Key_Setting; use Sample.Function_Key_Setting;
with Sample.Form_Demo.Handler;

package body Sample.Form_Demo is

   type User_Data is
      record
         Data : Integer;
      end record;
   type User_Access is access User_Data;

   package Fld_U is new
     Terminal_Interface.Curses.Forms.Field_User_Data (User_Data,
                                                      User_Access);

   package Frm_U is new
     Terminal_Interface.Curses.Forms.Form_User_Data (User_Data,
                                                     User_Access);

   Enums : constant Enum_Array := (new String'("alpha"),
                                   new String'("beta"),
                                   new String'("gamma"));

   Enum_Info : constant Enumeration_Info := (Enums'Length, Enums,
                                             False, False);

   Enum_Field : constant Enumeration_Field := Create (Enum_Info, True);

   procedure Demo
   is

      Mft : My_Data := (Ch => 'X');

      FA : Field_Array (1 .. 9) := (Make (0, 14, "Sample Entry Form"),
                                    Make (2, 0,  "An Enumeration"),
                                    Make (2, 20, "Numeric 1-10"),
                                    Make (2, 34, "Only 'X'"),
                                    Make (5, 0,
                                          "Multiple Lines offscreen (Scroll)"),

                                    Make (Width => 18, Top => 3, Left =>  0),
                                    Make (Width => 12, Top => 3, Left => 20),
                                    Make (Width => 12, Top => 3, Left => 34),
                                    Make (Width => 46, Top => 6, Left =>  0,
                                          Height => 4, Off_Screen => 2)
                                    );

      Frm : Terminal_Interface.Curses.Forms.Form := Create (FA);

      I_F : constant Integer_Field := (Precision   => 0,
                                       Lower_Limit => 1,
                                       Upper_Limit => 10);

      F1, F2 : User_Access;

      package Fh is new Sample.Form_Demo.Handler (Default_Driver);

   begin
      Push_Environment ("FORM00");
      Notepad ("FORM-PAD00");
      Default_Labels;

      Set_Type (FA (6), Enum_Field);
      Set_Type (FA (7), I_F);
      Set_Type (FA (8), Mft);

      F1 := new User_Data'(Data => 4711);
      Fld_U.Set_User_Data (FA (1), F1);

      Fh.Drive_Me (Frm);

      Fld_U.Get_User_Data (FA (1), F2);
      pragma Assert (F1 = F2);
      pragma Assert (F1.Data = F2.Data);

      Pop_Environment;
      Delete (Frm);

      for I in FA'Range loop
         Delete (FA (I));
      end loop;
   end Demo;

end Sample.Form_Demo;
