------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                         Sample.Function_Key_Setting                      --
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
--  $Revision: 1.3 $
------------------------------------------------------------------------------
with Ada.Unchecked_Deallocation;
with Sample.Manifest; use  Sample.Manifest;

--  This package implements a simple stack of function key label environments.
--
package body Sample.Function_Key_Setting is

   Max_Label_Length : constant Positive := 8;
   Number_Of_Keys   : Label_Number := Label_Number'Last;
   Justification    : Label_Justification := Left;

   subtype Label is String (1 .. Max_Label_Length);
   type Label_Array is array (Label_Number range <>) of Label;

   type Key_Environment (N : Label_Number := Label_Number'Last);
   type Env_Ptr is access Key_Environment;
   pragma Controlled (Env_Ptr);

   type String_Access is access String;
   pragma Controlled (String_Access);

   Active_Context : String_Access := new String'("MAIN");
   Active_Notepad : Panel := Null_Panel;

   type Key_Environment  (N : Label_Number := Label_Number'Last) is
      record
         Prev    : Env_Ptr;
         Help    : String_Access;
         Notepad : Panel;
         Labels  : Label_Array (1 .. N);
      end record;

   procedure Release_String is
     new Ada.Unchecked_Deallocation (String,
                                     String_Access);

   procedure Release_Environment is
      new Ada.Unchecked_Deallocation (Key_Environment,
                                      Env_Ptr);

   Top_Of_Stack : Env_Ptr := null;

   procedure Push_Environment (Key   : in String;
                               Reset : in Boolean := True)
   is
      P : constant Env_Ptr := new Key_Environment (Number_Of_Keys);
   begin
      --  Store the current labels in the environment
      for I in 1 .. Number_Of_Keys loop
         Get_Soft_Label_Key (I, P.Labels (I));
         if Reset then
            Set_Soft_Label_Key (I, " ");
         end if;
      end loop;
      P.Prev := Top_Of_Stack;
      --  now store active help context and notepad
      P.Help := Active_Context;
      P.Notepad := Active_Notepad;
      --  The notepad must now vanish and the new notepad is empty.
      if (P.Notepad /= Null_Panel) then
         Hide (P.Notepad);
         Update_Panels;
      end if;
      Active_Notepad := Null_Panel;
      Active_Context := new String'(Key);

      Top_Of_Stack := P;
      if Reset then
         Refresh_Soft_Label_Keys_Without_Update;
      end if;
   end Push_Environment;

   procedure Pop_Environment
   is
      P : Env_Ptr := Top_Of_Stack;
   begin
      if Top_Of_Stack = null then
         raise Function_Key_Stack_Error;
      else
         for I in 1 .. Number_Of_Keys loop
            Set_Soft_Label_Key (I, P.Labels (I), Justification);
         end loop;
         pragma Assert (Active_Context /= null);
         Release_String (Active_Context);
         Active_Context := P.Help;
         Refresh_Soft_Label_Keys_Without_Update;
         Notepad_To_Context (P.Notepad);
         Top_Of_Stack := P.Prev;
         Release_Environment (P);
      end if;
   end Pop_Environment;

   function Context return String
   is
   begin
      if Active_Context /= null then
         return Active_Context.all;
      else
         return "";
      end if;
   end Context;

   function Find_Context (Key : String) return Boolean
   is
      P : Env_Ptr := Top_Of_Stack;
   begin
      if Active_Context.all = Key then
         return True;
      else
         loop
            exit when P = null;
            if P.Help.all = Key then
               return True;
            else
               P := P.Prev;
            end if;
         end loop;
         return False;
      end if;
   end Find_Context;

   procedure Notepad_To_Context (Pan : in Panel)
   is
      W : Window;
   begin
      if Active_Notepad /= Null_Panel then
         W := Get_Window (Active_Notepad);
         Clear (W);
         Delete (Active_Notepad);
         Delete (W);
      end if;
      Active_Notepad := Pan;
      if Pan /= Null_Panel then
         Top  (Pan);
      end if;
      Update_Panels;
      Update_Screen;
   end Notepad_To_Context;

   procedure Initialize (Mode : Soft_Label_Key_Format := PC_Style;
                         Just : Label_Justification := Left)
   is
   begin
      case Mode is
         when PC_Style .. PC_Style_With_Index
           => Number_Of_Keys := 12;
         when others
           => Number_Of_Keys := 8;
      end case;
      Init_Soft_Label_Keys (Mode);
      Justification := Just;
   end Initialize;

   procedure Default_Labels
   is
   begin
      Set_Soft_Label_Key (FKEY_QUIT, "Quit");
      Set_Soft_Label_Key (FKEY_HELP, "Help");
      Set_Soft_Label_Key (FKEY_EXPLAIN, "Keys");
      Refresh_Soft_Label_Keys_Without_Update;
   end Default_Labels;

   function Notepad_Window return Window
   is
   begin
      if Active_Notepad /= Null_Panel then
         return Get_Window (Active_Notepad);
      else
         return Null_Window;
      end if;
   end Notepad_Window;

end Sample.Function_Key_Setting;
