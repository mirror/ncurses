------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                             Sample.Manifest                              --
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
--  Version Control
--  $Revision: 1.4 $
------------------------------------------------------------------------------
with Terminal_Interface.Curses; use Terminal_Interface.Curses;

package Sample.Manifest is

   QUIT         : constant User_Key_Code    := User_Key_Code'First;
   SELECT_ITEM  : constant User_Key_Code    := QUIT + 1;

   FKEY_HELP    : constant Label_Number     := 1;
   HELP_CODE    : constant Special_Key_Code := Key_F1;
   FKEY_EXPLAIN : constant Label_Number     := 2;
   EXPLAIN_CODE : constant Special_Key_Code := Key_F2;
   FKEY_QUIT    : constant Label_Number     := 3;
   QUIT_CODE    : constant Special_Key_Code := Key_F3;

   Menu_Marker : constant String := "=> ";

   Default_Colors  : constant Redefinable_Color_Pair := 1;
   Menu_Fore_Color : constant Redefinable_Color_Pair := 2;
   Menu_Back_Color : constant Redefinable_Color_Pair := 3;
   Menu_Grey_Color : constant Redefinable_Color_Pair := 4;
   Form_Fore_Color : constant Redefinable_Color_Pair := 5;
   Form_Back_Color : constant Redefinable_Color_Pair := 6;
   Notepad_Color   : constant Redefinable_Color_Pair := 7;
   Help_Color      : constant Redefinable_Color_Pair := 8;
   Header_Color    : constant Redefinable_Color_Pair := 9;

end Sample.Manifest;
