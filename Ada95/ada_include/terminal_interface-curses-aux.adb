------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                      Terminal_Interface.Curses.Aux                       --
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
--  $Revision: 1.3 $
------------------------------------------------------------------------------
package body Terminal_Interface.Curses.Aux is
   --
   --  Some helpers
   procedure Fill_String (Cp  : in  chars_ptr;
                          Str : out String)
   is
      --  Fill the string with the characters referenced by the
      --  chars_ptr.
      --
      Len : Natural;
   begin
      if Cp /= Null_Ptr then
         Len := Natural (Strlen (Cp));
         if Str'Length < Len then
            raise Constraint_Error;
         end if;
         declare
            S : String (1 .. Len);
         begin
            S := Value (Cp);
            Str (Str'First .. (Str'First + Len - 1)) := S (S'Range);
         end;
      else
         Len := 0;
      end if;

      if Len < Str'Length then
         Str ((Str'First + Len) .. Str'Last) := (others => ' ');
      end if;

   end Fill_String;

   procedure Eti_Exception (Code : Eti_Error)
   is
   begin
      case Code is
         when E_Ok              => null;
         when E_System_Error    => raise Eti_System_Error;
         when E_Bad_Argument    => raise Eti_Bad_Argument;
         when E_Posted          => raise Eti_Posted;
         when E_Connected       => raise Eti_Connected;
         when E_Bad_State       => raise Eti_Bad_State;
         when E_No_Room         => raise Eti_No_Room;
         when E_Not_Posted      => raise Eti_Not_Posted;
         when E_Unknown_Command => raise Eti_Unknown_Command;
         when E_No_Match        => raise Eti_No_Match;
         when E_Not_Selectable  => raise Eti_Not_Selectable;
         when E_Not_Connected   => raise Eti_Not_Connected;
         when E_Request_Denied  => raise Eti_Request_Denied;
         when E_Invalid_Field   => raise Eti_Invalid_Field;
         when E_Current         => raise Eti_Current;
      end case;
   end Eti_Exception;

end Terminal_Interface.Curses.Aux;
