------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--              Terminal_Interface.Curses.Forms.Choice_Field_Types          --
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
--  $Revision: 1.6 $
------------------------------------------------------------------------------
--  You must instantiate this package for any user defined field type
--  to make it visible to the runtime.
--
generic
   type User is new Ada_Defined_Field_Type with private;
   type User_Access is access User;
   with function Field_Check (Fld  : Field;
                              Info : User_Access) return Boolean;
   with function Character_Check (Ch   : Character;
                                  Info : User_Access) return Boolean;
   with function Next_Choice (Fld  : Field;
                              Info : User_Access) return Boolean;
   with function Prev_Choice (Fld  : Field;
                              Info : User_Access) return Boolean;
package Terminal_Interface.Curses.Forms.Choice_Field_Types is
--
--  Nothing public.
--  But we need the body.
    pragma Elaborate_Body;
end Terminal_Interface.Curses.Forms.Choice_Field_Types;
