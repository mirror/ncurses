------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                      Terminal_Interface.Curses.Forms                     --
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
--  $Revision: 1.9 $
------------------------------------------------------------------------------
with Ada.Tags; use Ada.Tags;
with Ada.Unchecked_Deallocation;
with Unchecked_Conversion;

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Terminal_Interface.Curses.Aux; use Terminal_Interface.Curses.Aux;
with GNAT.Htable;

package body Terminal_Interface.Curses.Forms is

------------------------------------------------------------------------------
   --  |
   --  |
   --  |
   --  subtype chars_ptr is Interfaces.C.Strings.chars_ptr;

   function FOS_2_CInt is new
     Unchecked_Conversion (Field_Option_Set,
                           C_Int);

   function CInt_2_FOS is new
     Unchecked_Conversion (C_Int,
                           Field_Option_Set);

   function FrmOS_2_CInt is new
     Unchecked_Conversion (Form_Option_Set,
                           C_Int);

   function CInt_2_FrmOS is new
     Unchecked_Conversion (C_Int,
                           Form_Option_Set);

   procedure Request_Name (Key  : in Form_Request_Code;
                                Name : out String)
   is
      function Form_Request_Name (Key : C_Int) return chars_ptr;
      pragma Import (C, Form_Request_Name, "form_request_name");
   begin
      Fill_String (Form_Request_Name (C_Int (Key)), Name);
   end Request_Name;
------------------------------------------------------------------------------
   procedure Free_Field_User_Wrapper is
     new Ada.Unchecked_Deallocation (Field_User_Wrapper,
                                     Field_User_Wrapper_Access);

   procedure Release_User_Wrapper (A : in out Field_User_Wrapper_Access);
   procedure Dup_User_Wrapper (A : in out Field_User_Wrapper_Access);

   procedure Release_User_Wrapper (A : in out Field_User_Wrapper_Access)
   is
   begin
      A.N := A.N - 1;
      if A.N = 0 then
         Free_Field_User_Wrapper (A);
      end if;
   end Release_User_Wrapper;
   pragma Inline (Release_User_Wrapper);

   procedure Dup_User_Wrapper (A : in out Field_User_Wrapper_Access)
   is
   begin
      A.N := A.N + 1;
   end Dup_User_Wrapper;
   pragma Inline (Dup_User_Wrapper);
------------------------------------------------------------------------------
   procedure Free_Form_User_Wrapper is
     new Ada.Unchecked_Deallocation (Form_User_Wrapper,
                                     Form_User_Wrapper_Access);
   --  |
   --  |
   --  |
   --  |
   --  |=====================================================================
   --  | man page form_field_new.3x
   --  |=====================================================================
   --  |
   --  |
   --  |
   function Create (Height       : Line_Count;
                    Width        : Column_Count;
                    Top          : Line_Position;
                    Left         : Column_Position;
                    Off_Screen   : Natural := 0;
                    More_Buffers : Buffer_Number := Buffer_Number'First)
                    return Field
   is
      function Newfield (H, W, T, L, O, M : C_Int) return Field;
      pragma Import (C, Newfield, "new_field");
      Fld : constant Field := Newfield (C_Int (Height), C_Int (Width),
                                        C_Int (Top), C_Int (Left),
                                        C_Int (Off_Screen),
                                        C_Int (More_Buffers));

      A   : Field_User_Wrapper_Access;
      Res : Eti_Error;
   begin
      if Fld = Null_Field then
         raise Form_Exception;
      else
         A := new Field_User_Wrapper'(U => System.Null_Address,
                                      T => null,
                                      N => 1);
         Res := Set_Field_Userptr (Fld, A);
         if Res /= E_Ok then
            Free_Field_User_Wrapper (A);
            Eti_Exception (Res);
         end if;
      end if;
      return Fld;
   end Create;
--  |
--  |
--  |
   procedure Delete (Fld : in out Field)
   is
      function Free_Field (Fld : Field) return C_Int;
      pragma Import (C, Free_Field, "free_field");
      procedure Free_Field_Type is
        new Ada.Unchecked_Deallocation (Field_Type'Class,
                                        Field_Type_Access);

      A   : Field_User_Wrapper_Access := Field_Userptr (Fld);
      Res : Eti_Error;
   begin
      if A /= null then
         if A.T /= null then
            Free_Field_Type (A.T);
         end if;
         Release_User_Wrapper (A);
      end if;
      Res := Free_Field (Fld);
      if Res /= E_Ok then
         Eti_Exception (Res);
      end if;
      Fld := Null_Field;
   end Delete;
   --  |
   --  |
   --  |
   function Duplicate (Fld  : Field;
                       Top  : Line_Position;
                       Left : Column_Position) return Field
   is
      function Dup_Field (Fld  : Field;
                          Top  : C_Int;
                          Left : C_Int) return Field;
      pragma Import (C, Dup_Field, "dup_field");

      A : Field_User_Wrapper_Access := Field_Userptr (Fld);
      F : constant Field := Dup_Field (Fld,
                                       C_Int (Top),
                                       C_Int (Left));
   begin
      if F = Null_Field then
         raise Form_Exception;
      else
         Dup_User_Wrapper (A);
      end if;
      return F;
   end Duplicate;
   --  |
   --  |
   --  |
   function Link (Fld  : Field;
                  Top  : Line_Position;
                  Left : Column_Position) return Field
   is
      function Lnk_Field (Fld  : Field;
                          Top  : C_Int;
                          Left : C_Int) return Field;
      pragma Import (C, Lnk_Field, "link_field");

      A : Field_User_Wrapper_Access := Field_Userptr (Fld);
      F : constant Field := Lnk_Field (Fld,
                                       C_Int (Top),
                                       C_Int (Left));
   begin
      if F = Null_Field then
         raise Form_Exception;
      else
         Dup_User_Wrapper (A);
      end if;
      return F;
   end Link;
   --  |
   --  |=====================================================================
   --  | man page form_field_just.3x
   --  |=====================================================================
   --  |
   --  |
   --  |
   procedure Set_Justification (Fld  : in Field;
                                Just : in Field_Justification := None)
   is
      function Set_Field_Just (Fld  : Field;
                               Just : C_Int) return C_Int;
      pragma Import (C, Set_Field_Just, "set_field_just");

      Res : constant Eti_Error :=
        Set_Field_Just (Fld,
                        C_Int (Field_Justification'Pos (Just)));
   begin
      if Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Justification;
   --  |
   --  |
   --  |
   function Get_Justification (Fld : Field) return Field_Justification
   is
      function Field_Just (Fld : Field) return C_Int;
      pragma Import (C, Field_Just, "field_just");
   begin
      return Field_Justification'Val (Field_Just (Fld));
   end Get_Justification;
   --  |
   --  |=====================================================================
   --  | man page form_field_buffer.3x
   --  |=====================================================================
   --  |
   --  |
   --  |
   procedure Set_Buffer
     (Fld    : in Field;
      Buffer : in Buffer_Number := Buffer_Number'First;
      Str    : in String)
   is
      type Char_Ptr is access all Interfaces.C.Char;
      function Set_Fld_Buffer (Fld    : Field;
                                 Bufnum : C_Int;
                                 S      : Char_Ptr)
        return C_Int;
      pragma Import (C, Set_Fld_Buffer, "set_field_buffer");

      Txt : char_array (0 .. Str'Length);
      Len : size_t;
      Res : Eti_Error;
   begin
      To_C (Str, Txt, Len);
      Res := Set_Fld_Buffer (Fld, C_Int (Buffer), Txt (Txt'First)'Access);
      if Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Buffer;
   --  |
   --  |
   --  |
   procedure Get_Buffer
     (Fld    : in Field;
      Buffer : in Buffer_Number := Buffer_Number'First;
      Str    : out String)
   is
      function Field_Buffer (Fld : Field;
                             B   : C_Int) return chars_ptr;
      pragma Import (C, Field_Buffer, "field_buffer");
   begin
      Fill_String (Field_Buffer (Fld, C_Int (Buffer)), Str);
   end Get_Buffer;
   --  |
   --  |
   --  |
   procedure Set_Status (Fld    : in Field;
                         Status : in Boolean := True)
   is
      function Set_Fld_Status (Fld : Field;
                               St  : C_Int) return C_Int;
      pragma Import (C, Set_Fld_Status, "set_field_status");

      Res : constant Eti_Error := Set_Fld_Status (Fld, Boolean'Pos (Status));
   begin
      if Res /= E_Ok then
         raise Form_Exception;
      end if;
   end Set_Status;
   --  |
   --  |
   --  |
   function Changed (Fld : Field) return Boolean
   is
      function Field_Status (Fld : Field) return C_Int;
      pragma Import (C, Field_Status, "field_status");

      Res : constant C_Int := Field_Status (Fld);
   begin
      if Res = Curses_False then
         return False;
      else
         return True;
      end if;
   end Changed;
   --  |
   --  |
   --  |
   procedure Set_Maximum_Size (Fld : in Field;
                               Max : in Natural := 0)
   is
      function Set_Field_Max (Fld : Field;
                              M   : C_Int) return C_Int;
      pragma Import (C, Set_Field_Max, "set_max_field");

      Res : constant Eti_Error := Set_Field_Max (Fld, C_Int (Max));
   begin
      if Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Maximum_Size;
   --  |
   --  |=====================================================================
   --  | man page form_field_opts.3x
   --  |=====================================================================
   --  |
   --  |
   --  |
   procedure Normalize_Field_Options (Options : in out C_Int);
   pragma Import (C, Normalize_Field_Options, "_nc_ada_normalize_field_opts");

   procedure Set_Options (Fld     : in Field;
                          Options : in Field_Option_Set)
   is
      function Set_Field_Opts (Fld : Field;
                               Opt : C_Int) return C_Int;
      pragma Import (C, Set_Field_Opts, "set_field_opts");

      Opt : C_Int := FOS_2_CInt (Options);
      Res : Eti_Error;
   begin
      Normalize_Field_Options (Opt);
      Res := Set_Field_Opts (Fld, Opt);
      if Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Options;
   --  |
   --  |
   --  |
   procedure Switch_Options (Fld     : in Field;
                             Options : in Field_Option_Set;
                             On      : Boolean := True)
   is
      function Field_Opts_On (Fld : Field;
                              Opt : C_Int) return C_Int;
      pragma Import (C, Field_Opts_On, "field_opts_on");
      function Field_Opts_Off (Fld : Field;
                               Opt : C_Int) return C_Int;
      pragma Import (C, Field_Opts_Off, "field_opts_off");

      Err : Eti_Error;
      Opt : C_Int := FOS_2_CInt (Options);
   begin
      Normalize_Field_Options (Opt);
      if On then
         Err := Field_Opts_On (Fld, Opt);
      else
         Err := Field_Opts_Off (Fld, Opt);
      end if;
      if Err /= E_Ok then
         Eti_Exception (Err);
      end if;
   end Switch_Options;
   --  |
   --  |
   --  |
   procedure Get_Options (Fld     : in  Field;
                          Options : out Field_Option_Set)
   is
      function Field_Opts (Fld : Field) return C_Int;
      pragma Import (C, Field_Opts, "field_opts");

      Res : C_Int := Field_Opts (Fld);
   begin
      Normalize_Field_Options (Res);
      Options := CInt_2_FOS (Res);
   end Get_Options;
   --  |
   --  |
   --  |
   function Get_Options (Fld : Field := Null_Field)
                         return Field_Option_Set
   is
      Fos : Field_Option_Set;
   begin
      Get_Options (Fld, Fos);
      return Fos;
   end Get_Options;
   --  |
   --  |=====================================================================
   --  | man page form_field_attributes.3x
   --  |=====================================================================
   --  |
   --  |
   --  |
   procedure Set_Foreground
     (Fld   : in Field;
      Fore  : in Character_Attribute_Set := Normal_Video;
      Color : in Color_Pair := Color_Pair'First)
   is
      function Set_Field_Fore (Fld  : Field;
                               Attr : C_Int) return C_Int;
      pragma Import (C, Set_Field_Fore, "set_field_fore");

      Ch : constant Attributed_Character := (Ch    => Character'First,
                                             Color => Color,
                                             Attr  => Fore);
      Res : constant Eti_Error := Set_Field_Fore (Fld, Chtype_To_CInt (Ch));
   begin
      if  Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Foreground;
   --  |
   --  |
   --  |
   procedure Foreground (Fld  : in  Field;
                         Fore : out Character_Attribute_Set)
   is
      function Field_Fore (Fld : Field) return C_Int;
      pragma Import (C, Field_Fore, "field_fore");
   begin
      Fore := CInt_To_Chtype (Field_Fore (Fld)).Attr;
   end Foreground;

   procedure Foreground (Fld   : in  Field;
                         Fore  : out Character_Attribute_Set;
                         Color : out Color_Pair)
   is
      function Field_Fore (Fld : Field) return C_Int;
      pragma Import (C, Field_Fore, "field_fore");
   begin
      Fore  := CInt_To_Chtype (Field_Fore (Fld)).Attr;
      Color := CInt_To_Chtype (Field_Fore (Fld)).Color;
   end Foreground;
   --  |
   --  |
   --  |
   procedure Set_Background
     (Fld   : in Field;
      Back  : in Character_Attribute_Set := Normal_Video;
      Color : in Color_Pair := Color_Pair'First)
   is
      function Set_Field_Back (Fld  : Field;
                               Attr : C_Int) return C_Int;
      pragma Import (C, Set_Field_Back, "set_field_back");

      Ch : constant Attributed_Character := (Ch    => Character'First,
                                             Color => Color,
                                             Attr  => Back);
      Res : constant Eti_Error := Set_Field_Back (Fld, Chtype_To_CInt (Ch));
   begin
      if  Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Background;
   --  |
   --  |
   --  |
   procedure Background (Fld  : in  Field;
                         Back : out Character_Attribute_Set)
   is
      function Field_Back (Fld : Field) return C_Int;
      pragma Import (C, Field_Back, "field_back");
   begin
      Back := CInt_To_Chtype (Field_Back (Fld)).Attr;
   end Background;

   procedure Background (Fld   : in  Field;
                         Back  : out Character_Attribute_Set;
                         Color : out Color_Pair)
   is
      function Field_Back (Fld : Field) return C_Int;
      pragma Import (C, Field_Back, "field_back");
   begin
      Back  := CInt_To_Chtype (Field_Back (Fld)).Attr;
      Color := CInt_To_Chtype (Field_Back (Fld)).Color;
   end Background;
   --  |
   --  |
   --  |
   procedure Set_Pad_Character (Fld : in Field;
                                Pad : in Character := Space)
   is
      function Set_Field_Pad (Fld : Field;
                              Ch  : C_Int) return C_Int;
      pragma Import (C, Set_Field_Pad, "set_field_pad");

      Res : constant Eti_Error := Set_Field_Pad (Fld,
                                                 C_Int (Character'Pos (Pad)));
   begin
      if Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Pad_Character;
   --  |
   --  |
   --  |
   procedure Pad_Character (Fld : in  Field;
                            Pad : out Character)
   is
      function Field_Pad (Fld : Field) return C_Int;
      pragma Import (C, Field_Pad, "field_pad");
   begin
      Pad := Character'Val (Field_Pad (Fld));
   end Pad_Character;
   --  |
   --  |=====================================================================
   --  | man page form_field_info.3x
   --  |=====================================================================
   --  |
   --  |
   --  |
   procedure Info (Fld                : in  Field;
                   Lines              : out Line_Count;
                   Columns            : out Column_Count;
                   First_Row          : out Line_Position;
                   First_Column       : out Column_Position;
                   Off_Screen         : out Natural;
                   Additional_Buffers : out Buffer_Number)
   is
      type C_Int_Access is access all C_Int;
      function Fld_Info (Fld : Field;
                         L, C, Fr, Fc, Os, Ab : C_Int_Access)
                         return C_Int;
      pragma Import (C, Fld_Info, "field_info");

      L, C, Fr, Fc, Os, Ab : aliased C_Int;
      Res : constant Eti_Error := Fld_Info (Fld,
                                            L'Access, C'Access,
                                            Fr'Access, Fc'Access,
                                            Os'Access, Ab'Access);
   begin
      if Res /= E_Ok then
         Eti_Exception (Res);
      else
         Lines              := Line_Count (L);
         Columns            := Column_Count (C);
         First_Row          := Line_Position (Fr);
         First_Column       := Column_Position (Fc);
         Off_Screen         := Natural (Os);
         Additional_Buffers := Buffer_Number (Ab);
      end if;
   end Info;
--  |
--  |
--  |
   procedure Dynamic_Info (Fld     : in Field;
                           Lines   : out Line_Count;
                           Columns : out Column_Count;
                           Max     : out Natural)
   is
      type C_Int_Access is access all C_Int;
      function Dyn_Info (Fld : Field; L, C, M : C_Int_Access) return C_Int;
      pragma Import (C, Dyn_Info, "dynamic_field_info");

      L, C, M : aliased C_Int;
      Res : constant Eti_Error := Dyn_Info (Fld,
                                            L'Access, C'Access,
                                            M'Access);
   begin
      if Res /= E_Ok then
         Eti_Exception (Res);
      else
         Lines   := Line_Count (L);
         Columns := Column_Count (C);
         Max     := Natural (M);
      end if;
   end Dynamic_Info;
   --  |
   --  |=====================================================================
   --  | man page form_win.3x
   --  |=====================================================================
   --  |
   --  |
   --  |
   procedure Set_Window (Frm : in Form;
                         Win : in Window)
   is
      function Set_Form_Win (Frm : Form;
                             Win : Window) return C_Int;
      pragma Import (C, Set_Form_Win, "set_form_win");

      Res : constant Eti_Error := Set_Form_Win (Frm, Win);
   begin
      if  Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Window;
   --  |
   --  |
   --  |
   function Get_Window (Frm : Form) return Window
   is
      function Form_Win (Frm : Form) return Window;
      pragma Import (C, Form_Win, "form_win");

      W : constant Window := Form_Win (Frm);
   begin
      return W;
   end Get_Window;
   --  |
   --  |
   --  |
   procedure Set_Sub_Window (Frm : in Form;
                             Win : in Window)
   is
      function Set_Form_Sub (Frm : Form;
                             Win : Window) return C_Int;
      pragma Import (C, Set_Form_Sub, "set_form_sub");

      Res : constant Eti_Error := Set_Form_Sub (Frm, Win);
   begin
      if  Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Sub_Window;
   --  |
   --  |
   --  |
   function Get_Sub_Window (Frm : Form) return Window
   is
      function Form_Sub (Frm : Form) return Window;
      pragma Import (C, Form_Sub, "form_sub");

      W : constant Window := Form_Sub (Frm);
   begin
      return W;
   end Get_Sub_Window;
   --  |
   --  |
   --  |
   procedure Scale (Frm     : in Form;
                    Lines   : out Line_Count;
                    Columns : out Column_Count)
   is
      type C_Int_Access is access all C_Int;
      function M_Scale (Frm : Form; Yp, Xp : C_Int_Access) return C_Int;
      pragma Import (C, M_Scale, "scale_form");

      X, Y : aliased C_Int;
      Res  : constant Eti_Error := M_Scale (Frm, Y'Access, X'Access);
   begin
      if Res /= E_Ok then
         Eti_Exception (Res);
      end if;
      Lines := Line_Count (Y);
      Columns := Column_Count (X);
   end Scale;
   --  |
   --  |=====================================================================
   --  | man page menu_hook.3x
   --  |=====================================================================
   --  |
   --  |
   --  |
   procedure Set_Field_Init_Hook (Frm  : in Form;
                                  Proc : in Form_Hook_Function)
   is
      function Set_Field_Init (Frm  : Form;
                               Proc : Form_Hook_Function) return C_Int;
      pragma Import (C, Set_Field_Init, "set_field_init");

      Res : constant Eti_Error := Set_Field_Init (Frm, Proc);
   begin
      if  Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Field_Init_Hook;
   --  |
   --  |
   --  |
   procedure Set_Field_Term_Hook (Frm  : in Form;
                                  Proc : in Form_Hook_Function)
   is
      function Set_Field_Term (Frm  : Form;
                               Proc : Form_Hook_Function) return C_Int;
      pragma Import (C, Set_Field_Term, "set_field_term");

      Res : constant Eti_Error := Set_Field_Term (Frm, Proc);
   begin
      if Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Field_Term_Hook;
   --  |
   --  |
   --  |
   procedure Set_Form_Init_Hook (Frm  : in Form;
                                 Proc : in Form_Hook_Function)
   is
      function Set_Form_Init (Frm  : Form;
                              Proc : Form_Hook_Function) return C_Int;
      pragma Import (C, Set_Form_Init, "set_form_init");

      Res : constant Eti_Error := Set_Form_Init (Frm, Proc);
   begin
      if  Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Form_Init_Hook;
   --  |
   --  |
   --  |
   procedure Set_Form_Term_Hook (Frm  : in Form;
                                 Proc : in Form_Hook_Function)
   is
      function Set_Form_Term (Frm  : Form;
                              Proc : Form_Hook_Function) return C_Int;
      pragma Import (C, Set_Form_Term, "set_form_term");

      Res : constant Eti_Error := Set_Form_Term (Frm, Proc);
   begin
      if Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Form_Term_Hook;
   --  |
   --  |=====================================================================
   --  | man page form_fields.3x
   --  |=====================================================================
   --  |
   --  |
   --  |
   procedure Free_Allocated_Fields is
     new Ada.Unchecked_Deallocation (Field_Array, Field_Array_Access);
   --  |
   --  |
   --  |
   --  This is a bit delicate if we want to manipulate an Ada created form
   --  from C routines or vice versa.
   --  In Ada created forms we use the low level user pointer to maintain
   --  binding internal additional informations about the form. This
   --  internal information contains a hook for the Ada provided user pointer.
   --  Unless you understand this implementation, the safest way in mixed
   --  language programs to deal with user pointers is, that only the language
   --  that created the form should also manipulate the user pointer for that
   --  form.
   procedure Redefine (Frm  : in Form;
                       Flds : in Field_Array)
   is
      function Set_Frm_Fields (Frm   : Form;
                               Items : Field_Array) return C_Int;
      pragma Import (C, Set_Frm_Fields, "set_form_fields");

      A   : constant Form_User_Wrapper_Access := Form_Userptr (Frm);
      I   : Field_Array_Access;
      Res : Eti_Error;
   begin
      if A = null or else A.I = null then raise Form_Exception;
      else
         I := new Field_Array (1 .. (Flds'Length + 1));
         I.all (1 .. Flds'Length) := Flds (Flds'First .. Flds'Last);
         I.all (Flds'Length + 1) := Null_Field;
         Res := Set_Frm_Fields (Frm, I.all);
         if  Res /= E_Ok then
            Free_Allocated_Fields (I);
            Eti_Exception (Res);
         else
            Free_Allocated_Fields (A.I);
            A.I := I;
         end if;
      end if;
   end Redefine;
   --  |
   --  |
   --  |
   function Fields (Frm : Form) return Field_Array_Access
   is
      A : constant Form_User_Wrapper_Access := Form_Userptr (Frm);
   begin
      if A = null or else A.I = null then
         raise Form_Exception;
      else
         return A.I;
      end if;
   end Fields;
   --  |
   --  |
   --  |
   function Field_Count (Frm : Form) return Natural
   is
      function Count (Frm : Form) return C_Int;
      pragma Import (C, Count, "field_count");
   begin
      return Natural (Count (Frm));
   end Field_Count;
   --  |
   --  |
   --  |
   procedure Move (Fld    : in Field;
                   Line   : in Line_Position;
                   Column : in Column_Position)
   is
      function Move (Fld : Field; L, C : C_Int) return C_Int;
      pragma Import (C, Move, "move_field");

      Res : constant Eti_Error := Move (Fld, C_Int (Line), C_Int (Column));
   begin
      if Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Move;
   --  |
   --  |=====================================================================
   --  | man page form_new.3x
   --  |=====================================================================
   --  |
   --  |
   --  |
   function Create (Fields : Field_Array) return Form
   is
      function NewForm (Fields : Field_Array) return Form;
      pragma Import (C, NewForm, "new_form");

      M   : Form;
      I   : Field_Array_Access;
      U   : Form_User_Wrapper_Access;
      Res : Eti_Error;
   begin
      I := new Field_Array (1 .. (Fields'Length + 1));
      I.all (1 .. Fields'Length) := Fields (Fields'First .. Fields'Last);
      I.all (Fields'Length + 1) := Null_Field;
      M := NewForm (I.all);
      if M = Null_Form then
         Free_Allocated_Fields (I);
         raise Form_Exception;
      end if;
      U := new Form_User_Wrapper'(U => System.Null_Address, I => I);
      Res := Set_Form_Userptr (M, U);
      if  Res /= E_Ok then
         Free_Allocated_Fields (I);
         Free_Form_User_Wrapper (U);
         Eti_Exception (Res);
      end if;
      return M;
   end Create;
   --  |
   --  |
   --  |
   procedure Delete (Frm : in out Form)
   is
      function Free (Frm : Form) return C_Int;
      pragma Import (C, Free, "free_form");

      U   : Form_User_Wrapper_Access := Form_Userptr (Frm);
      Res : constant Eti_Error := Free (Frm);
   begin
      if Res /= E_Ok then
         Eti_Exception (Res);
      end if;
      if U = null or else U.I = null then
         raise Form_Exception;
      end if;
      Free_Allocated_Fields (U.I);
      Free_Form_User_Wrapper (U);
      Frm := Null_Form;
   end Delete;
   --  |
   --  |=====================================================================
   --  | man page form_opts.3x
   --  |=====================================================================
   --  |
   --  |
   --  |
   procedure Normalize_Form_Options (Options : in out C_Int);
   pragma Import (C, Normalize_Form_Options, "_nc_ada_normalize_form_opts");

   procedure Set_Options (Frm     : in Form;
                          Options : in Form_Option_Set)
   is
      function Set_Form_Opts (Frm : Form;
                              Opt : C_Int) return C_Int;
      pragma Import (C, Set_Form_Opts, "set_form_opts");

      Opt : C_Int := FrmOS_2_CInt (Options);
      Res : Eti_Error;
   begin
      Normalize_Form_Options (Opt);
      Res := Set_Form_Opts (Frm, Opt);
      if  Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Options;
   --  |
   --  |
   --  |
   procedure Switch_Options (Frm     : in Form;
                             Options : in Form_Option_Set;
                             On      : Boolean := True)
   is
      function Form_Opts_On (Frm : Form;
                             Opt : C_Int) return C_Int;
      pragma Import (C, Form_Opts_On, "form_opts_on");
      function Form_Opts_Off (Frm : Form;
                              Opt : C_Int) return C_Int;
      pragma Import (C, Form_Opts_Off, "form_opts_off");

      Err : Eti_Error;
      Opt : C_Int := FrmOS_2_CInt (Options);
   begin
      Normalize_Form_Options (Opt);
      if On then
         Err := Form_Opts_On (Frm, Opt);
      else
         Err := Form_Opts_Off (Frm, Opt);
      end if;
      if Err /= E_Ok then
         Eti_Exception (Err);
      end if;
   end Switch_Options;
   --  |
   --  |
   --  |
   procedure Get_Options (Frm     : in  Form;
                          Options : out Form_Option_Set)
   is
      function Form_Opts (Frm : Form) return C_Int;
      pragma Import (C, Form_Opts, "form_opts");

      Res : C_Int := Form_Opts (Frm);
   begin
      Normalize_Form_Options (Res);
      Options := CInt_2_FrmOS (Res);
   end Get_Options;
   --  |
   --  |
   --  |
   function Get_Options (Frm : Form := Null_Form) return Form_Option_Set
   is
      Fos : Form_Option_Set;
   begin
      Get_Options (Frm, Fos);
      return Fos;
   end Get_Options;
   --  |
   --  |=====================================================================
   --  | man page form_post.3x
   --  |=====================================================================
   --  |
   --  |
   --  |
   procedure Post (Frm  : in Form;
                   Post : in Boolean := True)
   is
      function M_Post (Frm : Form) return C_Int;
      pragma Import (C, M_Post, "post_form");
      function M_Unpost (Frm : Form) return C_Int;
      pragma Import (C, M_Unpost, "unpost_form");

      Res : Eti_Error;
   begin
      if Post then
         Res := M_Post (Frm);
      else
         Res := M_Unpost (Frm);
      end if;
      if Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Post;
   --  |
   --  |=====================================================================
   --  | man page form_cursor.3x
   --  |=====================================================================
   --  |
   --  |
   --  |
   procedure Position_Cursor (Frm : Form)
   is
      function Pos_Form_Cursor (Frm : Form) return C_Int;
      pragma Import (C, Pos_Form_Cursor, "pos_form_cursor");

      Res : constant Eti_Error := Pos_Form_Cursor (Frm);
   begin
      if  Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Position_Cursor;
   --  |
   --  |=====================================================================
   --  | man page form_data.3x
   --  |=====================================================================
   --  |
   --  |
   --  |
   function Data_Ahead (Frm : Form) return Boolean
   is
      function Ahead (Frm : Form) return C_Int;
      pragma Import (C, Ahead, "data_ahead");

      Res : constant C_Int := Ahead (Frm);
   begin
      if Res = Curses_False then
         return False;
      else
         return True;
      end if;
   end Data_Ahead;
   --  |
   --  |
   --  |
   function Data_Behind (Frm : Form) return Boolean
   is
      function Behind (Frm : Form) return C_Int;
      pragma Import (C, Behind, "data_behind");

      Res : constant C_Int := Behind (Frm);
   begin
      if Res = Curses_False then
         return False;
      else
         return True;
      end if;
   end Data_Behind;
   --  |
   --  |=====================================================================
   --  | man page form_driver.3x
   --  |=====================================================================
   --  |
   --  |
   --  |
   function Driver (Frm : Form;
                    Key : Key_Code) return Driver_Result
   is
      function Frm_Driver (Frm : Form; Key : C_Int) return C_Int;
      pragma Import (C, Frm_Driver, "form_driver");

      R : Eti_Error := Frm_Driver (Frm, C_Int (Key));
   begin
      if R /= E_Ok then
         if R = E_Unknown_Command then
            return Unknown_Request;
         elsif R = E_Invalid_Field then
            return Invalid_Field;
         elsif R = E_Request_Denied then
            return Request_Denied;
         else
            Eti_Exception (R);
            return Form_Ok;
         end if;
      else
         return Form_Ok;
      end if;
   end Driver;
   --  |
   --  |=====================================================================
   --  | man page form_page.3x
   --  |=====================================================================
   --  |
   --  |
   --  |
   procedure Set_Current (Frm : in Form;
                          Fld : in Field)
   is
      function Set_Current_Fld (Frm : Form; Fld : Field) return C_Int;
      pragma Import (C, Set_Current_Fld, "set_current_field");

      Res : constant Eti_Error := Set_Current_Fld (Frm, Fld);
   begin
      if Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Current;
   --  |
   --  |
   --  |
   function Current (Frm : in Form) return Field
   is
      function Current_Fld (Frm : Form) return Field;
      pragma Import (C, Current_Fld, "current_field");

      Fld : constant Field := Current_Fld (Frm);
   begin
      if Fld = Null_Field then
         raise Form_Exception;
      end if;
      return Fld;
   end Current;
   --  |
   --  |
   --  |
   procedure Set_Page (Frm  : in Form;
                       Page : in Page_Number := Page_Number'First)
   is
      function Set_Frm_Page (Frm : Form; Pg : C_Int) return C_Int;
      pragma Import (C, Set_Frm_Page, "set_form_page");

      Res : constant Eti_Error := Set_Frm_Page (Frm, C_Int (Page));
   begin
      if Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Page;
   --  |
   --  |
   --  |
   function Page (Frm : Form) return Page_Number
   is
      function Get_Page (Frm : Form) return C_Int;
      pragma Import (C, Get_Page, "form_page");

      P : constant C_Int := Get_Page (Frm);
   begin
      if P < 0 then
         raise Form_Exception;
      else
         return Page_Number (P);
      end if;
   end Page;

   function Get_Index (Fld : Field) return Positive
   is
      function Get_Fieldindex (Fld : Field) return C_Int;
      pragma Import (C, Get_Fieldindex, "field_index");

      Res : constant C_Int := Get_Fieldindex (Fld);
   begin
      if Res = Curses_Err then
         raise Form_Exception;
      end if;
      return Positive (Natural (Res) + Positive'First);
   end Get_Index;

   --  |
   --  |=====================================================================
   --  | man page form_new_page.3x
   --  |=====================================================================
   --  |
   --  |
   --  |
   procedure Set_New_Page (Fld      : in Field;
                           New_Page : in Boolean := True)
   is
      function Set_Page (Fld : Field; Flg : C_Int) return C_Int;
      pragma Import (C, Set_Page, "set_new_page");

      Res : constant Eti_Error := Set_Page (Fld, Boolean'Pos (New_Page));
   begin
      if Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_New_Page;
   --  |
   --  |
   --  |
   function Is_New_Page (Fld : Field) return Boolean
   is
      function Is_New (Fld : Field) return C_Int;
      pragma Import (C, Is_New, "new_page");

      Res : constant C_Int := Is_New (Fld);
   begin
      if Res = Curses_False then
         return False;
      else
         return True;
      end if;
   end Is_New_Page;

------------------------------------------------------------------------------
   --  We use a GNAT internal hash table mechanism to create an association
   --  between an Ada_Defined_Field_Type and it's low level C_Field_Type
   --  peer.
   --  It shouldn´t be too complicated to reimplent this hashing mechanism
   --  for other compilers.
   --
   type Tag_Type_Pair;
   type Tag_Pair_Access is access all Tag_Type_Pair;
   pragma Controlled (Tag_Pair_Access);

   Null_Tag_Pair : constant Tag_Pair_Access := Tag_Pair_Access'(null);

   type Tag_Type_Pair is
      record
         Ada_Tag    : Tag;
         Cft        : C_Field_Type;
         Next       : Tag_Pair_Access;
      end record;

   type Htable_Headers is range 1 .. 31;
   procedure Free_Tag_Type_Pair is
     new Ada.Unchecked_Deallocation (Tag_Type_Pair, Tag_Pair_Access);

   procedure Set_Pair_Link (T : Tag_Pair_Access; Next : Tag_Pair_Access);
   function  Get_Pair_Link (T : Tag_Pair_Access) return Tag_Pair_Access;
   function  Get_Pair_Tag  (T : Tag_Pair_Access) return Tag;

   function Hash (T : Tag) return Htable_Headers;
   function Equal (A, B : Tag) return Boolean;

   package External_Pair_Htable is new GNAT.Htable.Static_Htable
     (Header_Num => Htable_Headers,
      Element    => Tag_Type_Pair,
      Elmt_Ptr   => Tag_Pair_Access,
      Null_Ptr   => Null_Tag_Pair,
      Set_Next   => Set_Pair_Link,
      Next       => Get_Pair_Link,
      Key        => Tag,
      Get_Key    => Get_Pair_Tag,
      Hash       => Hash,
      Equal      => Equal);

   procedure Set_Pair_Link (T : Tag_Pair_Access; Next : Tag_Pair_Access)
   is
   begin
      T.all.Next := Next;
   end Set_Pair_Link;

   function  Get_Pair_Link (T : Tag_Pair_Access) return Tag_Pair_Access
   is
   begin
      return T.all.Next;
   end Get_Pair_Link;

   function  Get_Pair_Tag  (T : Tag_Pair_Access) return Tag
   is
   begin
      return T.all.Ada_Tag;
   end Get_Pair_Tag;

   function Equal (A, B : Tag) return Boolean
   is
   begin
      return A = B;
   end Equal;

   function Hash (T : Tag) return Htable_Headers
   is
      function H is new GNAT.Htable.Hash (Htable_Headers);
   begin
      return H (External_Tag (T));
   end Hash;

   function Search_Type (T : Ada_Defined_Field_Type'Class)
                         return C_Field_Type
   is
      P : Tag_Pair_Access := External_Pair_Htable.Get (T'Tag);
   begin
      if P /= null then
         return P.Cft;
      else
         return Null_Field_Type;
      end if;
   end Search_Type;

   --  Register an Ada_Defined_Field_Type given by its Tag
   --  with it's associated C_Field_Type.
   procedure Register_Type (T   : in Ada_Defined_Field_Type'Class;
                            Cft : in C_Field_Type)
   is
      C : C_Field_Type := Search_Type (T);
      P : Tag_Pair_Access;
   begin
      if C /= Null_Field_Type then
         raise Form_Exception;
      else
         P := new Tag_Type_Pair'(T'Tag, Cft, null);
         External_Pair_Htable.Set (P);
      end if;
   end Register_Type;

   --  Unregister an Ada_Defined_Field_Type given by it's tag
   procedure Unregister_Type (T : in Ada_Defined_Field_Type'Class)
   is
      function Free_Fieldtype (Ft : C_Field_Type) return C_Int;
      pragma Import (C, Free_Fieldtype, "free_fieldtype");

      P   : Tag_Pair_Access := External_Pair_Htable.Get (T'Tag);
      Ft  : C_Field_Type;
      Res : C_Int;
   begin
      if P = null then
         raise Form_Exception;
      else
         Ft := P.Cft;
         External_Pair_Htable.Remove (T'Tag);
         Free_Tag_Type_Pair (P);
         Res := Free_Fieldtype (Ft);
         if Res /= E_Ok then
            Eti_Exception (Res);
         end if;
      end if;
   end Unregister_Type;

----------------------------------------------------------------------------
   --  |
   --  |
   --  |
   procedure Set_Type (Fld      : Field;
                       Fld_Type : Ada_Defined_Field_Type)
   is
      function Set_Fld_Type (F    : Field := Fld;
                             Ct   : C_Field_Type;
                             Arg1 : Ada_Defined_Field_Type'Class)
                             return C_Int;
      pragma Import (C, Set_Fld_Type, "set_field_type");
      function Field_Userptr (Fld : Field)
                              return Field_User_Wrapper_Access;
      pragma Import (C, Field_Userptr, "field_userptr");

      Res : Eti_Error;
      C   : constant C_Field_Type := Search_Type (Fld_Type);
   begin
      if C = Null_Field_Type then
         raise Form_Exception;
      else
         Res := Set_Fld_Type (Fld, C, Fld_Type);
         if Res /= E_Ok then
            Eti_Exception (Res);
         end if;
      end if;
   end Set_Type;
   --  |
   --  |
   --  |
   function Native_Type (Ftype : Ada_Defined_Field_Type)
                         return C_Field_Type
   is
      C : constant C_Field_Type := Search_Type (Ftype);
   begin
      if C = Null_Field_Type then
         raise Form_Exception;
      else
         return C;
      end if;
   end Native_Type;
   --  |
   --  |
   --  |
   function Native_Type (Ftype : Alpha_Field)
                         return C_Field_Type
   is
      C_Alpha_Field_Type : C_Field_Type;
      pragma Import (C, C_Alpha_Field_Type, "TYPE_ALPHA");
   begin
      return C_Alpha_Field_Type;
   end Native_Type;
   pragma Inline (Native_Type);
   --  |
   --  |
   --  |
   procedure Set_Type (Fld      : in Field;
                       Fld_Type : in Alpha_Field)
   is
      function Set_Fld_Type (F    : Field := Fld;
                             Cft  : C_Field_Type := Native_Type (Fld_Type);
                             Arg1 : C_Int) return C_Int;
      pragma Import (C, Set_Fld_Type, "set_field_type");
      function Field_Userptr (Fld : Field)
                              return Field_User_Wrapper_Access;
      pragma Import (C, Field_Userptr, "field_userptr");

      A   : constant Field_User_Wrapper_Access := Field_Userptr (Fld);
      Res : Eti_Error;
   begin
      Res := Set_Fld_Type (Arg1 => C_Int (Fld_Type.Minimum_Field_Width));
      if Res /= E_Ok then
         Eti_Exception (Res);
      else
         A.T := new Alpha_Field'(Fld_Type);
      end if;
   end Set_Type;
   --  |
   --  |
   --  |
   function Native_Type (Ftype : Alpha_Numeric_Field)
                         return C_Field_Type
   is
      C_Alpha_Numeric_Field_Type : C_Field_Type;
      pragma Import (C, C_Alpha_Numeric_Field_Type, "TYPE_ALNUM");
   begin
      return C_Alpha_Numeric_Field_Type;
   end Native_Type;
   pragma Inline (Native_Type);
   --  |
   --  |
   --  |
   procedure Set_Type (Fld      : in Field;
                       Fld_Type : in Alpha_Numeric_Field)
   is
      function Set_Fld_Type (F    : Field := Fld;
                             Cft  : C_Field_Type := Native_Type (Fld_Type);
                             Arg1 : C_Int) return C_Int;
      pragma Import (C, Set_Fld_Type, "set_field_type");
      function Field_Userptr (Fld : Field)
                              return Field_User_Wrapper_Access;
      pragma Import (C, Field_Userptr, "field_userptr");

      A   : constant Field_User_Wrapper_Access := Field_Userptr (Fld);
      Res : Eti_Error;
   begin
      Res := Set_Fld_Type (Arg1 => C_Int (Fld_Type.Minimum_Field_Width));
      if Res /= E_Ok then
         Eti_Exception (Res);
      else
         A.T := new Alpha_Numeric_Field'(Fld_Type);
      end if;
   end Set_Type;
   --  |
   --  |
   --  |
   function Native_Type (Ftype : Integer_Field)
                         return C_Field_Type
   is
      C_Integer_Field_Type : C_Field_Type;
      pragma Import (C, C_Integer_Field_Type, "TYPE_INTEGER");
   begin
      return C_Integer_Field_Type;
   end Native_Type;
   pragma Inline (Native_Type);
   --  |
   --  |
   --  |
   procedure Set_Type (Fld      : in Field;
                       Fld_Type : in Integer_Field)
   is
      function Set_Fld_Type (F    : Field := Fld;
                             Cft  : C_Field_Type := Native_Type (Fld_Type);
                             Arg1 : C_Int;
                             Arg2 : C_Long_Int;
                             Arg3 : C_Long_Int) return C_Int;
      pragma Import (C, Set_Fld_Type, "set_field_type");
      function Field_Userptr (Fld : Field)
                              return Field_User_Wrapper_Access;
      pragma Import (C, Field_Userptr, "field_userptr");

      A   : constant Field_User_Wrapper_Access := Field_Userptr (Fld);
      Res : Eti_Error;
   begin
      Res := Set_Fld_Type (Arg1 => C_Int (Fld_Type.Precision),
                           Arg2 => C_Long_Int (Fld_Type.Lower_Limit),
                           Arg3 => C_Long_Int (Fld_Type.Upper_Limit));
      if Res /= E_Ok then
         Eti_Exception (Res);
      else
         A.T := new Integer_Field'(Fld_Type);
      end if;
   end Set_Type;
   --  |
   --  |
   --  |
   function Native_Type (Ftype : Numeric_Field)
                         return C_Field_Type
   is
      C_Numeric_Field_Type : C_Field_Type;
      pragma Import (C, C_Numeric_Field_Type, "TYPE_NUMERIC");
   begin
      return C_Numeric_Field_Type;
   end Native_Type;
   pragma Inline (Native_Type);
   --  |
   --  |
   --  |
   procedure Set_Type (Fld      : in Field;
                       Fld_Type : in Numeric_Field)
   is
      type Double is new Interfaces.C.double;

   function Set_Fld_Type (F    : Field := Fld;
                          Cft  : C_Field_Type := Native_Type (Fld_Type);
                          Arg1 : Double;
                          Arg2 : Double;
                          Arg3 : Double) return C_Int;
      pragma Import (C, Set_Fld_Type, "set_field_type");
      function Field_Userptr (Fld : Field)
                              return Field_User_Wrapper_Access;
      pragma Import (C, Field_Userptr, "field_userptr");

      A   : constant Field_User_Wrapper_Access := Field_Userptr (Fld);
      Res : Eti_Error;
   begin
      Res := Set_Fld_Type (Arg1 => Double (Fld_Type.Precision),
                           Arg2 => Double (Fld_Type.Lower_Limit),
                           Arg3 => Double (Fld_Type.Upper_Limit));
      if Res /= E_Ok then
         Eti_Exception (Res);
      else
         A.T := new Numeric_Field'(Fld_Type);
      end if;
   end Set_Type;
   --  |
   --  |
   --  |
   function Native_Type (Ftype : Regular_Expression_Field)
                         return C_Field_Type
   is
      C_Regexp_Field_Type : C_Field_Type;
      pragma Import (C, C_Regexp_Field_Type, "TYPE_REGEXP");
   begin
      return C_Regexp_Field_Type;
   end Native_Type;
   pragma Inline (Native_Type);
   --  |
   --  |
   --  |
   procedure Set_Type (Fld      : in Field;
                       Fld_Type : in Regular_Expression_Field)
   is
      type Char_Ptr is access all Interfaces.C.Char;
      function Set_Fld_Type (F    : Field := Fld;
                             Cft  : C_Field_Type := Native_Type (Fld_Type);
                             Arg1 : Char_Ptr) return C_Int;
      pragma Import (C, Set_Fld_Type, "set_field_type");
      function Field_Userptr (Fld : Field)
                              return Field_User_Wrapper_Access;
      pragma Import (C, Field_Userptr, "field_userptr");

      A   : constant Field_User_Wrapper_Access := Field_Userptr (Fld);
      Txt : char_array (0 .. Fld_Type.Regular_Expression.all'Length);
      Len : size_t;
      Res : Eti_Error;
   begin
      To_C (Fld_Type.Regular_Expression.all, Txt, Len);
      Res := Set_Fld_Type (Arg1 => Txt (Txt'First)'Access);
      if Res /= E_Ok then
         Eti_Exception (Res);
      else
         A.T := new Regular_Expression_Field'(Fld_Type);
      end if;
   end Set_Type;
   --  |
   --  |
   --  |
   function Native_Type (Ftype : Enumeration_Field)
                         return C_Field_Type
   is
      C_Enum_Type : C_Field_Type;
      pragma Import (C, C_Enum_Type, "TYPE_ENUM");
   begin
      return C_Enum_Type;
   end Native_Type;
   pragma Inline (Native_Type);
   --  |
   --  |
   --  |
   function Create (Info               : Enumeration_Info;
                    Auto_Release_Names : Boolean := False)
     return Enumeration_Field
   is
      procedure Release_String is
        new Ada.Unchecked_Deallocation (String,
                                        String_Access);
      E : Enumeration_Field;
      L : constant size_t := 1 + size_t (Info.C);
      S : String_Access;
   begin
      E.Case_Sensitive       := Info.Case_Sensitive;
      E.Match_Must_Be_Unique := Info.Match_Must_Be_Unique;
      E.Arr := new chars_ptr_array (size_t (1) .. L);
      for I in 1 .. Positive (L - 1) loop
         if Info.Names (I) = null then
            raise Form_Exception;
         end if;
         E.Arr (size_t (I)) := New_String (Info.Names (I).all);
         if Auto_Release_Names then
            S := Info.Names (I);
            Release_String (S);
         end if;
      end loop;
      E.Arr (L) := Null_Ptr;
      return E;
   end Create;

   procedure Release (Enum : in out Enumeration_Field)
   is
      I : size_t := 0;
      P : chars_ptr;
   begin
      loop
         P := Enum.Arr (I);
         exit when P = Null_Ptr;
         Free (P);
         Enum.Arr (I) := Null_Ptr;
         I := I + 1;
      end loop;
      Enum.Arr := null;
   end Release;

   procedure Set_Type (Fld      : in Field;
                       Fld_Type : in Enumeration_Field)
   is
      function Set_Fld_Type (F    : Field := Fld;
                             Cft  : C_Field_Type := Native_Type (Fld_Type);
                             Arg1 : chars_ptr_array;
                             Arg2 : C_Int;  -- case
                             Arg3 : C_Int) return C_Int;
      pragma Import (C, Set_Fld_Type, "set_field_type");
      function Field_Userptr (Fld : Field)
                              return Field_User_Wrapper_Access;
      pragma Import (C, Field_Userptr, "field_userptr");

      A   : constant Field_User_Wrapper_Access := Field_Userptr (Fld);
      Res : Eti_Error;
   begin
      if Fld_Type.Arr = null then
         raise Form_Exception;
      end if;
      Res := Set_Fld_Type (Arg1 => Fld_Type.Arr.all,
                           Arg2 => C_Int (Boolean'Pos
                                          (Fld_Type.Case_Sensitive)),
                           Arg3 =>
                             C_Int (Boolean'Pos
                                    (Fld_Type.Match_Must_Be_Unique)));
      if Res /= E_Ok then
         Eti_Exception (Res);
      else
         A.T := new Enumeration_Field'(Fld_Type);
      end if;
   end Set_Type;


   function Native_Type (Ftype : Internet_V4_Address_Field)
                         return C_Field_Type
   is
      C_IPV4_Field_Type : C_Field_Type;
      pragma Import (C, C_IPV4_Field_Type, "TYPE_IPV4");
   begin
      return C_IPV4_Field_Type;
   end Native_Type;
   pragma Inline (Native_Type);
   --  |
   --  |
   --  |
   procedure Set_Type (Fld      : in Field;
                       Fld_Type : in Internet_V4_Address_Field)
   is
      function Set_Fld_Type (F    : Field := Fld;
                             Cft  : C_Field_Type := Native_Type (Fld_Type))
                             return C_Int;
      pragma Import (C, Set_Fld_Type, "set_field_type");
      function Field_Userptr (Fld : Field)
                              return Field_User_Wrapper_Access;
      pragma Import (C, Field_Userptr, "field_userptr");

      A   : constant Field_User_Wrapper_Access := Field_Userptr (Fld);
      Res : Eti_Error;
   begin
      Res := Set_Fld_Type;
      if Res /= E_Ok then
         Eti_Exception (Res);
      else
         A.T := new Internet_V4_Address_Field'(Fld_Type);
      end if;
   end Set_Type;

   --  |
   --  |=====================================================================
   --  | man page form_field_validation.3x
   --  |=====================================================================
   --  |
   --  |
   --  |
   function Get_Type (Fld : in Field) return Field_Type_Access
   is
      A : constant Field_User_Wrapper_Access := Field_Userptr (Fld);
   begin
      if A = null then
         return null;
      else
         return A.T;
      end if;
   end Get_Type;

begin
   Default_Field_Options := Get_Options (Null_Field);
   Default_Form_Options  := Get_Options (Null_Form);
end Terminal_Interface.Curses.Forms;
