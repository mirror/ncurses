--  -*- ada -*-
define(`HTMLNAME',`terminal_interface-curses-forms_s.html')dnl
include(M4MACRO)dnl
------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                      Terminal_Interface.Curses.Form                      --
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
--  $Revision: 1.9 $
------------------------------------------------------------------------------
include(`Form_Base_Defs')
with System;
with Ada.Tags; use Ada.Tags;
with Ada.Characters.Latin_1;
with Interfaces.C;
with Interfaces.C.Strings;

package Terminal_Interface.Curses.Forms is

include(`Form_Linker_Options')

   Space : Character renames Ada.Characters.Latin_1.Space;

   type Field        is private;
   type Form         is private;
   type C_Field_Type is private;

   Null_Field        : constant Field;
   Null_Form         : constant Form;
   Null_Field_Type   : constant C_Field_Type;


   type Field_Justification is (None,
                                Left,
                                Center,
                                Right);

include(`Field_Rep')

   Default_Field_Options : Field_Option_Set;
   --  The initial defaults for the field options.

include(`Form_Opt_Rep')

   Default_Form_Options : Form_Option_Set;
   --  The initial defaults for the form options.

   type Buffer_Number is new Natural;

   type Field_Array is array (Positive range <>) of aliased Field;
   pragma Convention (C, Field_Array);

   type Field_Array_Access is access all Field_Array;

   subtype Form_Request_Code is Key_Code range (Key_Max + 1) .. (Key_Max + 57);

   --  The prefix F_ stands for "Form Request"
   F_Next_Page                : constant Form_Request_Code := Key_Max + 1;
   F_Previous_Page            : constant Form_Request_Code := Key_Max + 2;
   F_First_Page               : constant Form_Request_Code := Key_Max + 3;
   F_Last_Page                : constant Form_Request_Code := Key_Max + 4;

   F_Next_Field               : constant Form_Request_Code := Key_Max + 5;
   F_Previous_Field           : constant Form_Request_Code := Key_Max + 6;
   F_First_Field              : constant Form_Request_Code := Key_Max + 7;
   F_Last_Field               : constant Form_Request_Code := Key_Max + 8;
   F_Sorted_Next_Field        : constant Form_Request_Code := Key_Max + 9;
   F_Sorted_Previous_Field    : constant Form_Request_Code := Key_Max + 10;
   F_Sorted_First_Field       : constant Form_Request_Code := Key_Max + 11;
   F_Sorted_Last_Field        : constant Form_Request_Code := Key_Max + 12;
   F_Left_Field               : constant Form_Request_Code := Key_Max + 13;
   F_Right_Field              : constant Form_Request_Code := Key_Max + 14;
   F_Up_Field                 : constant Form_Request_Code := Key_Max + 15;
   F_Down_Field               : constant Form_Request_Code := Key_Max + 16;

   F_Next_Char                : constant Form_Request_Code := Key_Max + 17;
   F_Previous_Char            : constant Form_Request_Code := Key_Max + 18;
   F_Next_Line                : constant Form_Request_Code := Key_Max + 19;
   F_Previous_Line            : constant Form_Request_Code := Key_Max + 20;
   F_Next_Word                : constant Form_Request_Code := Key_Max + 21;
   F_Previous_Word            : constant Form_Request_Code := Key_Max + 22;
   F_Begin_Field              : constant Form_Request_Code := Key_Max + 23;
   F_End_Field                : constant Form_Request_Code := Key_Max + 24;
   F_Begin_Line               : constant Form_Request_Code := Key_Max + 25;
   F_End_Line                 : constant Form_Request_Code := Key_Max + 26;
   F_Left_Char                : constant Form_Request_Code := Key_Max + 27;
   F_Right_Char               : constant Form_Request_Code := Key_Max + 28;
   F_Up_Char                  : constant Form_Request_Code := Key_Max + 29;
   F_Down_Char                : constant Form_Request_Code := Key_Max + 30;

   F_New_Line                 : constant Form_Request_Code := Key_Max + 31;
   F_Insert_Char              : constant Form_Request_Code := Key_Max + 32;
   F_Insert_Line              : constant Form_Request_Code := Key_Max + 33;
   F_Delete_Char              : constant Form_Request_Code := Key_Max + 34;
   F_Delete_Previous          : constant Form_Request_Code := Key_Max + 35;
   F_Delete_Line              : constant Form_Request_Code := Key_Max + 36;
   F_Delete_Word              : constant Form_Request_Code := Key_Max + 37;
   F_Clear_EOL                : constant Form_Request_Code := Key_Max + 38;
   F_Clear_EOF                : constant Form_Request_Code := Key_Max + 39;
   F_Clear_Field              : constant Form_Request_Code := Key_Max + 40;
   F_Overlay_Mode             : constant Form_Request_Code := Key_Max + 41;
   F_Insert_Mode              : constant Form_Request_Code := Key_Max + 42;

   --  Vertical Scrolling
   F_ScrollForward_Line       : constant Form_Request_Code := Key_Max + 43;
   F_ScrollBackward_Line      : constant Form_Request_Code := Key_Max + 44;
   F_ScrollForward_Page       : constant Form_Request_Code := Key_Max + 45;
   F_ScrollBackward_Page      : constant Form_Request_Code := Key_Max + 46;
   F_ScrollForward_HalfPage   : constant Form_Request_Code := Key_Max + 47;
   F_ScrollBackward_HalfPage  : constant Form_Request_Code := Key_Max + 48;

   --  Horizontal Scrolling
   F_HScrollForward_Char      : constant Form_Request_Code := Key_Max + 49;
   F_HScrollBackward_Char     : constant Form_Request_Code := Key_Max + 50;
   F_HScrollForward_Line      : constant Form_Request_Code := Key_Max + 51;
   F_HScrollBackward_Line     : constant Form_Request_Code := Key_Max + 52;
   F_HScrollForward_HalfLine  : constant Form_Request_Code := Key_Max + 53;
   F_HScrollBackward_HalfLine : constant Form_Request_Code := Key_Max + 54;

   F_Validate_Field           : constant Form_Request_Code := Key_Max + 55;
   F_Next_Choice              : constant Form_Request_Code := Key_Max + 56;
   F_Previous_Choice          : constant Form_Request_Code := Key_Max + 57;

   --  For those who like the old 'C' style request names
   REQ_NEXT_PAGE    : Form_Request_Code renames F_Next_Page;
   REQ_PREV_PAGE    : Form_Request_Code renames F_Previous_Page;
   REQ_FIRST_PAGE   : Form_Request_Code renames F_First_Page;
   REQ_LAST_PAGE    : Form_Request_Code renames F_Last_Page;

   REQ_NEXT_FIELD   : Form_Request_Code renames F_Next_Field;
   REQ_PREV_FIELD   : Form_Request_Code renames F_Previous_Field;
   REQ_FIRST_FIELD  : Form_Request_Code renames F_First_Field;
   REQ_LAST_FIELD   : Form_Request_Code renames F_Last_Field;
   REQ_SNEXT_FIELD  : Form_Request_Code renames F_Sorted_Next_Field;
   REQ_SPREV_FIELD  : Form_Request_Code renames F_Sorted_Previous_Field;
   REQ_SFIRST_FIELD : Form_Request_Code renames F_Sorted_First_Field;
   REQ_SLAST_FIELD  : Form_Request_Code renames F_Sorted_Last_Field;
   REQ_LEFT_FIELD   : Form_Request_Code renames F_Left_Field;
   REQ_RIGHT_FIELD  : Form_Request_Code renames F_Right_Field;
   REQ_UP_FIELD     : Form_Request_Code renames F_Up_Field;
   REQ_DOWN_FIELD   : Form_Request_Code renames F_Down_Field;

   REQ_NEXT_CHAR    : Form_Request_Code renames F_Next_Char;
   REQ_PREV_CHAR    : Form_Request_Code renames F_Previous_Char;
   REQ_NEXT_LINE    : Form_Request_Code renames F_Next_Line;
   REQ_PREV_LINE    : Form_Request_Code renames F_Previous_Line;
   REQ_NEXT_WORD    : Form_Request_Code renames F_Next_Word;
   REQ_PREV_WORD    : Form_Request_Code renames F_Previous_Word;
   REQ_BEG_FIELD    : Form_Request_Code renames F_Begin_Field;
   REQ_END_FIELD    : Form_Request_Code renames F_End_Field;
   REQ_BEG_LINE     : Form_Request_Code renames F_Begin_Line;
   REQ_END_LINE     : Form_Request_Code renames F_End_Line;
   REQ_LEFT_CHAR    : Form_Request_Code renames F_Left_Char;
   REQ_RIGHT_CHAR   : Form_Request_Code renames F_Right_Char;
   REQ_UP_CHAR      : Form_Request_Code renames F_Up_Char;
   REQ_DOWN_CHAR    : Form_Request_Code renames F_Down_Char;

   REQ_NEW_LINE     : Form_Request_Code renames F_New_Line;
   REQ_INS_CHAR     : Form_Request_Code renames F_Insert_Char;
   REQ_INS_LINE     : Form_Request_Code renames F_Insert_Line;
   REQ_DEL_CHAR     : Form_Request_Code renames F_Delete_Char;
   REQ_DEL_PREV     : Form_Request_Code renames F_Delete_Previous;
   REQ_DEL_LINE     : Form_Request_Code renames F_Delete_Line;
   REQ_DEL_WORD     : Form_Request_Code renames F_Delete_Word;
   REQ_CLR_EOL      : Form_Request_Code renames F_Clear_EOL;
   REQ_CLR_EOF      : Form_Request_Code renames F_Clear_EOF;
   REQ_CLR_FIELD    : Form_Request_Code renames F_Clear_Field;
   REQ_OVL_MODE     : Form_Request_Code renames F_Overlay_Mode;
   REQ_INS_MODE     : Form_Request_Code renames F_Insert_Mode;

   REQ_SCR_FLINE    : Form_Request_Code renames F_ScrollForward_Line;
   REQ_SCR_BLINE    : Form_Request_Code renames F_ScrollBackward_Line;
   REQ_SCR_FPAGE    : Form_Request_Code renames F_ScrollForward_Page;
   REQ_SCR_BPAGE    : Form_Request_Code renames F_ScrollBackward_Page;
   REQ_SCR_FHPAGE   : Form_Request_Code renames F_ScrollForward_HalfPage;
   REQ_SCR_BHPAGE   : Form_Request_Code renames F_ScrollBackward_HalfPage;

   REQ_SCR_FCHAR    : Form_Request_Code renames F_HScrollForward_Char;
   REQ_SCR_BCHAR    : Form_Request_Code renames F_HScrollBackward_Char;
   REQ_SCR_HFLINE   : Form_Request_Code renames F_HScrollForward_Line;
   REQ_SCR_HBLINE   : Form_Request_Code renames F_HScrollBackward_Line;
   REQ_SCR_HFHALF   : Form_Request_Code renames F_HScrollForward_HalfLine;
   REQ_SCR_HBHALF   : Form_Request_Code renames F_HScrollBackward_HalfLine;

   REQ_VALIDATION   : Form_Request_Code renames F_Validate_Field;
   REQ_NEXT_CHOICE  : Form_Request_Code renames F_Next_Choice;
   REQ_PREV_CHOICE  : Form_Request_Code renames F_Previous_Choice;


   procedure Request_Name (Key  : in Form_Request_Code;
                           Name : out String);

   ------------------
   --  Exceptions  --
   ------------------
   Form_Exception : exception;

   --  MANPAGE(`form_field_new.3x')

   --  ANCHOR(`new_field()',`Create')
   function Create (Height       : Line_Count;
                    Width        : Column_Count;
                    Top          : Line_Position;
                    Left         : Column_Position;
                    Off_Screen   : Natural := 0;
                    More_Buffers : Buffer_Number := Buffer_Number'First)
                    return Field;
   --  AKA

   --  ANCHOR(`new_field()',`New_Field')
   function New_Field (Height       : Line_Count;
                       Width        : Column_Count;
                       Top          : Line_Position;
                       Left         : Column_Position;
                       Off_Screen   : Natural := 0;
                       More_Buffers : Buffer_Number := Buffer_Number'First)
                       return Field renames Create;
   --  AKA

   --  ANCHOR(`free_field()',`Delete')
   procedure Delete (Fld : in out Field);
   --  AKA
   --  Reset Fld to Null_Field

   --  ANCHOR(`dup_field()',`Duplicate')
   function Duplicate (Fld  : Field;
                       Top  : Line_Position;
                       Left : Column_Position) return Field;
   --  AKA

   --  ANCHOR(`link_field()',`Link')
   function Link (Fld  : Field;
                  Top  : Line_Position;
                  Left : Column_Position) return Field;
   --  AKA

   --  MANPAGE(`form_field_just.3x')

   --  ANCHOR(`set_field_just()',`Set_Justification')
   procedure Set_Justification (Fld  : in Field;
                                Just : in Field_Justification := None);
   --  AKA

   --  ANCHOR(`field_just()',`Get_Justification')
   function Get_Justification (Fld : Field) return Field_Justification;
   --  AKA

   --  MANPAGE(`form_field_buffer.3x')

   --  ANCHOR(`set_field_buffer()',`Set_Buffer')
   procedure Set_Buffer
     (Fld    : in Field;
      Buffer : in Buffer_Number := Buffer_Number'First;
      Str    : in String);
   --  AKA

   --  ANCHOR(`field_buffer()',`Get_Buffer')
   procedure Get_Buffer
     (Fld    : in Field;
      Buffer : in Buffer_Number := Buffer_Number'First;
      Str    : out String);
   --  AKA

   --  ANCHOR(`set_field_status()',`Set_Status')
   procedure Set_Status (Fld    : in Field;
                         Status : in Boolean := True);
   --  AKA

   --  ANCHOR(`field_status()',`Changed')
   function Changed (Fld : Field) return Boolean;
   --  AKA

   --  ANCHOR(`set_field_max()',`Set_Maximum_Size')
   procedure Set_Maximum_Size (Fld : in Field;
                               Max : in Natural := 0);
   --  AKA

   --  MANPAGE(`form_field_opts.3x')

   --  ANCHOR(`set_field_opts()',`Set_Options')
   procedure Set_Options (Fld     : in Field;
                          Options : in Field_Option_Set);
   --  AKA

   --  ANCHOR(`field_opts_on()',`Switch_Options')
   procedure Switch_Options (Fld     : in Field;
                             Options : in Field_Option_Set;
                             On      : Boolean := True);
   --  AKA
   --  ALIAS(`field_opts_off()')

   --  ANCHOR(`field_opts()',`Get_Options')
   procedure Get_Options (Fld     : in  Field;
                          Options : out Field_Option_Set);
   --  AKA

   --  ANCHOR(`field_opts()',`Get_Options')
   function Get_Options (Fld : Field := Null_Field)
                         return Field_Option_Set;
   --  AKA

   --  MANPAGE(`form_field_attributes.3x')

   --  ANCHOR(`set_field_fore()',`Set_Foreground')
   procedure Set_Foreground
     (Fld   : in Field;
      Fore  : in Character_Attribute_Set := Normal_Video;
      Color : in Color_Pair := Color_Pair'First);
   --  AKA

   --  ANCHOR(`field_fore()',`Foreground')
   procedure Foreground (Fld  : in  Field;
                         Fore : out Character_Attribute_Set);
   --  AKA

   --  ANCHOR(`field_fore()',`Foreground')
   procedure Foreground (Fld   : in  Field;
                         Fore  : out Character_Attribute_Set;
                         Color : out Color_Pair);
   --  AKA

   --  ANCHOR(`set_field_back()',`Set_Background')
   procedure Set_Background
     (Fld   : in Field;
      Back  : in Character_Attribute_Set := Normal_Video;
      Color : in Color_Pair := Color_Pair'First);
   --  AKA

   --  ANCHOR(`field_back()',`Background')
   procedure Background (Fld  : in  Field;
                         Back : out Character_Attribute_Set);
   --  AKA

   --  ANCHOR(`field_back()',`Background')
   procedure Background (Fld   : in  Field;
                         Back  : out Character_Attribute_Set;
                         Color : out Color_Pair);
   --  AKA

   --  ANCHOR(`set_field_pad()',`Set_Pad_Character')
   procedure Set_Pad_Character (Fld : in Field;
                                Pad : in Character := Space);
   --  AKA

   --  ANCHOR(`field_pad()',`Pad_Character')
   procedure Pad_Character (Fld : in  Field;
                            Pad : out Character);
   --  AKA

   --  MANPAGE(`form_field_info.3x')

   --  ANCHOR(`field_info()',`Info')
   procedure Info (Fld                : in  Field;
                   Lines              : out Line_Count;
                   Columns            : out Column_Count;
                   First_Row          : out Line_Position;
                   First_Column       : out Column_Position;
                   Off_Screen         : out Natural;
                   Additional_Buffers : out Buffer_Number);
   --  AKA

   --  ANCHOR(`dynamic_field_info()',`Dynamic_Info')
   procedure Dynamic_Info (Fld     : in Field;
                           Lines   : out Line_Count;
                           Columns : out Column_Count;
                           Max     : out Natural);
   --  AKA

   --  MANPAGE(`form_win.3x')

   --  ANCHOR(`set_form_win()',`Set_Window')
   procedure Set_Window (Frm : in Form;
                         Win : in Window);
   --  AKA

   --  ANCHOR(`form_win()',`Get_Window')
   function Get_Window (Frm : Form) return Window;
   --  AKA

   --  ANCHOR(`set_form_sub()',`Set_Sub_Window')
   procedure Set_Sub_Window (Frm : in Form;
                             Win : in Window);
   --  AKA

   --  ANCHOR(`form_sub()',`Get_Sub_Window')
   function Get_Sub_Window (Frm : Form) return Window;
   --  AKA

   --  ANCHOR(`scale_form()',`Scale')
   procedure Scale (Frm     : in Form;
                    Lines   : out Line_Count;
                    Columns : out Column_Count);
   --  AKA

   --  MANPAGE(`form_hook.3x')

   type Form_Hook_Function is access procedure (Frm : in Form);
   pragma Convention (C, Form_Hook_Function);

   --  ANCHOR(`set_field_init()',`Set_Field_Init_Hook')
   procedure Set_Field_Init_Hook (Frm  : in Form;
                                  Proc : in Form_Hook_Function);
   --  AKA

   --  ANCHOR(`set_field_term()',`Set_Field_Term_Hook')
   procedure Set_Field_Term_Hook (Frm  : in Form;
                                  Proc : in Form_Hook_Function);
   --  AKA

   --  ANCHOR(`set_form_init()',`Set_Form_Init_Hook')
   procedure Set_Form_Init_Hook (Frm  : in Form;
                                 Proc : in Form_Hook_Function);
   --  AKA

   --  ANCHOR(`set_form_term()',`Set_Form_Term_Hook')
   procedure Set_Form_Term_Hook (Frm  : in Form;
                                 Proc : in Form_Hook_Function);
   --  AKA

   --  ANCHOR(`field_init()',`Get_Field_Init_Hook')
   function Get_Field_Init_Hook (Frm : Form) return Form_Hook_Function;
   --  AKA
   pragma Import (C, Get_Field_Init_Hook, "field_init");

   --  ANCHOR(`field_term()',`Get_Field_Term_Hook')
   function Get_Field_Term_Hook (Frm : Form) return Form_Hook_Function;
   --  AKA
   pragma Import (C, Get_Field_Term_Hook, "field_term");

   --  ANCHOR(`form_init()',`Get_Form_Init_Hook')
   function Get_Form_Init_Hook (Frm : Form) return Form_Hook_Function;
   --  AKA
   pragma Import (C, Get_Form_Init_Hook, "form_init");

   --  ANCHOR(`form_term()',`Get_Form_Term_Hook')
   function Get_Form_Term_Hook (Frm : Form) return Form_Hook_Function;
   --  AKA
   pragma Import (C, Get_Form_Term_Hook, "form_term");

   --  MANPAGE(`form_field.3x')

   --  ANCHOR(`set_form_fields()',`Redefine')
   procedure Redefine (Frm  : in Form;
                       Flds : in Field_Array);
   --  AKA
   --  With a bit more comfort. You don´t need to terminate the Field_Array
   --  with a null entry. This is handled internally in the binding.

   --  ANCHOR(`set_form_fields()',`Set_Fields')
   procedure Set_Fields (Frm  : in Form;
                         Flds : in Field_Array) renames Redefine;
   --  AKA

   --  ANCHOR(`form_fields()',`Fields')
   function Fields (Frm : Form) return Field_Array_Access;
   --  AKA

   --  ANCHOR(`field_count()',`Field_Count')
   function Field_Count (Frm : Form) return Natural;
   --  AKA

   --  ANCHOR(`move_field()',`Move')
   procedure Move (Fld    : in Field;
                   Line   : in Line_Position;
                   Column : in Column_Position);
   --  AKA

   --  MANPAGE(`form_new.3x')

   --  ANCHOR(`new_form()',`Create')
   function Create (Fields : Field_Array) return Form;
   --  AKA

   --  ANCHOR(`new_form()',`New_Form')
   function New_Form (Fields : Field_Array) return Form renames Create;
   --  AKA

   --  ANCHOR(`free_form()',`Delete')
   procedure Delete (Frm : in out Form);
   --  AKA
   --  Reset Frm to Null_Form

   --  MANPAGE(`form_opts.3x')

   --  ANCHOR(`set_form_opts()',`Set_Options')
   procedure Set_Options (Frm     : in Form;
                          Options : in Form_Option_Set);
   --  AKA

   --  ANCHOR(`form_opts_on()',`Switch_Options')
   procedure Switch_Options (Frm     : in Form;
                             Options : in Form_Option_Set;
                             On      : Boolean := True);
   --  AKA
   --  ALIAS(`form_opts_off()')

   --  ANCHOR(`form_opts()',`Get_Options')
   procedure Get_Options (Frm     : in  Form;
                          Options : out Form_Option_Set);
   --  AKA

   --  ANCHOR(`form_opts()',`Get_Options')
   function Get_Options (Frm : Form := Null_Form) return Form_Option_Set;
   --  AKA

   --  MANPAGE(`form_post.3x')

   --  ANCHOR(`post_form()',`Post')
   procedure Post (Frm  : in Form;
                   Post : in Boolean := True);
   --  AKA
   --  ALIAS(`unpost_form()')

   --  MANPAGE(`form_cursor.3x')

   --  ANCHOR(`pos_form_cursor()',`Position_Cursor')
   procedure Position_Cursor (Frm : Form);
   --  AKA

   --  MANPAGE(`form_data.3x')

   --  ANCHOR(`data_ahead()',`Data_Ahead')
   function Data_Ahead (Frm : Form) return Boolean;
   --  AKA

   --  ANCHOR(`data_behind()',`Data_Behind')
   function Data_Behind (Frm : Form) return Boolean;
   --  AKA

   --  MANPAGE(`form_driver.3x')

   type Driver_Result is (Form_Ok,
                          Request_Denied,
                          Unknown_Request,
                          Invalid_Field);

   --  ANCHOR(`form_driver()',`Driver')
   function Driver (Frm : Form;
                    Key : Key_Code) return Driver_Result;
   --  AKA

   --  MANPAGE(`form_page.3x')

   type Page_Number is new Natural;

   --  ANCHOR(`set_current_field()',`Set_Current')
   procedure Set_Current (Frm : in Form;
                          Fld : in Field);
   --  AKA

   --  ANCHOR(`current_field()',`Current')
   function Current (Frm : in Form) return Field;
   --  AKA

   --  ANCHOR(`set_form_page()',`Set_Page')
   procedure Set_Page (Frm  : in Form;
                       Page : in Page_Number := Page_Number'First);
   --  AKA

   --  ANCHOR(`form_page()',`Page')
   function Page (Frm : Form) return Page_Number;
   --  AKA

   --  ANCHOR(`field_index()',`Get_Index')
   function Get_Index (Fld : Field) return Positive;
   --  AKA
   --  Please note that in this binding we start the numbering of fields
   --  with 1. So this is number is one more than you get from the low
   --  level call.

   --  MANPAGE(`form_new_page.3x')

   --  ANCHOR(`set_new_page()',`Set_New_Page')
   procedure Set_New_Page (Fld      : in Field;
                           New_Page : in Boolean := True);
   --  AKA

   --  ANCHOR(`new_page()',`Is_New_Page')
   function Is_New_Page (Fld : Field) return Boolean;
   --  AKA

   --  MANPAGE(`form_fieldtype.3x')

   type Field_Type is abstract tagged null record;
   type Field_Type_Access is access all Field_Type'Class;

   function Native_Type (Ftype : Field_Type)
                         return C_Field_Type is abstract;
   --  This function returns the C libraries handle to the field type.
   --  May be you need this if you want to interface to lower level
   --  routines in the form library.

   --  ANCHOR(`set_field_type()',`Set_Type')
   procedure Set_Type (Fld      : in Field;
                       Fld_Type : in Field_Type) is abstract;
   --  AKA
   --  But: we hide the vararg mechanism of the C interface. You always
   --       have to pass a single Field_Type parameter.

   type C_Defined_Field_Type is abstract new Field_Type with null record;
   --  This is the root of all field typed defined in C, i.e. this are
   --  the predefined field types in the form library.

   type Alpha_Field is new C_Defined_Field_Type
      with record
         Minimum_Field_Width : Natural := 0;
      end record;
   procedure Set_Type (Fld      : in Field;
                       Fld_Type : in Alpha_Field);
   function Native_Type (Ftype : Alpha_Field)
                         return C_Field_Type;

   type Alpha_Numeric_Field is new C_Defined_Field_Type with
      record
         Minimum_Field_Width : Natural := 0;
      end record;
   procedure Set_Type (Fld      : in Field;
                       Fld_Type : in Alpha_Numeric_Field);
   function Native_Type (Ftype : Alpha_Numeric_Field)
                         return C_Field_Type;

   type Integer_Field is new C_Defined_Field_Type with
      record
         Precision   : Natural;
         Lower_Limit : Integer;
         Upper_Limit : Integer;
      end record;
   procedure Set_Type (Fld      : in Field;
                       Fld_Type : in Integer_Field);
   function Native_Type (Ftype : Integer_Field)
                         return C_Field_Type;

   type Numeric_Field is new C_Defined_Field_Type with
      record
         Precision   : Natural;
         Lower_Limit : Float;
         Upper_Limit : Float;
      end record;
   procedure Set_Type (Fld      : in Field;
                       Fld_Type : in Numeric_Field);
   function Native_Type (Ftype : Numeric_Field)
                         return C_Field_Type;

   type String_Access is access String;

   type Regular_Expression_Field is new C_Defined_Field_Type with
      record
         Regular_Expression : String_Access;
      end record;
   procedure Set_Type (Fld      : in Field;
                       Fld_Type : in Regular_Expression_Field);
   function Native_Type (Ftype : Regular_Expression_Field)
                         return C_Field_Type;

   type Enum_Array is array (Positive range <>)
      of String_Access;

   type Enumeration_Info (C : Positive) is
      record
         Names                : Enum_Array (1 .. C);
         Case_Sensitive       : Boolean := False;
         Match_Must_Be_Unique : Boolean := False;
      end record;

   type Enumeration_Field is new C_Defined_Field_Type with private;

   function Create (Info : Enumeration_Info;
                    Auto_Release_Names : Boolean := False)
                    return Enumeration_Field;
   --  Make an fieldtype from the info. Enumerations are special, because
   --  they normally don't copy the enum values into a private store, so
   --  we have to care for the lifetime of the info we provide.
   --  The Auto_Release_Names flag may be used to automatically releases
   --  the strings in the Names array of the Enumeration_Info.

   function Make_Enumeration_Type (Info : Enumeration_Info;
                                   Auto_Release_Names : Boolean := False)
                                   return Enumeration_Field renames Create;

   procedure Release (Enum : in out Enumeration_Field);
   --  But we may want to release the field to release the memory allocated
   --  by it internally. After that the Enumeration field is no longer usable.

   procedure Set_Type (Fld      : in Field;
                       Fld_Type : in Enumeration_Field);
   function Native_Type (Ftype : Enumeration_Field)
                         return C_Field_Type;

   --  The next type defintions are all ncurses extensions. They are typically
   --  not available in other curses implementations.

   type Internet_V4_Address_Field is new C_Defined_Field_Type
     with null record;
   procedure Set_Type (Fld      : in Field;
                       Fld_Type : in Internet_V4_Address_Field);
   function Native_Type (Ftype : Internet_V4_Address_Field)
                         return C_Field_Type;


   type Ada_Defined_Field_Type is abstract new Field_Type with null record;
   --  This is the root of the mechanism we use to create field types in
   --  Ada95. You don't have to redefine the Set_Field_Type and
   --  Native_Field_Type methods, because they work generically on this
   --  class.

   procedure Set_Type (Fld      : Field;
                       Fld_Type : Ada_Defined_Field_Type);

   function Native_Type (Ftype : Ada_Defined_Field_Type)
                         return C_Field_Type;

   --  MANPAGE(`form_field_validation.3x')

   --  ANCHOR(`field_type()',`Get_Type')
   function Get_Type (Fld : in Field) return Field_Type_Access;
   --  AKA
   --  ALIAS(`field_arg()')
   --  In Ada95 we can combine these

------------------------------------------------------------------------------
private

   type Field        is new System.Address;
   type Form         is new System.Address;
   type C_Field_Type is new System.Address;

   Null_Field        : constant Field        := Field (System.Null_Address);
   Null_Form         : constant Form         := Form  (System.Null_Address);
   Null_Field_Type   : constant C_Field_Type :=
     C_Field_Type (System.Null_Address);

   type CPA_Access is access Interfaces.C.Strings.chars_ptr_array;

   type Enumeration_Field is new C_Defined_Field_Type with
      record
         Case_Sensitive       : Boolean := False;
         Match_Must_Be_Unique : Boolean := False;
         Arr                  : CPA_Access := null;
      end record;

   --  In our binding we use the fields user pointer as hook to maintain
   --  our own info structure about the field type. To be able to still
   --  provide a user pointer, we use this wrapper.
   --
   type Field_User_Wrapper is
      record
         U : System.Address;    --  the hook we provide for the user
         T : Field_Type_Access; --  may be null
         N : Natural;           --  use counter
      end record;
   pragma Convention (C, Field_User_Wrapper);
   type Field_User_Wrapper_Access is access all Field_User_Wrapper;
   pragma Controlled (Field_User_Wrapper_Access);

   function Set_Field_Userptr (Fld : Field;
                               Wrp : Field_User_Wrapper_Access)
                               return Interfaces.C.int;
   pragma Import (C, Set_Field_Userptr, "set_field_userptr");

   function Field_Userptr (Fld : Field) return Field_User_Wrapper_Access;
   pragma Import (C, Field_Userptr, "field_userptr");

   --  In our binding we use the forms user pointer as hook to maintain
   --  our own info structure about the field association. To be able to still
   --  provide a user pointer, we use this wrapper.
   --
   type Form_User_Wrapper is
      record
         U : System.Address;      --  the hook we provide for the user
         I : Field_Array_Access;
      end record;
   pragma Convention (C, Form_User_Wrapper);
   type Form_User_Wrapper_Access is access all Form_User_Wrapper;
   pragma Controlled (Form_User_Wrapper_Access);

   function Set_Form_Userptr (Frm : Form;
                              Wrp : Form_User_Wrapper_Access)
                              return Interfaces.C.int;
   pragma Import (C, Set_Form_Userptr, "set_form_userptr");

   function Form_Userptr (Frm : Form) return Form_User_Wrapper_Access;
   pragma Import (C, Form_Userptr, "form_userptr");

   procedure Register_Type   (T   : in Ada_Defined_Field_Type'Class;
                              Cft : in C_Field_Type);
   procedure Unregister_Type (T   : in Ada_Defined_Field_Type'Class);
   function  Search_Type (T : Ada_Defined_Field_Type'Class)
                          return C_Field_Type;

   Generation_Bit_Order : constant System.Bit_Order := System.M4_BIT_ORDER;
   --  This constant may be different on your system.

end Terminal_Interface.Curses.Forms;
