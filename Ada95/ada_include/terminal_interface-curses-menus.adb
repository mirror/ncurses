------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                      Terminal_Interface.Curses.Menus                     --
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
--  $Revision: 1.7 $
------------------------------------------------------------------------------
with Terminal_Interface.Curses.Aux; use Terminal_Interface.Curses.Aux;

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with Terminal_Interface.Curses;

with Ada.Unchecked_Deallocation;
with Unchecked_Conversion;

package body Terminal_Interface.Curses.Menus is

   use type System.Bit_Order;
   subtype chars_ptr is Interfaces.C.Strings.chars_ptr;

   function MOS_2_CInt is new
     Unchecked_Conversion (Menu_Option_Set,
                           C_Int);

   function CInt_2_MOS is new
     Unchecked_Conversion (C_Int,
                           Menu_Option_Set);

   function IOS_2_CInt is new
     Unchecked_Conversion (Item_Option_Set,
                           C_Int);

   function CInt_2_IOS is new
     Unchecked_Conversion (C_Int,
                           Item_Option_Set);

------------------------------------------------------------------------------
   procedure Free_Allocated_Items is
     new Ada.Unchecked_Deallocation (Item_Array, Item_Array_Access);

   procedure Free_User_Wrapper is
     new Ada.Unchecked_Deallocation (Ada_User_Wrapper,
                                     Ada_User_Wrapper_Access);

------------------------------------------------------------------------------
   procedure Request_Name (Key  : in Menu_Request_Code;
                                Name : out String)
   is
      function Request_Name (Key : C_Int) return chars_ptr;
      pragma Import (C, Request_Name, "menu_request_name");
   begin
      Fill_String (Request_Name (C_Int (Key)), Name);
   end Request_Name;

   --  !!! W A R N I N G !!!
   --  If you want to port this binding to a non ncurses version of the
   --  ETI, this must be rewritten. In ncurses the menu items and
   --  descriptions may be automatic variables, because ncurses copies
   --  the parameters into private allocated internal structures.
   --  Other implementations don't do that usually, so depending on
   --  scopes you may see unexpected results.
   function Create (Name        : String;
                    Description : String := "") return Item
   is
      type Char_Ptr is access all Interfaces.C.Char;
      function Newitem (Name, Desc : Char_Ptr) return Item;
      pragma Import (C, Newitem, "new_item");

      Name_Str : char_array (0 .. Name'Length);
      Desc_Str : char_array (0 .. Description'Length);
      Name_Len, Desc_Len : size_t;
      Result : Item;
   begin
      To_C (Name, Name_Str, Name_Len);
      To_C (Description, Desc_Str, Desc_Len);
      Result := Newitem (Name_Str (Name_Str'First)'Access,
                         Desc_Str (Desc_Str'First)'Access);
      if Result = Null_Item then
         raise Eti_System_Error;
      end if;
      return Result;
   end Create;

   procedure Delete (Itm : in out Item)
   is
      function Freeitem (Itm : Item) return C_Int;
      pragma Import (C, Freeitem, "free_item");

      Res : constant Eti_Error := Freeitem (Itm);
   begin
      if Res /= E_Ok then
         Eti_Exception (Res);
      end if;
      Itm := Null_Item;
   end Delete;
-------------------------------------------------------------------------------
   procedure Set_Value (Itm   : in Item;
                        Value : in Boolean := True)
   is
      function Set_Item_Val (Itm : Item;
                             Val : C_Int) return C_Int;
      pragma Import (C, Set_Item_Val, "set_item_value");

      Res : constant Eti_Error := Set_Item_Val (Itm, Boolean'Pos (Value));
   begin
      if  Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Value;

   function Value (Itm : Item) return Boolean
   is
      function Item_Val (Itm : Item) return C_Int;
      pragma Import (C, Item_Val, "item_value");
   begin
      if Item_Val (Itm) = Curses_False then
         return False;
      else
         return True;
      end if;
   end Value;

-------------------------------------------------------------------------------
   function Visible (Itm : Item) return Boolean
   is
      function Item_Vis (Itm : Item) return C_Int;
      pragma Import (C, Item_Vis, "item_visible");
   begin
      if Item_Vis (Itm) = Curses_False then
         return False;
      else
         return True;
      end if;
   end Visible;
-------------------------------------------------------------------------------
   procedure Normalize_Item_Options (Options : in out C_Int);
   pragma Import (C, Normalize_Item_Options, "_nc_ada_normalize_item_opts");

   procedure Set_Options (Itm     : in Item;
                          Options : in Item_Option_Set)
   is
      function Set_Item_Opts (Itm : Item;
                              Opt : C_Int) return C_Int;
      pragma Import (C, Set_Item_Opts, "set_item_opts");

      Opt : C_Int := IOS_2_CInt (Options);
      Res : Eti_Error;
   begin
      Normalize_Item_Options (Opt);
      Res := Set_Item_Opts (Itm, Opt);
      if Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Options;

   procedure Switch_Options (Itm     : in Item;
                             Options : in Item_Option_Set;
                             On      : Boolean := True)
   is
      function Item_Opts_On (Itm : Item;
                             Opt : C_Int) return C_Int;
      pragma Import (C, Item_Opts_On, "item_opts_on");
      function Item_Opts_Off (Itm : Item;
                              Opt : C_Int) return C_Int;
      pragma Import (C, Item_Opts_Off, "item_opts_off");

      Opt : C_Int := IOS_2_CInt (Options);
      Err : Eti_Error;
   begin
      Normalize_Item_Options (Opt);
      if On then
         Err := Item_Opts_On (Itm, Opt);
      else
         Err := Item_Opts_Off (Itm, Opt);
      end if;
      if Err /= E_Ok then
         Eti_Exception (Err);
      end if;
   end Switch_Options;

   procedure Get_Options (Itm     : in  Item;
                          Options : out Item_Option_Set)
   is
      function Item_Opts (Itm : Item) return C_Int;
      pragma Import (C, Item_Opts, "item_opts");

      Res : C_Int := Item_Opts (Itm);
   begin
      Normalize_Item_Options (Res);
      Options := CInt_2_IOS (Res);
   end Get_Options;

   function Get_Options (Itm : Item := Null_Item) return Item_Option_Set
   is
      Ios : Item_Option_Set;
   begin
      Get_Options (Itm, Ios);
      return Ios;
   end Get_Options;
-------------------------------------------------------------------------------
   procedure Name (Itm  : in Item;
                   Name : out String)
   is
      function Itemname (Itm : Item) return chars_ptr;
      pragma Import (C, Itemname, "item_name");
   begin
      Fill_String (Itemname (Itm), Name);
   end Name;

   procedure Description (Itm         : in Item;
                          Description : out String)
   is
      function Descname (Itm  : Item) return chars_ptr;
      pragma Import (C, Descname, "item_description");
   begin
      Fill_String (Descname (Itm), Description);
   end Description;
-------------------------------------------------------------------------------
   procedure Set_Current (Men : in Menu;
                          Itm : in Item)
   is
      function Set_Curr_Item (Men : Menu;
                              Itm : Item) return C_Int;
      pragma Import (C, Set_Curr_Item, "set_current_item");

      Res : constant Eti_Error := Set_Curr_Item (Men, Itm);
   begin
      if Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Current;

   function Current (Men : Menu) return Item
   is
      function Curr_Item (Men : Menu) return Item;
      pragma Import (C, Curr_Item, "current_item");

      Res : constant Item := Curr_Item (Men);
   begin
      if Res = Null_Item then
         raise Menu_Exception;
      end if;
      return Res;
   end Current;

   procedure Set_Top_Row (Men  : in Menu;
                          Line : in Line_Position)
   is
      function Set_Toprow (Men  : Menu;
                           Line : C_Int) return C_Int;
      pragma Import (C, Set_Toprow, "set_top_row");

      Res : constant Eti_Error := Set_Toprow (Men, C_Int (Line));
   begin
      if  Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Top_Row;

   function Top_Row (Men : Menu) return Line_Position
   is
      function Toprow (Men : Menu) return C_Int;
      pragma Import (C, Toprow, "top_row");

      Res : constant C_Int := Toprow (Men);
   begin
      if Res = Curses_Err then
         raise Menu_Exception;
      end if;
      return Line_Position (Res);
   end Top_Row;

   function Get_Index (Itm : Item) return Positive
   is
      function Get_Itemindex (Itm : Item) return C_Int;
      pragma Import (C, Get_Itemindex, "item_index");

      Res : constant C_Int := Get_Itemindex (Itm);
   begin
      if Res = Curses_Err then
         raise Menu_Exception;
      end if;
      return Positive (Natural (Res) + Positive'First);
   end Get_Index;
-------------------------------------------------------------------------------
   procedure Post (Men  : in Menu;
                   Post : in Boolean := True)
   is
      function M_Post (Men : Menu) return C_Int;
      pragma Import (C, M_Post, "post_menu");
      function M_Unpost (Men : Menu) return C_Int;
      pragma Import (C, M_Unpost, "unpost_menu");

      Res : Eti_Error;
   begin
      if Post then
         Res := M_Post (Men);
      else
         Res := M_Unpost (Men);
      end if;
      if Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Post;
-------------------------------------------------------------------------------
   procedure Normalize_Menu_Options (Options : in out C_Int);
   pragma Import (C, Normalize_Menu_Options, "_nc_ada_normalize_menu_opts");

   procedure Set_Options (Men     : in Menu;
                          Options : in Menu_Option_Set)
   is
      function Set_Menu_Opts (Men : Menu;
                              Opt : C_Int) return C_Int;
      pragma Import (C, Set_Menu_Opts, "set_menu_opts");

      Opt : C_Int := MOS_2_CInt (Options);
      Res : Eti_Error;
   begin
      Normalize_Menu_Options (Opt);
      Res := Set_Menu_Opts (Men, Opt);
      if  Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Options;

   procedure Switch_Options (Men     : in Menu;
                             Options : in Menu_Option_Set;
                             On      : in Boolean := True)
   is
      function Menu_Opts_On (Men : Menu;
                             Opt : C_Int) return C_Int;
      pragma Import (C, Menu_Opts_On, "menu_opts_on");
      function Menu_Opts_Off (Men : Menu;
                              Opt : C_Int) return C_Int;
      pragma Import (C, Menu_Opts_Off, "menu_opts_off");

      Opt : C_Int := MOS_2_CInt (Options);
      Err : Eti_Error;
   begin
      Normalize_Menu_Options (Opt);
      if On then
         Err := Menu_Opts_On  (Men, Opt);
      else
         Err := Menu_Opts_Off (Men, Opt);
      end if;
      if Err /= E_Ok then
         Eti_Exception (Err);
      end if;
   end Switch_Options;

   procedure Get_Options (Men     : in  Menu;
                               Options : out Menu_Option_Set)
   is
      function Menu_Opts (Men : Menu) return C_Int;
      pragma Import (C, Menu_Opts, "menu_opts");

      Res : C_Int := Menu_Opts (Men);
   begin
      Normalize_Menu_Options (Res);
      Options := CInt_2_MOS (Res);
   end Get_Options;

   function Get_Options (Men : Menu := Null_Menu) return Menu_Option_Set
   is
      Mos : Menu_Option_Set;
   begin
      Get_Options (Men, Mos);
      return Mos;
   end Get_Options;
-------------------------------------------------------------------------------
   procedure Set_Window (Men : in Menu;
                         Win : in Window)
   is
      function Set_Menu_Win (Men : Menu;
                             Win : Window) return C_Int;
      pragma Import (C, Set_Menu_Win, "set_menu_win");

      Res : constant Eti_Error := Set_Menu_Win (Men, Win);
   begin
      if  Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Window;

   function Get_Window (Men : Menu) return Window
   is
      function Menu_Win (Men : Menu) return Window;
      pragma Import (C, Menu_Win, "menu_win");

      W : constant Window := Menu_Win (Men);
   begin
      return W;
   end Get_Window;

   procedure Set_Sub_Window (Men : in Menu;
                             Win : in Window)
   is
      function Set_Menu_Sub (Men : Menu;
                             Win : Window) return C_Int;
      pragma Import (C, Set_Menu_Sub, "set_menu_sub");

      Res : constant Eti_Error := Set_Menu_Sub (Men, Win);
   begin
      if  Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Sub_Window;

   function Get_Sub_Window (Men : Menu) return Window
   is
      function Menu_Sub (Men : Menu) return Window;
      pragma Import (C, Menu_Sub, "menu_sub");

      W : constant Window := Menu_Sub (Men);
   begin
      return W;
   end Get_Sub_Window;

   procedure Scale (Men     : in Menu;
                    Lines   : out Line_Count;
                    Columns : out Column_Count)
   is
      type C_Int_Access is access all C_Int;
      function M_Scale (Men    : Menu;
                        Yp, Xp : C_Int_Access) return C_Int;
      pragma Import (C, M_Scale, "scale_menu");

      X, Y : aliased C_Int;
      Res  : constant Eti_Error := M_Scale (Men, Y'Access, X'Access);
   begin
      if Res /= E_Ok then
         Eti_Exception (Res);
      end if;
      Lines := Line_Count (Y);
      Columns := Column_Count (X);
   end Scale;
-------------------------------------------------------------------------------
   procedure Position_Cursor (Men : Menu)
   is
      function Pos_Menu_Cursor (Men : Menu) return C_Int;
      pragma Import (C, Pos_Menu_Cursor, "pos_menu_cursor");

      Res : constant Eti_Error := Pos_Menu_Cursor (Men);
   begin
      if  Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Position_Cursor;

-------------------------------------------------------------------------------
   procedure Set_Mark (Men  : in Menu;
                       Mark : in String)
   is
      type Char_Ptr is access all Interfaces.C.Char;
      function Set_Mark (Men  : Menu;
                         Mark : Char_Ptr) return C_Int;
      pragma Import (C, Set_Mark, "set_menu_mark");

      Txt : char_array (0 .. Mark'Length);
      Len : size_t;
      Res : Eti_Error;
   begin
      To_C (Mark, Txt, Len);
      Res := Set_Mark (Men, Txt (Txt'First)'Access);
      if Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Mark;

   procedure Mark (Men  : in  Menu;
                   Mark : out String)
   is
      function Get_Menu_Mark (Men : Menu) return chars_ptr;
      pragma Import (C, Get_Menu_Mark, "menu_mark");
   begin
      Fill_String (Get_Menu_Mark (Men), Mark);
   end Mark;

-------------------------------------------------------------------------------
   procedure Set_Foreground
     (Men   : in Menu;
      Fore  : in Character_Attribute_Set := Normal_Video;
      Color : in Color_Pair := Color_Pair'First)
   is
      function Set_Menu_Fore (Men  : Menu;
                              Attr : C_Int) return C_Int;
      pragma Import (C, Set_Menu_Fore, "set_menu_fore");

      Ch : constant Attributed_Character := (Ch    => Character'First,
                                             Color => Color,
                                             Attr  => Fore);
      Res : constant Eti_Error := Set_Menu_Fore (Men, Chtype_To_CInt (Ch));
   begin
      if  Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Foreground;

   procedure Foreground (Men  : in  Menu;
                         Fore : out Character_Attribute_Set)
   is
      function Menu_Fore (Men : Menu) return C_Int;
      pragma Import (C, Menu_Fore, "menu_fore");
   begin
      Fore := CInt_To_Chtype (Menu_Fore (Men)).Attr;
   end Foreground;

   procedure Foreground (Men   : in  Menu;
                         Fore  : out Character_Attribute_Set;
                         Color : out Color_Pair)
   is
      function Menu_Fore (Men : Menu) return C_Int;
      pragma Import (C, Menu_Fore, "menu_fore");
   begin
      Fore  := CInt_To_Chtype (Menu_Fore (Men)).Attr;
      Color := CInt_To_Chtype (Menu_Fore (Men)).Color;
   end Foreground;

   procedure Set_Background
     (Men   : in Menu;
      Back  : in Character_Attribute_Set := Normal_Video;
      Color : in Color_Pair := Color_Pair'First)
   is
      function Set_Menu_Back (Men  : Menu;
                              Attr : C_Int) return C_Int;
      pragma Import (C, Set_Menu_Back, "set_menu_back");

      Ch : constant Attributed_Character := (Ch    => Character'First,
                                             Color => Color,
                                             Attr  => Back);
      Res : constant Eti_Error := Set_Menu_Back (Men, Chtype_To_CInt (Ch));
   begin
      if  Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Background;

   procedure Background (Men  : in  Menu;
                         Back : out Character_Attribute_Set)
   is
      function Menu_Back (Men : Menu) return C_Int;
      pragma Import (C, Menu_Back, "menu_back");
   begin
      Back := CInt_To_Chtype (Menu_Back (Men)).Attr;
   end Background;

   procedure Background (Men   : in  Menu;
                         Back  : out Character_Attribute_Set;
                         Color : out Color_Pair)
   is
      function Menu_Back (Men : Menu) return C_Int;
      pragma Import (C, Menu_Back, "menu_back");
   begin
      Back  := CInt_To_Chtype (Menu_Back (Men)).Attr;
      Color := CInt_To_Chtype (Menu_Back (Men)).Color;
   end Background;

   procedure Set_Grey (Men   : in Menu;
                       Grey  : in Character_Attribute_Set := Normal_Video;
                       Color : in Color_Pair := Color_Pair'First)
   is
      function Set_Menu_Grey (Men  : Menu;
                              Attr : C_Int) return C_Int;
      pragma Import (C, Set_Menu_Grey, "set_menu_grey");

      Ch : constant Attributed_Character := (Ch    => Character'First,
                                             Color => Color,
                                             Attr  => Grey);

      Res : constant Eti_Error := Set_Menu_Grey (Men, Chtype_To_CInt (Ch));
   begin
      if  Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Grey;

   procedure Grey (Men  : in  Menu;
                   Grey : out Character_Attribute_Set)
   is
      function Menu_Grey (Men : Menu) return C_Int;
      pragma Import (C, Menu_Grey, "menu_grey");
   begin
      Grey := CInt_To_Chtype (Menu_Grey (Men)).Attr;
   end Grey;

   procedure Grey (Men  : in  Menu;
                   Grey : out Character_Attribute_Set;
                   Color : out Color_Pair)
   is
      function Menu_Grey (Men : Menu) return C_Int;
      pragma Import (C, Menu_Grey, "menu_grey");
   begin
      Grey  := CInt_To_Chtype (Menu_Grey (Men)).Attr;
      Color := CInt_To_Chtype (Menu_Grey (Men)).Color;
   end Grey;

   procedure Set_Pad_Character (Men : in Menu;
                                Pad : in Character := Space)
   is
      function Set_Menu_Pad (Men : Menu;
                             Ch  : C_Int) return C_Int;
      pragma Import (C, Set_Menu_Pad, "set_menu_pad");

      Res : constant Eti_Error := Set_Menu_Pad (Men,
                                                C_Int (Character'Pos (Pad)));
   begin
      if Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Pad_Character;

   procedure Pad_Character (Men : in  Menu;
                            Pad : out Character)
   is
      function Menu_Pad (Men : Menu) return C_Int;
      pragma Import (C, Menu_Pad, "menu_pad");
   begin
      Pad := Character'Val (Menu_Pad (Men));
   end Pad_Character;
-------------------------------------------------------------------------------
   procedure Set_Spacing (Men   : in Menu;
                          Descr : in Column_Position := 0;
                          Row   : in Line_Position   := 0;
                          Col   : in Column_Position := 0)
   is
      function Set_Spacing (Men     : Menu;
                            D, R, C : C_Int) return C_Int;
      pragma Import (C, Set_Spacing, "set_menu_spacing");

      Res : constant Eti_Error := Set_Spacing (Men,
                                               C_Int (Descr),
                                               C_Int (Row),
                                               C_Int (Col));
   begin
      if Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Spacing;

   procedure Spacing (Men   : in Menu;
                      Descr : out Column_Position;
                      Row   : out Line_Position;
                      Col   : out Column_Position)
   is
      type C_Int_Access is access all C_Int;
      function Get_Spacing (Men     : Menu;
                            D, R, C : C_Int_Access) return C_Int;
      pragma Import (C, Get_Spacing, "menu_spacing");

      D, R, C : aliased C_Int;
      Res : constant Eti_Error := Get_Spacing (Men,
                                               D'Access,
                                               R'Access,
                                               C'Access);
   begin
      if Res /= E_Ok then
         Eti_Exception (Res);
      else
         Descr := Column_Position (D);
         Row   := Line_Position (R);
         Col   := Column_Position (C);
      end if;
   end Spacing;
-------------------------------------------------------------------------------
   function Set_Pattern (Men  : Menu;
                         Text : String) return Boolean
   is
      type Char_Ptr is access all Interfaces.C.Char;
      function Set_Pattern (Men     : Menu;
                            Pattern : Char_Ptr) return C_Int;
      pragma Import (C, Set_Pattern, "set_menu_pattern");

      S   : char_array (0 .. Text'Length);
      L   : size_t;
      Res : Eti_Error;
   begin
      To_C (Text, S, L);
      Res := Set_Pattern (Men, S (S'First)'Access);
      case Res is
         when E_No_Match => return False;
         when E_Ok       => return True;
         when others =>
            Eti_Exception (Res);
            return False;
      end case;
   end Set_Pattern;

   procedure Pattern (Men  : in  Menu;
                           Text : out String)
   is
      function Get_Pattern (Men : Menu) return chars_ptr;
      pragma Import (C, Get_Pattern, "menu_pattern");
   begin
      Fill_String (Get_Pattern (Men), Text);
   end Pattern;
-------------------------------------------------------------------------------
   procedure Set_Format (Men     : in Menu;
                         Lines   : in Line_Count;
                         Columns : in Column_Count)
   is
      function Set_Menu_Fmt (Men : Menu;
                             Lin : C_Int;
                             Col : C_Int) return C_Int;
      pragma Import (C, Set_Menu_Fmt, "set_menu_format");

      Res : constant Eti_Error := Set_Menu_Fmt (Men,
                                                C_Int (Lines),
                                                C_Int (Columns));
   begin
      if  Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Format;

   procedure Format (Men     : in  Menu;
                     Lines   : out Line_Count;
                     Columns : out Column_Count)
   is
      type C_Int_Access is access all C_Int;
      function Menu_Fmt (Men  : Menu;
                         Y, X : C_Int_Access) return C_Int;
      pragma Import (C, Menu_Fmt, "menu_format");

      L, C : aliased C_Int;
      Res  : constant Eti_Error := Menu_Fmt (Men, L'Access, C'Access);
   begin
      if Res /= E_Ok then
         Eti_Exception (Res);
      else
         Lines   := Line_Count (L);
         Columns := Column_Count (C);
      end if;
   end Format;
-------------------------------------------------------------------------------
   procedure Set_Item_Init_Hook (Men  : in Menu;
                                 Proc : in Menu_Hook_Function)
   is
      function Set_Item_Init (Men  : Menu;
                              Proc : Menu_Hook_Function) return C_Int;
      pragma Import (C, Set_Item_Init, "set_item_init");

      Res : constant Eti_Error := Set_Item_Init (Men, Proc);
   begin
      if  Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Item_Init_Hook;

   procedure Set_Item_Term_Hook (Men  : in Menu;
                                 Proc : in Menu_Hook_Function)
   is
      function Set_Item_Term (Men  : Menu;
                              Proc : Menu_Hook_Function) return C_Int;
      pragma Import (C, Set_Item_Term, "set_item_term");

      Res : constant Eti_Error := Set_Item_Term (Men, Proc);
   begin
      if Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Item_Term_Hook;

   procedure Set_Menu_Init_Hook (Men  : in Menu;
                                 Proc : in Menu_Hook_Function)
   is
      function Set_Menu_Init (Men  : Menu;
                              Proc : Menu_Hook_Function) return C_Int;
      pragma Import (C, Set_Menu_Init, "set_menu_init");

      Res : constant Eti_Error := Set_Menu_Init (Men, Proc);
   begin
      if  Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Menu_Init_Hook;

   procedure Set_Menu_Term_Hook (Men  : in Menu;
                                 Proc : in Menu_Hook_Function)
   is
      function Set_Menu_Term (Men  : Menu;
                              Proc : Menu_Hook_Function) return C_Int;
      pragma Import (C, Set_Menu_Term, "set_menu_term");

      Res : constant Eti_Error := Set_Menu_Term (Men, Proc);
   begin
      if Res /= E_Ok then
         Eti_Exception (Res);
      end if;
   end Set_Menu_Term_Hook;

   function Get_Item_Init_Hook (Men : Menu) return Menu_Hook_Function
   is
      function Item_Init (Men : Menu) return Menu_Hook_Function;
      pragma Import (C, Item_Init, "item_init");
   begin
      return Item_Init (Men);
   end Get_Item_Init_Hook;

   function Get_Item_Term_Hook (Men : Menu) return Menu_Hook_Function
   is
      function Item_Term (Men : Menu) return Menu_Hook_Function;
      pragma Import (C, Item_Term, "item_term");
   begin
      return Item_Term (Men);
   end Get_Item_Term_Hook;

   function Get_Menu_Init_Hook (Men : Menu) return Menu_Hook_Function
   is
      function Menu_Init (Men : Menu) return Menu_Hook_Function;
      pragma Import (C, Menu_Init, "menu_init");
   begin
      return Menu_Init (Men);
   end Get_Menu_Init_Hook;

   function Get_Menu_Term_Hook (Men : Menu) return Menu_Hook_Function
   is
      function Menu_Term (Men : Menu) return Menu_Hook_Function;
      pragma Import (C, Menu_Term, "menu_term");
   begin
      return Menu_Term (Men);
   end Get_Menu_Term_Hook;
-------------------------------------------------------------------------------
   --  This is a bit delicate if we want to manipulate an Ada created menu
   --  from C routines or vice versa.
   --  In Ada created menus we use the low level user pointer to maintain
   --  binding internal additional informations about the menu. This
   --  internal information contains a hook for the Ada provided user pointer.
   --  Unless you understand this implementation, the safest way in mixed
   --  language programs to deal with user pointers is, that only the language
   --  that created the menu should also manipulate the user pointer for that
   --  menu.
   procedure Redefine (Men   : in Menu;
                       Items : in Item_Array)
   is
      function Set_Items (Men   : Menu;
                          Items : Item_Array_Access) return C_Int;
      pragma Import (C, Set_Items, "set_menu_items");

      function Menu_Userptr (Men : Menu) return Ada_User_Wrapper_Access;
      pragma Import (C, Menu_Userptr, "menu_userptr");

      U   : Ada_User_Wrapper_Access := Menu_Userptr (Men);
      I   : Item_Array_Access;
      Res : Eti_Error;
   begin
      if U = null or else U.I = null then raise Menu_Exception;
      else
         --  create internally an array of items that contains an
         --  additional place for the terminating null item.
         I := new Item_Array (1 .. (Items'Length + 1));
         I.all (1 .. Items'Length) := Items (Items'First .. Items'Last);
         I.all (Items'Length + 1) := Null_Item;
         Res := Set_Items (Men, I);
         if  Res /= E_Ok then
            Free_Allocated_Items (I);
            Eti_Exception (Res);
         else
            Free_Allocated_Items (U.I);
            U.I := I;
         end if;
      end if;
   end Redefine;

   function Item_Count (Men : Menu) return Natural
   is
      function Count (Men : Menu) return C_Int;
      pragma Import (C, Count, "item_count");
   begin
      return Natural (Count (Men));
   end Item_Count;

   function Items (Men : Menu) return Item_Array_Access
   is
      function M_Items (Men : Menu) return Item_Array_Access;
      pragma Import (C, M_Items, "menu_items");
   begin
      return M_Items (Men);
   end Items;

-------------------------------------------------------------------------------
   function Create (Items : Item_Array) return Menu
   is
      function Newmenu (Items : Item_Array_Access) return Menu;
      pragma Import (C, Newmenu, "new_menu");

      function Set_Menu_Userptr (Men  : Menu;
                                 Addr : Ada_User_Wrapper_Access)  return C_Int;
      pragma Import (C, Set_Menu_Userptr, "set_menu_userptr");

      M   : Menu;
      I   : Item_Array_Access;
      U   : Ada_User_Wrapper_Access;
      Res : Eti_Error;
   begin
      I := new Item_Array (1 .. (Items'Length + 1));
      I.all (1 .. Items'Length) := Items (Items'First .. Items'Last);
      I.all (Items'Length + 1) := Null_Item;
      M := Newmenu (I);
      if M = Null_Menu then
         Free_Allocated_Items (I);
         raise Menu_Exception;
      end if;
      U := new Ada_User_Wrapper' (System.Null_Address, I);
      Res := Set_Menu_Userptr (M, U);
      if  Res /= E_Ok then
         Free_Allocated_Items (I);
         Free_User_Wrapper (U);
         Eti_Exception (Res);
      end if;
      return M;
   end Create;

   procedure Delete (Men : in out Menu)
   is
      function Free (Men : Menu) return C_Int;
      pragma Import (C, Free, "free_menu");
      function Menu_Userptr (Men : Menu) return Ada_User_Wrapper_Access;
      pragma Import (C, Menu_Userptr, "menu_userptr");

      U   : Ada_User_Wrapper_Access := Menu_Userptr (Men);
      Res : constant Eti_Error := Free (Men);
   begin
      if Res /= E_Ok then
         Eti_Exception (Res);
      end if;
      if U = null or else U.I = null then
         raise Menu_Exception;
      end if;
      Free_Allocated_Items (U.I);
      Free_User_Wrapper (U);
      Men := Null_Menu;
   end Delete;

------------------------------------------------------------------------------
   function Driver (Men : Menu;
                    Key : Key_Code) return Driver_Result
   is
      function Driver (Men : Menu;
                       Key : C_Int) return C_Int;
      pragma Import (C, Driver, "menu_driver");

      R : Eti_Error := Driver (Men, C_Int (Key));
   begin
      if R /= E_Ok then
         case R is
            when E_Unknown_Command  => return Unknown_Request;
            when E_No_Match         => return No_Match;
            when E_Request_Denied |
                 E_Not_Selectable   => return Request_Denied;
            when others =>
               Eti_Exception (R);
         end case;
      end if;
      return Menu_Ok;
   end Driver;
-------------------------------------------------------------------------------
begin
   if Generation_Bit_Order /= System.Default_Bit_Order then
      raise Constraint_Error;
   end if;

   Default_Menu_Options  := Get_Options (Null_Menu);
   Default_Item_Options  := Get_Options (Null_Item);
end Terminal_Interface.Curses.Menus;
