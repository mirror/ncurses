--  -*- ada -*-
define(`HTMLNAME',`terminal_interface-curses-menus_s.html')dnl
include(M4MACRO)dnl
------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                      Terminal_Interface.Curses.Menu                      --
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
--  $Revision: 1.7 $
------------------------------------------------------------------------------
include(`Menu_Base_Defs')
with System;
with Interfaces.C;
with Ada.Characters.Latin_1;

package Terminal_Interface.Curses.Menus is

include(`Menu_Linker_Options')


   Space : Character renames Ada.Characters.Latin_1.Space;

   type Item is private;
   type Menu is private;

   ---------------------------
   --  Interface constants  --
   ---------------------------
   Null_Item : constant Item;
   Null_Menu : constant Menu;

   subtype Menu_Request_Code is Key_Code
     range (Key_Max + 1) .. (Key_Max + 17);

   --  The prefix M_ stands for "Menu Request"
   M_Left_Item       : constant Menu_Request_Code := Key_Max + 1;
   M_Right_Item      : constant Menu_Request_Code := Key_Max + 2;
   M_Up_Item         : constant Menu_Request_Code := Key_Max + 3;
   M_Down_Item       : constant Menu_Request_Code := Key_Max + 4;
   M_ScrollUp_Line   : constant Menu_Request_Code := Key_Max + 5;
   M_ScrollDown_Line : constant Menu_Request_Code := Key_Max + 6;
   M_ScrollDown_Page : constant Menu_Request_Code := Key_Max + 7;
   M_ScrollUp_Page   : constant Menu_Request_Code := Key_Max + 8;
   M_First_Item      : constant Menu_Request_Code := Key_Max + 9;
   M_Last_Item       : constant Menu_Request_Code := Key_Max + 10;
   M_Next_Item       : constant Menu_Request_Code := Key_Max + 11;
   M_Previous_Item   : constant Menu_Request_Code := Key_Max + 12;
   M_Toggle_Item     : constant Menu_Request_Code := Key_Max + 13;
   M_Clear_Pattern   : constant Menu_Request_Code := Key_Max + 14;
   M_Back_Pattern    : constant Menu_Request_Code := Key_Max + 15;
   M_Next_Match      : constant Menu_Request_Code := Key_Max + 16;
   M_Previous_Match  : constant Menu_Request_Code := Key_Max + 17;

   --  For those who like the old 'C' names for the request codes
   REQ_LEFT_ITEM     : Menu_Request_Code renames M_Left_Item;
   REQ_RIGHT_ITEM    : Menu_Request_Code renames M_Right_Item;
   REQ_UP_ITEM       : Menu_Request_Code renames M_Up_Item;
   REQ_DOWN_ITEM     : Menu_Request_Code renames M_Down_Item;
   REQ_SCR_ULINE     : Menu_Request_Code renames M_ScrollUp_Line;
   REQ_SCR_DLINE     : Menu_Request_Code renames M_ScrollDown_Line;
   REQ_SCR_DPAGE     : Menu_Request_Code renames M_ScrollDown_Page;
   REQ_SCR_UPAGE     : Menu_Request_Code renames M_ScrollUp_Page;
   REQ_FIRST_ITEM    : Menu_Request_Code renames M_First_Item;
   REQ_LAST_ITEM     : Menu_Request_Code renames M_Last_Item;
   REQ_NEXT_ITEM     : Menu_Request_Code renames M_Next_Item;
   REQ_PREV_ITEM     : Menu_Request_Code renames M_Previous_Item;
   REQ_TOGGLE_ITEM   : Menu_Request_Code renames M_Toggle_Item;
   REQ_CLEAR_PATTERN : Menu_Request_Code renames M_Clear_Pattern;
   REQ_BACK_PATTERN  : Menu_Request_Code renames M_Back_Pattern;
   REQ_NEXT_MATCH    : Menu_Request_Code renames M_Next_Match;
   REQ_PREV_MATCH    : Menu_Request_Code renames M_Previous_Match;

   procedure Request_Name (Key  : in Menu_Request_Code;
                           Name : out String);

   ------------------
   --  Exceptions  --
   ------------------

   Menu_Exception : exception;
   --
   --  Menu options
   --

include(`Menu_Opt_Rep')

   Default_Menu_Options : Menu_Option_Set;
   --  Initial default options for a menu.

   --
   --  Item options
   --
include(`Item_Rep')

   Default_Item_Options : Item_Option_Set;
   --  Initial default options for an item.

   --
   --  Item Array
   --
   type Item_Array is array (Positive range <>) of aliased Item;
   pragma Convention (C, Item_Array);

   type Item_Array_Access is access all Item_Array;

   --  MANPAGE(`mitem_new.3x')

   --  ANCHOR(`new_item()',`Create')
   function Create (Name        : String;
                    Description : String := "") return Item;
   --  AKA

   --  ANCHOR(`new_item()',`New_Item')
   function New_Item (Name        : String;
                      Description : String := "") return Item
     renames Create;
   --  AKA

   --  ANCHOR(`free_item()',`Delete')
   procedure Delete (Itm : in out Item);
   --  AKA
   --  Resets Itm to Null_Item

   --  MANPAGE(`mitem_value.3x')

   --  ANCHOR(`set_item_value()',`Set_Value')
   procedure Set_Value (Itm   : in Item;
                        Value : in Boolean := True);
   --  AKA

   --  ANCHOR(`item_value()',`Value')
   function Value (Itm : Item) return Boolean;
   --  AKA

   --  MANPAGE(`mitem_visible.3x')

   --  ANCHOR(`item_visible()',`Visible')
   function Visible (Itm : Item) return Boolean;
   --  AKA

   --  MANPAGE(`mitem_opts.3x')

   --  ANCHOR(`set_item_opts()',`Set_Options')
   procedure Set_Options (Itm     : in Item;
                          Options : in Item_Option_Set);
   --  AKA

   --  ANCHOR(`item_opts_on()',`Switch_Options')
   procedure Switch_Options (Itm     : in Item;
                             Options : in Item_Option_Set;
                             On      : Boolean := True);
   --  AKA
   --  ALIAS(`item_opts_off()')

   --  ANCHOR(`item_opts()',`Get_Options')
   procedure Get_Options (Itm     : in  Item;
                          Options : out Item_Option_Set);
   --  AKA

   --  ANCHOR(`item_opts()',`Get_Options')
   function Get_Options (Itm : Item := Null_Item) return Item_Option_Set;
   --  AKA

   --  MANPAGE(`mitem_name.3x')

   --  ANCHOR(`item_name()',`Name')
   procedure Name (Itm  : in Item;
                   Name : out String);
   --  AKA

   --  ANCHOR(`item_description();',`Description')
   procedure Description (Itm         : in Item;
                          Description : out String);
   --  AKA

   --  MANPAGE(`mitem_current.3x')

   --  ANCHOR(`set_current_item()',`Set_Current')
   procedure Set_Current (Men : in Menu;
                          Itm : in Item);
   --  AKA

   --  ANCHOR(`current_item()',`Current')
   function Current (Men : Menu) return Item;
   --  AKA

   --  ANCHOR(`set_top_row()',`Set_Top_Row')
   procedure Set_Top_Row (Men  : in Menu;
                          Line : in Line_Position);
   --  AKA

   --  ANCHOR(`top_row()',`Top_Row')
   function Top_Row (Men : Menu) return Line_Position;
   --  AKA

   --  ANCHOR(`item_index()',`Get_Index')
   function Get_Index (Itm : Item) return Positive;
   --  AKA
   --  Please note that in this binding we start the numbering of items
   --  with 1. So this is number is one more than you get from the low
   --  level call.

   --  MANPAGE(`menu_post.3x')

   --  ANCHOR(`post_menu()',`Post')
   procedure Post (Men  : in Menu;
                   Post : in Boolean := True);
   --  AKA
   --  ALIAS(`unpost_menu()')

   --  MANPAGE(`menu_opts.3x')

   --  ANCHOR(`set_menu_opts()',`Set_Options')
   procedure Set_Options (Men     : in Menu;
                          Options : in Menu_Option_Set);
   --  AKA

   --  ANCHOR(`menu_opts_on()',`Switch_Options')
   procedure Switch_Options (Men     : in Menu;
                             Options : in Menu_Option_Set;
                             On      : Boolean := True);
   --  AKA
   --  ALIAS(`menu_opts_off()')

   --  ANCHOR(`menu_opts()',`Get_Options')
   procedure Get_Options (Men     : in  Menu;
                          Options : out Menu_Option_Set);
   --  AKA

   --  ANCHOR(`menu_opts()',`Get_Options')
   function Get_Options (Men : Menu := Null_Menu) return Menu_Option_Set;
   --  AKA

   --  MANPAGE(`menu_win.3x')

   --  ANCHOR(`set_menu_win()',`Set_Window')
   procedure Set_Window (Men : in Menu;
                         Win : in Window);
   --  AKA

   --  ANCHOR(`menu_win()',`Get_Window')
   function Get_Window (Men : Menu) return Window;
   --  AKA

   --  ANCHOR(`set_menu_sub()',`Set_Sub_Window')
   procedure Set_Sub_Window (Men : in Menu;
                             Win : in Window);
   --  AKA

   --  ANCHOR(`menu_sub()',`Get_Sub_Window')
   function Get_Sub_Window (Men : Menu) return Window;
   --  AKA

   --  ANCHOR(`scale_menu()',`Scale')
   procedure Scale (Men     : in Menu;
                    Lines   : out Line_Count;
                    Columns : out Column_Count);
   --  AKA

   --  MANPAGE(`menu_cursor.3x')

   --  ANCHOR(`pos_menu_cursor()',`Position_Cursor')
   procedure Position_Cursor (Men : Menu);
   --  AKA

   --  MANPAGE(`menu_mark.3x')

   --  ANCHOR(`set_menu_mark()',`Set_Mark')
   procedure Set_Mark (Men  : in Menu;
                       Mark : in String);
   --  AKA

   --  ANCHOR(`menu_mark()',`Mark')
   procedure Mark (Men  : in  Menu;
                   Mark : out String);
   --  AKA

   --  MANPAGE(`menu_attribs.3x')

   --  ANCHOR(`set_menu_fore()',`Set_Foreground')
   procedure Set_Foreground
     (Men   : in Menu;
      Fore  : in Character_Attribute_Set := Normal_Video;
      Color : in Color_Pair := Color_Pair'First);
   --  AKA

   --  ANCHOR(`menu_fore()',`Foreground')
   procedure Foreground (Men   : in  Menu;
                         Fore  : out Character_Attribute_Set);
   --  AKA

   --  ANCHOR(`menu_fore()',`Foreground')
   procedure Foreground (Men   : in  Menu;
                         Fore  : out Character_Attribute_Set;
                         Color : out Color_Pair);
   --  AKA

   --  ANCHOR(`set_menu_back()',`Set_Background')
   procedure Set_Background
     (Men   : in Menu;
      Back  : in Character_Attribute_Set := Normal_Video;
      Color : in Color_Pair := Color_Pair'First);
   --  AKA

   --  ANCHOR(`menu_back()',`Background')
   procedure Background (Men  : in  Menu;
                         Back : out Character_Attribute_Set);
   --  AKA
   --  ANCHOR(`menu_back()',`Background')

   procedure Background (Men   : in  Menu;
                         Back  : out Character_Attribute_Set;
                         Color : out Color_Pair);
   --  AKA

   --  ANCHOR(`set_menu_grey()',`Set_Grey')
   procedure Set_Grey
     (Men   : in Menu;
      Grey  : in Character_Attribute_Set := Normal_Video;
      Color : in Color_Pair := Color_Pair'First);
   --  AKA

   --  ANCHOR(`menu_grey()',`Grey')
   procedure Grey (Men  : in  Menu;
                   Grey : out Character_Attribute_Set);
   --  AKA

   --  ANCHOR(`menu_grey()',`Grey')
   procedure Grey
     (Men   : in  Menu;
      Grey  : out Character_Attribute_Set;
      Color : out Color_Pair);
   --  AKA

   --  ANCHOR(`set_menu_pad()',`Set_Pad_Character')
   procedure Set_Pad_Character (Men : in Menu;
                                Pad : in Character := Space);
   --  AKA

   --  ANCHOR(`menu_pad()',`Pad_Character')
   procedure Pad_Character (Men : in  Menu;
                            Pad : out Character);
   --  AKA

   --  MANPAGE(`menu_spacing.3x')

   --  ANCHOR(`set_menu_spacing()',`Set_Spacing')
   procedure Set_Spacing (Men   : in Menu;
                          Descr : in Column_Position := 0;
                          Row   : in Line_Position   := 0;
                          Col   : in Column_Position := 0);
   --  AKA

   --  ANCHOR(`menu_spacing()',`Spacing')
   procedure Spacing (Men   : in Menu;
                      Descr : out Column_Position;
                      Row   : out Line_Position;
                      Col   : out Column_Position);
   --  AKA

   --  MANPAGE(`menu_pattern.3x')

   --  ANCHOR(`set_menu_pattern()',`Set_Pattern')
   function Set_Pattern (Men  : Menu;
                         Text : String) return Boolean;
   --  AKA
   --  Return TRUE if the pattern matches, FALSE otherwise

   --  ANCHOR(`menu_pattern()',`Pattern')
   procedure Pattern (Men  : in  Menu;
                      Text : out String);
   --  AKA

   --  MANPAGE(`menu_format.3x')

   --  ANCHOR(`set_menu_format()',`Set_Format')
   procedure Set_Format (Men     : in Menu;
                         Lines   : in Line_Count;
                         Columns : in Column_Count);
   --  AKA

   --  ANCHOR(`menu_format()',`Format')
   procedure Format (Men     : in  Menu;
                     Lines   : out Line_Count;
                     Columns : out Column_Count);
   --  AKA

   --  MANPAGE(`menu_hook.3x')

   type Menu_Hook_Function is access procedure (Men : in Menu);
   pragma Convention (C, Menu_Hook_Function);

   --  ANCHOR(`set_item_init()',`Set_Item_Init_Hook')
   procedure Set_Item_Init_Hook (Men  : in Menu;
                                 Proc : in Menu_Hook_Function);
   --  AKA

   --  ANCHOR(`set_item_term()',`Set_Item_Term_Hook')
   procedure Set_Item_Term_Hook (Men  : in Menu;
                                 Proc : in Menu_Hook_Function);
   --  AKA

   --  ANCHOR(`set_menu_init()',`Set_Menu_Init_Hook')
   procedure Set_Menu_Init_Hook (Men  : in Menu;
                                 Proc : in Menu_Hook_Function);
   --  AKA

   --  ANCHOR(`set_menu_term()',`Set_Menu_Term_Hook')
   procedure Set_Menu_Term_Hook (Men  : in Menu;
                                 Proc : in Menu_Hook_Function);
   --  AKA

   --  ANCHOR(`item_init()',`Get_Item_Init_Hook')
   function Get_Item_Init_Hook (Men : Menu) return Menu_Hook_Function;
   --  AKA

   --  ANCHOR(`item_term()',`Get_Item_Term_Hook')
   function Get_Item_Term_Hook (Men : Menu) return Menu_Hook_Function;
   --  AKA

   --  ANCHOR(`menu_init()',`Get_Menu_Init_Hook')
   function Get_Menu_Init_Hook (Men : Menu) return Menu_Hook_Function;
   --  AKA

   --  ANCHOR(`menu_term()',`Get_Menu_Term_Hook')
   function Get_Menu_Term_Hook (Men : Menu) return Menu_Hook_Function;
   --  AKA

   --  MANPAGE(`menu_items.3x')

   --  ANCHOR(`set_menu_items()',`Redefine')
   procedure Redefine (Men   : in Menu;
                       Items : in Item_Array);
   --  AKA
   --  With a bit more comfort. You don´t need to terminate the Item_Array
   --  with a null entry. This is handled internally in the binding.

   procedure Set_Items (Men   : in Menu;
                        Items : in Item_Array) renames Redefine;

   --  ANCHOR(`menu_items()',`Items')
   function Items (Men : Menu) return Item_Array_Access;
   --  AKA

   --  ANCHOR(`item_count()',`Item_Count')
   function Item_Count (Men : Menu) return Natural;
   --  AKA

   --  MANPAGE(`menu_new.3x')

   --  ANCHOR(`new_menu()',`Create')
   function Create (Items : Item_Array) return Menu;
   --  AKA

   function New_Menu (Items : Item_Array) return Menu renames Create;

   --  ANCHOR(`free_menu()',`Delete')
   procedure Delete (Men : in out Menu);
   --  AKA
   --  Reset Men to Null_Menu

   --  MANPAGE(`menu_new.3x')

   type Driver_Result is (Menu_Ok,
                          Request_Denied,
                          Unknown_Request,
                          No_Match);

   --  ANCHOR(`menu_driver()',`Driver')
   function Driver (Men : Menu;
                    Key : Key_Code) return Driver_Result;
   --  AKA

-------------------------------------------------------------------------------
private
   type Item   is new System.Address;
   type Menu   is new System.Address;

   Null_Item : constant Item := Item (System.Null_Address);
   Null_Menu : constant Menu := Menu (System.Null_Address);

   --  This binding uses the original user pointer mechanism of a menu to store
   --  specific informations about a menu. This wrapper record carries this
   --  specifics and contains a field to maintain a new user pointer. Please
   --  note that you must take this into account if you wan't to use the user
   --  pointer mechanism of a menu created with this binding in low-level C
   --  routines.
   type Ada_User_Wrapper is
      record
         U : System.Address;
         I : Item_Array_Access;
      end record;
   pragma Convention (C, Ada_User_Wrapper);
   type Ada_User_Wrapper_Access is access all Ada_User_Wrapper;
   pragma Controlled (Ada_User_Wrapper_Access);

   Generation_Bit_Order : constant System.Bit_Order := System.M4_BIT_ORDER;
   --  This constant may be different on your system.

end Terminal_Interface.Curses.Menus;
