/*----------------------------------------------------------------------------
//                                                                          --
//                           GNAT ncurses Binding                           --
//                                                                          --
//                                  gen.c                                   --
//                                                                          --
//                                 B O D Y                                  --
//                                                                          --
//  Version 00.92                                                           --
//                                                                          --
//  The ncurses Ada95 binding is copyrighted 1996 by                        --
//  Juergen Pfeifer, Email: Juergen.Pfeifer@T-Online.de                     --
//                                                                          --
//  Permission is hereby granted to reproduce and distribute this           --
//  binding by any means and for any fee, whether alone or as part          --
//  of a larger distribution, in source or in binary form, PROVIDED         --
//  this notice is included with any such distribution, and is not          --
//  removed from any of its header files. Mention of ncurses and the        --
//  author of this binding in any applications linked with it is            --
//  highly appreciated.                                                     --
//                                                                          --
//  This binding comes AS IS with no warranty, implied or expressed.        --
//----------------------------------------------------------------------------
    Version Control
    $Revision: 1.8 $
  --------------------------------------------------------------------------*/
/*
  This program generates various record structures and constants from the
  ncurses header file for the Ada95 packages. Essentially it produces
  Ada95 source on stdout, which is then merged using m4 into a template
  to produce the real source.
  */

#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <time.h>

#include <menu.h>
#include <form.h>

#define RES_NAME "Reserved"

static int little_endian = 0;

typedef struct {
  const char *name;
  unsigned int attr;
} name_attribute_pair;

static int find_pos (char *s, unsigned len, int *low, int *high)
{
  unsigned int i,j; 
  int l = 0;

  *high = -1;
  *low  = 8*len;

  for(i=0; i < len; i++,s++)
    {
      if (*s)
	{
	  for(j=0;j<8*sizeof(char);j++)
	    {
	      if ((( little_endian && ((*s)&0x01)) ||
		   (!little_endian && ((*s)&0x80))) )
		{
		  if (l > *high)
		    *high = l;
		  if (l < *low)
		    *low = l;
		}
	      l++;
	      if (little_endian)
		*s >>= 1;
	      else
		*s <<= 1;
	    }
	}
      else
	l += 8;
    }
  return (*high >= 0 && (*low <= *high)) ? *low : -1;
}

static void gen_reps ( const name_attribute_pair *nap,
		       const char *name,
		       int len)
{
  int i,l,cnt = 0,low,high;
  int width = strlen(RES_NAME);
  int bias = 0;
  unsigned int a;
  unsigned int mask = 0;
  char *suffix;

  assert (nap);

  if (len == sizeof(int)/2)
    {
      bias = little_endian ? 8 * len : 0;
      suffix = " / 2";
    }
  else
    {
      assert(len==sizeof(int));
      suffix = "";
    }

  for (i=0; nap[i].name != (char *)0; i++)
    {
      cnt++;
      l = strlen(nap[i].name);
      if (l>width)
	width = l;
    }
  assert (width > 0);

  printf("   type %s is\n",name);
  printf("      record\n");
  for (i=0; nap[i].name != (char *)0; i++)
    {
      printf("         %-*s : Boolean;\n",width,nap[i].name);
    }  
  if (cnt != 8*len)
    {
      printf("         %-*s : Boolean;\n",width,RES_NAME);
    }
  printf("      end record;\n");
  printf("   pragma Pack (%s);\n",name);
  printf("   pragma Convention (C, %s);\n\n",name);

  printf("   for %s use\n",name);
  printf("      record\n");

  for (i=0; nap[i].name != (char *)0; i++)
    {
      a = nap[i].attr;
      mask |= a;
      l = find_pos( (char *)&a,sizeof(a),&low,&high );
      if (l>=0)
	printf("         %-*s at 0 range %2d .. %2d;\n",width,nap[i].name,low-bias,high-bias);
    }  
  if (cnt != 8*len)
    {
      mask = ~mask;
      assert(mask);
      if (little_endian)
	l = 8*len - 1;
      else
	l = 0;
      printf("         %-*s at 0 range %2d .. %2d;\n",width,RES_NAME,l,l);
    }
  printf("      end record;\n");
  printf("   for %s'Size use Interfaces.C.int'Size%s;\n", name, suffix);
  printf("   --  Please note: this rep. clause is generated and may be\n");
  printf("   --               different on your system.");
}


static void chtype_rep (const char *name, int mask)
{
  int x = -1;
  int t = x & mask;
  int low, high;
  int l = find_pos ((char *)&t, sizeof(t), &low, &high);
  if (l>=0)
    printf("         %-5s at 0 range %2d .. %2d;\n",name,low,high); 
}

static void gen_chtype_rep(const char *name)
{
  printf("   for %s use\n      record\n",name);
  chtype_rep("Ch",A_CHARTEXT);
  chtype_rep("Color",A_COLOR);
  chtype_rep("Attr",(A_ATTRIBUTES&~A_COLOR));
  printf("      end record;\n   for %s'Size use Interfaces.C.int'Size;\n",name);
  printf("      --  Please note: this rep. clause is generated and may be\n");
  printf("      --               different on your system.\n");
}


static void mrep_rep (const char *name, void *rec)
{
  int low, high;
  int l = find_pos((char *)rec, sizeof(MEVENT), &low, &high);
  if (l>=0)
    printf("         %-7s at 0 range %3d .. %3d;\n",name,low,high); 
}


static void gen_mrep_rep(const char *name)
{
  MEVENT x;

  printf("   for %s use\n      record\n",name);

  memset(&x,0,sizeof(x));
  x.id = -1;
  mrep_rep("Id",&x);

  memset(&x,0,sizeof(x));
  x.x = -1;
  mrep_rep("X",&x);

  memset(&x,0,sizeof(x));
  x.y = -1;
  mrep_rep("Y",&x);

  memset(&x,0,sizeof(x));
  x.z = -1;
  mrep_rep("Z",&x);

  memset(&x,0,sizeof(x));
  x.bstate = -1;
  mrep_rep("Bstate",&x);

  printf("      end record;\n");
  printf("      --  Please note: this rep. clause is generated and may be\n");
  printf("      --               different on your system.\n");
}

static void gen_attr_set( const char *name )
{
  static const name_attribute_pair nap[] = {
#ifdef A_STANDOUT
    {"Stand_Out",               A_STANDOUT},
#endif
#ifdef A_UNDERLINE
    {"Under_Line",              A_UNDERLINE},
#endif
#ifdef A_REVERSE
    {"Reverse_Video",           A_REVERSE},
#endif
#ifdef A_BLINK
    {"Blink",                   A_BLINK},
#endif
#ifdef A_DIM
    {"Dim_Character",           A_DIM},
#endif
#ifdef A_BOLD
    {"Bold_Character",          A_BOLD},
#endif
#ifdef A_ALTCHARSET
    {"Alternate_Character_Set", A_ALTCHARSET},
#endif
#ifdef A_INVIS
    {"Invisible_Character",     A_INVIS},
#endif
#ifdef A_PROTECT
    {"Protected_Character",     A_PROTECT},
#endif
#ifdef A_HORIZONTAL
    {"Horizontal",              A_HORIZONTAL},
#endif
#ifdef A_LEFT
    {"Left",                    A_LEFT},
#endif
#ifdef A_LOW
    {"Low",                     A_LOW},
#endif
#ifdef A_RIGHT
    {"Right",                   A_RIGHT},
#endif
#ifdef A_TOP
    {"Top",                     A_TOP},
#endif
#ifdef A_VERTICAL
    {"Vertical",                A_VERTICAL},
#endif
    {(char *)0,                 0}
  };
  gen_reps (nap, name, sizeof(int)/2);
}

static void gen_menu_opt_rep(const char *name)
{
  static const name_attribute_pair nap[] = {
#ifdef O_ONEVALUE
    {"One_Valued", O_ONEVALUE},
#endif
#ifdef O_SHOWDESC
    {"Show_Descriptions", O_SHOWDESC},
#endif
#ifdef O_ROWMAJOR
    {"Row_Major_Order", O_ROWMAJOR},
#endif
#ifdef O_IGNORECASE
    {"Ignore_Case", O_IGNORECASE},
#endif
#ifdef O_SHOWMATCH
    {"Show_Matches", O_SHOWMATCH},
#endif
#ifdef O_NONCYCLIC
    {"Non_Cyclic", O_NONCYCLIC},
#endif
    {(char *)0, 0}
  };
  gen_reps (nap, name, sizeof(int));
}

static void gen_item_opt_rep(const char *name)
{
  static const name_attribute_pair nap[] = {
#ifdef O_SELECTABLE
    {"Selectable", O_SELECTABLE},
#endif
    {(char *)0   , 0}
  };  
  gen_reps (nap, name, sizeof(int));
}

static void gen_form_opt_rep(const char *name)
{
  static const name_attribute_pair nap[] = {
#ifdef O_NL_OVERLOAD
    {"NL_Overload", O_NL_OVERLOAD},
#endif
#ifdef O_BS_OVERLOAD
    {"BS_Overload", O_BS_OVERLOAD},
#endif
    {(char *)0    , 0}
  };
  gen_reps (nap, name, sizeof(int));
}

static void gen_field_opt_rep(const char *name)
{
  static const name_attribute_pair nap[] = {
#ifdef O_VISIBLE
    {"Visible",O_VISIBLE},
#endif
#ifdef O_ACTIVE
    {"Active",O_ACTIVE},
#endif
#ifdef O_PUBLIC
    {"Public",O_PUBLIC},
#endif
#ifdef O_EDIT
    {"Edit",O_EDIT},
#endif
#ifdef O_WRAP
    {"Wrap",O_WRAP},
#endif
#ifdef O_BLANK
    {"Blank",O_BLANK},
#endif
#ifdef O_AUTOSKIP
    {"Auto_Skip",O_AUTOSKIP},
#endif
#ifdef O_NULLOK
    {"Null_Ok",O_NULLOK},
#endif
#ifdef O_PASSOK
    {"Pass_Ok",O_PASSOK},
#endif
#ifdef O_STATIC
    {"Static",O_STATIC},
#endif
    {(char *)0, 0}
  };
  gen_reps (nap, name, sizeof(int));
}

static void keydef(const char *name, const char *old_name, int value, int mode)
{
  if (mode==0)
    printf("   %-30s : constant Special_Key_Code := 8#%3o#;\n",name,value);
  else
    {
      const char *s = old_name; const char *t = name;
      while ( *s && *t && (toupper(*s++) == toupper(*t++)));
      if (*s || *t)
	printf("   %-16s : Special_Key_Code renames %s;\n",old_name,name);
    }
}
  
static void gen_keydefs (int mode)
{
  char buf[16];
  char obuf[16];
  int i;

#ifdef KEY_CODE_YES
  keydef("Key_Code_Yes","KEY_CODE_YES",KEY_CODE_YES,mode);
#endif
#ifdef KEY_MIN
  keydef("Key_Min","KEY_MIN",KEY_MIN,mode);
#endif
#ifdef KEY_BREAK
  keydef("Key_Break","KEY_BREAK",KEY_BREAK,mode);
#endif
#ifdef KEY_DOWN
  keydef("Key_Cursor_Down","KEY_DOWN",KEY_DOWN,mode);
#endif
#ifdef KEY_UP
  keydef("Key_Cursor_Up","KEY_UP",KEY_UP,mode);
#endif
#ifdef KEY_LEFT
  keydef("Key_Cursor_Left","KEY_LEFT",KEY_LEFT,mode);
#endif
#ifdef KEY_RIGHT
  keydef("Key_Cursor_Right","KEY_RIGHT",KEY_RIGHT,mode);
#endif
#ifdef KEY_HOME
  keydef("Key_Home","KEY_HOME",KEY_HOME,mode);
#endif
#ifdef KEY_BACKSPACE
  keydef("Key_Backspace","KEY_BACKSPACE",KEY_BACKSPACE,mode);
#endif
#ifdef KEY_F0
  keydef("Key_F0","KEY_F0",KEY_F0,mode);
#endif
#ifdef KEY_F
  for(i=1;i<=24;i++)
    {
      sprintf(buf ,"Key_F%d",i);
      sprintf(obuf,"KEY_F%d",i);
      keydef(buf,obuf,KEY_F(i),mode);
    }
#endif
#ifdef KEY_DL
  keydef("Key_Delete_Line","KEY_DL",KEY_DL,mode);
#endif
#ifdef KEY_IL
  keydef("Key_Insert_Line","KEY_IL",KEY_IL,mode);
#endif
#ifdef KEY_DC
  keydef("Key_Delete_Char","KEY_DC",KEY_DC,mode);
#endif
#ifdef KEY_IC
  keydef("Key_Insert_Char","KEY_IC",KEY_IC,mode);
#endif
#ifdef KEY_EIC
  keydef("Key_Exit_Insert_Mode","KEY_EIC",KEY_EIC,mode);
#endif
#ifdef KEY_CLEAR
  keydef("Key_Clear_Screen","KEY_CLEAR",KEY_CLEAR,mode);
#endif
#ifdef KEY_EOS
  keydef("Key_Clear_End_Of_Screen","KEY_EOS",KEY_EOS,mode);
#endif
#ifdef KEY_EOL
  keydef("Key_Clear_End_Of_Line","KEY_EOL",KEY_EOL,mode);
#endif
#ifdef KEY_SF
  keydef("Key_Scroll_1_Forward","KEY_SF",KEY_SF,mode);
#endif
#ifdef KEY_SR
  keydef("Key_Scroll_1_Backward","KEY_SR",KEY_SR,mode);
#endif
#ifdef KEY_NPAGE
  keydef("Key_Next_Page","KEY_NPAGE",KEY_NPAGE,mode);
#endif
#ifdef KEY_PPAGE
  keydef("Key_Previous_Page","KEY_PPAGE",KEY_PPAGE,mode);
#endif
#ifdef KEY_STAB
  keydef("Key_Set_Tab","KEY_STAB",KEY_STAB,mode);
#endif
#ifdef KEY_CTAB
  keydef("Key_Clear_Tab","KEY_CTAB",KEY_CTAB,mode);
#endif
#ifdef KEY_CATAB
  keydef("Key_Clear_All_Tabs","KEY_CATAB",KEY_CATAB,mode);
#endif
#ifdef KEY_ENTER
  keydef("Key_Enter_Or_Send","KEY_ENTER",KEY_ENTER,mode);
#endif
#ifdef KEY_SRESET
  keydef("Key_Soft_Reset","KEY_SRESET",KEY_SRESET,mode);
#endif
#ifdef KEY_RESET
  keydef("Key_Reset","KEY_RESET",KEY_RESET,mode);
#endif
#ifdef KEY_PRINT
  keydef("Key_Print","KEY_PRINT",KEY_PRINT,mode);
#endif
#ifdef KEY_LL
  keydef("Key_Bottom","KEY_LL",KEY_LL,mode);
#endif
#ifdef KEY_A1
  keydef("Key_Upper_Left_Of_Keypad","KEY_A1",KEY_A1,mode);
#endif
#ifdef KEY_A3
  keydef("Key_Upper_Right_Of_Keypad","KEY_A3",KEY_A3,mode);
#endif
#ifdef KEY_B2
  keydef("Key_Center_Of_Keypad","KEY_B2",KEY_B2,mode);
#endif
#ifdef KEY_C1
  keydef("Key_Lower_Left_Of_Keypad","KEY_C1",KEY_C1,mode);
#endif
#ifdef KEY_C3
  keydef("Key_Lower_Right_Of_Keypad","KEY_C3",KEY_C3,mode);
#endif
#ifdef KEY_BTAB
  keydef("Key_Back_Tab","KEY_BTAB",KEY_BTAB,mode);
#endif
#ifdef KEY_BEG
  keydef("Key_Beginning","KEY_BEG",KEY_BEG,mode);
#endif
#ifdef KEY_CANCEL
  keydef("Key_Cancel","KEY_CANCEL",KEY_CANCEL,mode);
#endif
#ifdef KEY_CLOSE
  keydef("Key_Close","KEY_CLOSE",KEY_CLOSE,mode);
#endif
#ifdef KEY_COMMAND
  keydef("Key_Command","KEY_COMMAND",KEY_COMMAND,mode);
#endif
#ifdef KEY_COPY
  keydef("Key_Copy","KEY_COPY",KEY_COPY,mode);
#endif
#ifdef KEY_CREATE
  keydef("Key_Create","KEY_CREATE",KEY_CREATE,mode);
#endif
#ifdef KEY_END
  keydef("Key_End","KEY_END",KEY_END,mode);
#endif
#ifdef KEY_EXIT
  keydef("Key_Exit","KEY_EXIT",KEY_EXIT,mode);
#endif
#ifdef KEY_FIND
  keydef("Key_Find","KEY_FIND",KEY_FIND,mode);
#endif
#ifdef KEY_HELP
  keydef("Key_Help","KEY_HELP",KEY_HELP,mode);
#endif
#ifdef KEY_MARK
  keydef("Key_Mark","KEY_MARK",KEY_MARK,mode);
#endif
#ifdef KEY_MESSAGE
  keydef("Key_Message","KEY_MESSAGE",KEY_MESSAGE,mode);
#endif
#ifdef KEY_MOVE
  keydef("Key_Move","KEY_MOVE",KEY_MOVE,mode);
#endif
#ifdef KEY_NEXT
  keydef("Key_Next","KEY_NEXT",KEY_NEXT,mode);
#endif
#ifdef KEY_OPEN
  keydef("Key_Open","KEY_OPEN",KEY_OPEN,mode);
#endif
#ifdef KEY_OPTIONS
  keydef("Key_Options","KEY_OPTIONS",KEY_OPTIONS,mode);
#endif
#ifdef KEY_PREVIOUS
  keydef("Key_Previous","KEY_PREVIOUS",KEY_PREVIOUS,mode);
#endif
#ifdef KEY_REDO
  keydef("Key_Redo","KEY_REDO",KEY_REDO,mode);
#endif
#ifdef KEY_REFERENCE
  keydef("Key_Reference","KEY_REFERENCE",KEY_REFERENCE,mode);
#endif
#ifdef KEY_REFRESH
  keydef("Key_Refresh","KEY_REFRESH",KEY_REFRESH,mode);
#endif
#ifdef KEY_REPLACE
  keydef("Key_Replace","KEY_REPLACE",KEY_REPLACE,mode);
#endif
#ifdef KEY_RESTART
  keydef("Key_Restart","KEY_RESTART",KEY_RESTART,mode);
#endif
#ifdef KEY_RESUME
  keydef("Key_Resume","KEY_RESUME",KEY_RESUME,mode);
#endif
#ifdef KEY_SAVE
  keydef("Key_Save","KEY_SAVE",KEY_SAVE,mode);
#endif
#ifdef KEY_SBEG
  keydef("Key_Shift_Begin","KEY_SBEG",KEY_SBEG,mode);
#endif
#ifdef KEY_SCANCEL
  keydef("Key_Shift_Cancel","KEY_SCANCEL",KEY_SCANCEL,mode);
#endif
#ifdef KEY_SCOMMAND
  keydef("Key_Shift_Command","KEY_SCOMMAND",KEY_SCOMMAND,mode);
#endif
#ifdef KEY_SCOPY
  keydef("Key_Shift_Copy","KEY_SCOPY",KEY_SCOPY,mode);
#endif
#ifdef KEY_SCREATE
  keydef("Key_Shift_Create","KEY_SCREATE",KEY_SCREATE,mode);
#endif
#ifdef KEY_SDC
  keydef("Key_Shift_Delete_Char","KEY_SDC",KEY_SDC,mode);
#endif
#ifdef KEY_SDL
  keydef("Key_Shift_Delete_Line","KEY_SDL",KEY_SDL,mode);
#endif
#ifdef KEY_SELECT
  keydef("Key_Select","KEY_SELECT",KEY_SELECT,mode);
#endif
#ifdef KEY_SEND
  keydef("Key_Shift_End","KEY_SEND",KEY_SEND,mode);
#endif
#ifdef KEY_SEOL
  keydef("Key_Shift_Clear_End_Of_Line","KEY_SEOL",KEY_SEOL,mode);
#endif
#ifdef KEY_SEXIT
  keydef("Key_Shift_Exit","KEY_SEXIT",KEY_SEXIT,mode);
#endif
#ifdef KEY_SFIND
  keydef("Key_Shift_Find","KEY_SFIND",KEY_SFIND,mode);
#endif
#ifdef KEY_SHELP
  keydef("Key_Shift_Help","KEY_SHELP",KEY_SHELP,mode);
#endif
#ifdef KEY_SHOME
  keydef("Key_Shift_Home","KEY_SHOME",KEY_SHOME,mode);
#endif
#ifdef KEY_SIC
  keydef("Key_Shift_Insert_Char","KEY_SIC",KEY_SIC,mode);
#endif
#ifdef KEY_SLEFT
  keydef("Key_Shift_Cursor_Left","KEY_SLEFT",KEY_SLEFT,mode);
#endif
#ifdef KEY_SMESSAGE
  keydef("Key_Shift_Message","KEY_SMESSAGE",KEY_SMESSAGE,mode);
#endif
#ifdef KEY_SMOVE
  keydef("Key_Shift_Move","KEY_SMOVE",KEY_SMOVE,mode);
#endif
#ifdef KEY_SNEXT
  keydef("Key_Shift_Next_Page","KEY_SNEXT",KEY_SNEXT,mode);
#endif
#ifdef KEY_SOPTIONS
  keydef("Key_Shift_Options","KEY_SOPTIONS",KEY_SOPTIONS,mode);
#endif
#ifdef KEY_SPREVIOUS
  keydef("Key_Shift_Previous_Page","KEY_SPREVIOUS",KEY_SPREVIOUS,mode);
#endif
#ifdef KEY_SPRINT
  keydef("Key_Shift_Print","KEY_SPRINT",KEY_SPRINT,mode);
#endif
#ifdef KEY_SREDO
  keydef("Key_Shift_Redo","KEY_SREDO",KEY_SREDO,mode);
#endif
#ifdef KEY_SREPLACE
  keydef("Key_Shift_Replace","KEY_SREPLACE",KEY_SREPLACE,mode);
#endif
#ifdef KEY_SRIGHT
  keydef("Key_Shift_Cursor_Right","KEY_SRIGHT",KEY_SRIGHT,mode);
#endif
#ifdef KEY_SRSUME
  keydef("Key_Shift_Resume","KEY_SRSUME",KEY_SRSUME,mode);
#endif
#ifdef KEY_SSAVE
  keydef("Key_Shift_Save","KEY_SSAVE",KEY_SSAVE,mode);
#endif
#ifdef KEY_SSUSPEND
  keydef("Key_Shift_Suspend","KEY_SSUSPEND",KEY_SSUSPEND,mode);
#endif
#ifdef KEY_SUNDO
  keydef("Key_Shift_Undo","KEY_SUNDO",KEY_SUNDO,mode);
#endif
#ifdef KEY_SUSPEND
  keydef("Key_Suspend","KEY_SUSPEND",KEY_SUSPEND,mode);
#endif
#ifdef KEY_UNDO
  keydef("Key_Undo","KEY_UNDO",KEY_UNDO,mode);
#endif
#ifdef KEY_MOUSE
  keydef("Key_Mouse","KEY_MOUSE",KEY_MOUSE,mode);
#endif  
}

static void acs_def (const char *name, chtype *a)
{
  int c = a - &acs_map[0];
  printf("   %-24s : constant Character := ",name);
  if (isprint(c) && (c!='`'))
    printf("'%c';\n",c);
  else
    printf("Character'Val (%d);\n",c);
}


static void gen_acs (void)
{
#ifdef ACS_ULCORNER
  acs_def("ACS_Upper_Left_Corner",&ACS_ULCORNER);
#endif
#ifdef ACS_LLCORNER
  acs_def("ACS_Lower_Left_Corner",&ACS_LLCORNER);
#endif
#ifdef ACS_URCORNER
  acs_def("ACS_Upper_Right_Corner",&ACS_URCORNER);
#endif
#ifdef ACS_LRCORNER
  acs_def("ACS_Lower_Right_Corner",&ACS_LRCORNER);
#endif
#ifdef ACS_LTEE
  acs_def("ACS_Left_Tee",&ACS_LTEE);
#endif
#ifdef ACS_RTEE
  acs_def("ACS_Right_Tee",&ACS_RTEE);
#endif
#ifdef ACS_BTEE
  acs_def("ACS_Bottom_Tee",&ACS_BTEE);
#endif
#ifdef ACS_TTEE
  acs_def("ACS_Top_Tee",&ACS_TTEE);
#endif
#ifdef ACS_HLINE
  acs_def("ACS_Horizontal_Line",&ACS_HLINE);
#endif
#ifdef ACS_VLINE
  acs_def("ACS_Vertical_Line",&ACS_VLINE);
#endif
#ifdef ACS_PLUS
  acs_def("ACS_Plus_Symbol",&ACS_PLUS);
#endif
#ifdef ACS_S1
  acs_def("ACS_Scan_Line_1",&ACS_S1);
#endif
#ifdef ACS_S9
  acs_def("ACS_Scan_Line_9",&ACS_S9);
#endif
#ifdef ACS_DIAMOND
  acs_def("ACS_Diamond",&ACS_DIAMOND);
#endif
#ifdef ACS_CKBOARD
  acs_def("ACS_Checker_Board",&ACS_CKBOARD);
#endif
#ifdef ACS_DEGREE
  acs_def("ACS_Degree",&ACS_DEGREE);
#endif
#ifdef ACS_PLMINUS
  acs_def("ACS_Plus_Minus",&ACS_PLMINUS);
#endif
#ifdef ACS_BULLET
  acs_def("ACS_Bullet",&ACS_BULLET);
#endif
#ifdef ACS_LARROW
  acs_def("ACS_Left_Arrow",&ACS_LARROW);
#endif
#ifdef ACS_RARROW
  acs_def("ACS_Right_Arrow",&ACS_RARROW);
#endif
#ifdef ACS_DARROW
  acs_def("ACS_Down_Arrow",&ACS_DARROW);
#endif
#ifdef ACS_UARROW
  acs_def("ACS_Up_Arrow",&ACS_UARROW);
#endif
#ifdef ACS_BOARD
  acs_def("ACS_Board_Of_Squares",&ACS_BOARD);
#endif
#ifdef ACS_LANTERN
  acs_def("ACS_Lantern",&ACS_LANTERN);
#endif
#ifdef ACS_BLOCK
  acs_def("ACS_Solid_Block",&ACS_BLOCK);
#endif
#ifdef ACS_S3
  acs_def("ACS_Scan_Line_3",&ACS_S3);
#endif
#ifdef ACS_S7
  acs_def("ACS_Scan_Line_7",&ACS_S7);
#endif
#ifdef ACS_LEQUAL
  acs_def("ACS_Less_Or_Equal",&ACS_LEQUAL);
#endif
#ifdef ACS_GEQUAL
  acs_def("ACS_Greater_Or_Equal",&ACS_GEQUAL);
#endif
#ifdef ACS_PI
  acs_def("ACS_PI",&ACS_PI);
#endif
#ifdef ACS_NEQUAL
  acs_def("ACS_Not_Equal",&ACS_NEQUAL);
#endif
#ifdef ACS_STERLING
  acs_def("ACS_Sterling",&ACS_STERLING);
#endif
}

static void prologue(const char *name)
{
  time_t t = time(NULL);
  printf("--  %s binding, generated at %s",name,ctime(&t));
  printf("--  This module is generated. Please don't change it manually!\n");
  printf("--  Run the generator instead.\n--  |");

  printf("define(`M4_BIT_ORDER',`%s_Order_First')",little_endian ? "Low":"High");
}

static void basedefs (void)
{
  prologue("curses");
#ifndef KEY_MAX
#  define KEY_MAX 0777
#endif
  printf("define(`M4_KEY_MAX',`8#%o#')",KEY_MAX);
#ifndef KEY_MIN
#  define KEY_MIN 0401
#endif
  if (KEY_MIN == 256)
    abort();
  printf("define(`M4_SPECIAL_FIRST',`8#%o#')",KEY_MIN - 1);
}

static void menu_basedefs (void)
{
  prologue("menu");
}

static void form_basedefs (void)
{
  prologue("form");
}

static void mouse_basedefs(void)
{
  prologue("mouse");
}

static void color_def (const char *name, int value)
{
  printf("   %-8s : constant Color_Number := %d;\n",name,value);
}

static void gen_color (void)
{
#ifdef COLOR_BLACK
  color_def ("Black",COLOR_BLACK);
#endif
#ifdef COLOR_RED
  color_def ("Red",COLOR_RED);
#endif
#ifdef COLOR_GREEN
  color_def ("Green",COLOR_GREEN);
#endif
#ifdef COLOR_YELLOW
  color_def ("Yellow",COLOR_YELLOW);
#endif
#ifdef COLOR_BLUE
  color_def ("Blue",COLOR_BLUE);
#endif
#ifdef COLOR_MAGENTA
  color_def ("Magenta",COLOR_MAGENTA);
#endif
#ifdef COLOR_CYAN
  color_def ("Cyan",COLOR_CYAN);
#endif
#ifdef COLOR_WHITE
  color_def ("White",COLOR_WHITE);
#endif
}

static void gen_linkopts (void)
{
   printf("   pragma Linker_Options (\"-lncurses\");\n");
}

static void gen_menu_linkopts (void)
{
   printf("   pragma Linker_Options (\"-lmenu\");\n");
}

static void gen_form_linkopts (void)
{
   printf("   pragma Linker_Options (\"-lform\");\n");
}

static void gen_panel_linkopts (void)
{
   printf("   pragma Linker_Options (\"-lpanel\");\n");
}


int main(int argc, char *argv[])
{
  int x = 0x12345678;
  char *s = (char *)&x;

  if (*s == 0x78)
    little_endian = 1;

  if (argc!=3)
    exit(1);

  switch(argv[1][0])
    {
    case 'B':
      switch(argv[2][0])
	{
	case 'A':
	  gen_attr_set("Character_Attribute_Set");
	  break;
	case 'K':
	  gen_keydefs(0);
	  break;
	case 'B':
	  basedefs();
	  break;
	case 'C':
	  gen_color();
	  break;
	case 'M':
	  gen_acs();
	  break;
	case 'L':
	  gen_linkopts();
	  break;
	case 'O':
	  gen_keydefs(1);
	  break;
	case 'R':
	  gen_chtype_rep("Attributed_Character");
	  break;
	default:
	  break;
	}
      break;
    case 'M':
      switch(argv[2][0])
	{
	case 'R':
	  gen_menu_opt_rep("Menu_Option_Set");
	  break;
	case 'B':
	  menu_basedefs();
	  break;
	case 'L':
	  gen_menu_linkopts();
	  break;
	case 'I':
	  gen_item_opt_rep("Item_Option_Set");
	  break;
	default:
	  break;
	}
      break;
    case 'F':
      switch(argv[2][0])
	{
	case 'R':
	  gen_form_opt_rep("Form_Option_Set");
	  break;
	case 'B':
	  form_basedefs();
	  break;
	case 'L':
	  gen_form_linkopts();
	  break;
	case 'I':
	  gen_field_opt_rep("Field_Option_Set");
	  break;
	default:
	  break;
	}
      break;
    case 'P':
      switch(argv[2][0])
	{
	case 'B':
	  mouse_basedefs();
	  break;
	case 'M':
	  gen_mrep_rep("Mouse_Event");
	  break;
	case 'L':
	  gen_panel_linkopts();
	  break;
	default:
	  break;
	}
	break;
    default:
      break;
    }
  return 0;
}
