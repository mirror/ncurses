// * This makes emacs happy -*-Mode: C++;-*-
/*-----------------------------------------------------------------------------+
|            The ncurses menu C++ binding is Copyright (C) 1997                |
|             by Juergen Pfeifer <Juergen.Pfeifer@T-Online.de>                 |
|                          All Rights Reserved.                                |
|                                                                              |
| Permission to use, copy, modify, and distribute this software and its        |
| documentation for any purpose and without fee is hereby granted, provided    |
| that the above copyright notice appear in all copies and that both that      |
| copyright notice and this permission notice appear in supporting             |
| documentation, and that the name of the above listed copyright holder(s) not |
| be used in advertising or publicity pertaining to distribution of the        |
| software without specific, written prior permission.                         | 
|                                                                              |
| THE ABOVE LISTED COPYRIGHT HOLDER(S) DISCLAIM ALL WARRANTIES WITH REGARD TO  |
| THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FIT-  |
| NESS, IN NO EVENT SHALL THE ABOVE LISTED COPYRIGHT HOLDER(S) BE LIABLE FOR   |
| ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RE- |
| SULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, |
| NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH    |
| THE USE OR PERFORMANCE OF THIS SOFTWARE.                                     |
+-----------------------------------------------------------------------------*/
#ifndef _CURSESM_H
#define _CURSESM_H

#include <assert.h>
#include <cursesp.h>
#include <etip.h>

extern "C" {
#  include <menu.h>
}

// This wraps the ITEM type of <menu.h>
class NCursesMenuItem 
{
  friend class NCursesMenu;

protected:
  ITEM *item;

  void OnError (int err) const THROWS(NCursesMenuException)
  {
    if (err != E_OK)
      THROW(new NCursesMenuException (err));
  }

public:
  // Create an item. If you pass both parameters as NULL, a delimiter
  // item is constructed which can be used to terminate a list of
  // NCursesMenu objects.
  NCursesMenuItem (const char* p_name     = NULL,
		   const char* p_descript = NULL )
  { 
    item = p_name ? ::new_item (p_name, p_descript) : (ITEM*)0; 
    if (p_name && !item)
      OnError (E_SYSTEM_ERROR);
  }
  
  // Release the items memory
  virtual ~NCursesMenuItem ()
  {
    if (item)
      ::free_item (item);
  }

  // Name of the item
  inline const char* name () const
  {
    return ::item_name (item);
  }

  // Description of the item
  inline const char* description () const
  {
    return ::item_description (item);
  }

  // index of the item in an item array (or -1)
  inline int index (void) const
  {
    return ::item_index (item);
  }

  // switch on an item's option
  inline void options_on (Item_Options options)
  {
    OnError (::item_opts_on (item, options));
  }

  // switch off an item's option
  inline void options_off (Item_Options options)
  {
    OnError (::item_opts_off (item, options));
  }

  // return the item's options
  inline Item_Options options () const
  {
    return ::item_opts (item);
  }

  // set the item's options
  inline void set_options (Item_Options options)
  {
    OnError (::set_item_opts (item, options));
  }

  // set/reset the item's selection state
  inline void set_value (bool f)
  {
    OnError (::set_item_value (item,f));
  }

  // return the item's selection state
  inline bool value () const
  {
    return ::item_value (item);
  }
  
  // return visibility of the item
  inline bool visible () const
  {
    return ::item_visible (item);
  }

  // perform an action associated with this item; you may use this in an
  // user supplied driver for a menu; you may derive from this class and
  // overload action() to supply items with different actions.
  // If an action returns true, the menu will be exited.
  virtual bool action() {
    return FALSE;
  };
};

// Prototype for an items callback function.
typedef bool ITEMCALLBACK(NCursesMenuItem&);

// If you don't like to create a child class for individual items to
// overload action(), you may use this class and provide a callback
// function pointer for items.
class NCursesMenuCallbackItem : public NCursesMenuItem
{
private:
  const ITEMCALLBACK* p_fct;

public:
  NCursesMenuCallbackItem(const ITEMCALLBACK* fct = NULL,
			  const char* p_name      = NULL,
			  const char* p_descript  = NULL )
    : NCursesMenuItem (p_name, p_descript),
      p_fct (fct) {
  }
  
  virtual ~NCursesMenuCallbackItem() {
  };

  bool action() {
    if (p_fct)
      return p_fct (*this);
    else
      return FALSE;
  }
};


class NCursesMenu : public NCursesPanel {
protected:
  MENU *menu;

private:
  // Keep book of the number of instantiated C++ menus.
  static unsigned long total_count;

  NCursesWindow* sub;   // the subwindow object
  bool b_sub_owner;     // is this our own subwindow?
  bool b_framed;        // has the menu a border?

  NCursesMenuItem** my_items; // The array of items for this menu

  // This structure is used for the menu's user data field to link the
  // MENU* to the C++ object and to provide extra space for a user pointer.
  typedef struct {
    const void*        m_user;      // the pointer for the user's data
    const NCursesMenu* m_back;      // backward pointer to C++ object
    const MENU*        m_owner;
  } UserHook;

  // Get the backward pointer to the C++ object from a MENU
  static inline NCursesMenu* getHook(const MENU *m)
  {
    UserHook* hook = (UserHook*)::menu_userptr(m);
    assert(hook && hook->m_owner==m);
    return (NCursesMenu*)(hook->m_back);
  }

  // This are the built-in hook functions in this C++ binding. In C++ we use
  // virtual member functions (see below On_..._Init and On_..._Termination)
  // to provide this functionality in an object oriented manner.
  static void mnu_init(MENU *);
  static void mnu_term(MENU *);
  static void itm_init(MENU *);
  static void itm_term(MENU *);
  
  // Calculate ITEM* array for the menu
  ITEM** mapItems(NCursesMenuItem* nitems[]);
  
protected:
  // internal routines 
  void set_user(const void *user)
  {
    UserHook* uptr = (UserHook*)::menu_userptr (menu);
    assert (uptr && uptr->m_back==this && uptr->m_owner==menu);
    uptr->m_user = user;
  }

  const void *get_user()
  {
    UserHook* uptr = (UserHook*)::menu_userptr (menu);
    assert (uptr && uptr->m_back==this && uptr->m_owner==menu);
    return uptr->m_user;
  }


  void InitMenu (NCursesMenuItem* menu[], bool with_frame=FALSE);

  void OnError (int err) const THROWS(NCursesMenuException)
  {
    if (err != E_OK)
      THROW(new NCursesMenuException (this, err));
  }
  
  // this wraps the menu_driver call.
  virtual int driver (int c) ;

public:
  // make a full window size menu
  NCursesMenu (NCursesMenuItem* menu[]);

  // make a menu with a window of this size.
  NCursesMenu (NCursesMenuItem* menu[], 
	       int  lines, 
	       int  cols, 
	       int  begin_y, 
	       int  begin_x,
	       bool with_frame=FALSE);
  
  virtual ~NCursesMenu ();

  // Retrieve the menus subwindow
  inline NCursesWindow& subWindow() const {
    assert(sub!=NULL);
    return *sub;
  }

  // Set the menus subwindow
  void setSubWindow(NCursesWindow& sub);

  // Set these items for the menu
  void setItems(NCursesMenuItem* nitems[]);

  // Remove the menu from the screen
  inline void unpost (void)
  { 
    OnError (::unpost_menu (menu)); 
  }
  
  // Post the menu to the screen if flag is true, unpost it otherwise
  inline void post(bool flag = TRUE)
  {
    flag ? OnError (::post_menu(menu)) : OnError (::unpost_menu (menu)); 
  }

  // Get the numer of rows and columns for this menu
  inline void scale (int& mrows, int& mcols) const 
  {
    OnError (::scale_menu (menu, &mrows, &mcols));
  }

  // Set the format of this menu
  inline void set_format(int mrows, int mcols)
  {
    OnError (::set_menu_format(menu, mrows, mcols));
  }
  
  // Get the format of this menu
  void menu_format(int& rows,int& cols)
  { 
    ::menu_format(menu,&rows,&cols); 
  }
  
  // item things
  NCursesMenuItem* items() const
  {
    return *my_items; 
  }

  // Get the number of items in this menu
  int count() const
  {
    return ::item_count(menu); 
  }

  // Get the current item (i.e. the one the cursor is located)
  NCursesMenuItem& current_item() const
  {
    return *(my_items[::item_index(::current_item(menu))]);
  }
  
  // Get the marker string
  inline const char* mark() const
  {
    return ::menu_mark(menu);
  }

  // Set the marker string
  inline void set_mark(const char *mark)
  {
    OnError (::set_menu_mark (menu, mark));
  }

  // Get the name of the request code c
  inline static const char* request_name(int c)
  {
    return ::menu_request_name(c);
  }

  // Get the current pattern
  inline char* pattern() const
  {
    return ::menu_pattern(menu);
  }

  // true if there is a pattern match, false otherwise.
  bool set_pattern (const char *pat);

  // set the default attributes for the menu
  // i.e. set fore, back and grey attribute
  virtual void setDefaultAttributes();

  // Get the menus background attributes
  chtype back() const
  {
    return ::menu_back(menu);
  }

  // Get the menus foreground attributes
  chtype fore() const
  {
    return ::menu_fore(menu);
  }

  // Get the menus grey attributes (used for unselectable items)
  chtype grey() const
  {
    return ::menu_grey(menu);
  }

  // Set the menus background attributes
  chtype set_background(chtype a)
  {
    return ::set_menu_back(menu,a);
  }

  // Set the menus foreground attributes
  chtype foreground(chtype a)
  {
    return ::set_menu_fore(menu,a);
  }

  // Set the menus grey attributes (used for unselectable items)
  chtype set_grey(chtype a)
  {
    return ::set_menu_grey(menu,a);
  }

  inline void options_on (Menu_Options opts)
  {
    OnError (::menu_opts_on (menu,opts));
  }

  inline void options_off(Menu_Options opts)
  {
    OnError (::menu_opts_off(menu,opts));
  }

  inline Menu_Options options() const {
    return ::menu_opts(menu);
  }

  inline void set_options (Menu_Options opts)
  {
    OnError (::set_menu_opts (menu,opts));
  }

  inline int pad() const
  {
    return ::menu_pad(menu);
  }

  inline void set_pad (int padch)
  {
    OnError (::set_menu_pad (menu, padch));
  }

  // Position the cursor to the current item
  inline void position_cursor () const
  {
    OnError (::pos_menu_cursor (menu));
  }

  // Set the current item
  inline void set_current(NCursesMenuItem& I)
  {
    OnError (::set_current_item(menu, I.item));
  }

  // Get the current top row of the menu
  inline int top_row (void) const
  {
    return ::top_row (menu);
  }

  // Set the current top row of the menu
  inline void set_top_row (int row)
  {
    OnError (::set_top_row (menu, row));
  }

  // spacing control
  // Set the spacing for the menu
  inline void setSpacing(int spc_description,
			 int spc_rows,
			 int spc_columns) {
    OnError(::set_menu_spacing(menu,
			       spc_description,
			       spc_rows,
			       spc_columns));
  }

  // Get the spacing info for the menu
  inline void Spacing(int& spc_description,
		      int& spc_rows,
		      int& spc_columns) const {
    OnError(::menu_spacing(menu,
			   &spc_description,
			   &spc_rows,
			   &spc_columns));
  }

  // Decorations
  void frame(const char *title=NULL, const char* btitle=NULL) {
    if (b_framed)
      NCursesPanel::frame(title,btitle);
    else
      OnError(E_SYSTEM_ERROR);
  }

  void boldframe(const char *title=NULL, const char* btitle=NULL) {
    if (b_framed)
      NCursesPanel::boldframe(title,btitle);
    else
      OnError(E_SYSTEM_ERROR);
  }

  void label(const char *topLabel, const char *bottomLabel) {
    if (b_framed)
      NCursesPanel::label(topLabel,bottomLabel);
    else
      OnError(E_SYSTEM_ERROR);
  }

  // -----
  // Hooks
  // -----

  // Called after the menu gets repositioned in its window.
  // This is especially true if the menu is posted.
  virtual void On_Menu_Init()
  {
  }

  // Called before the menu gets repositioned in its window.
  // This is especially true if the menu is unposted.
  virtual void On_Menu_Termination()
  {
  }

  // Called after the item became the current item
  virtual void On_Item_Init(NCursesMenuItem& item)
  {
  }

  // Called before this item is left as current item.
  virtual void On_Item_Termination(NCursesMenuItem& item)
  {
  }

  // Provide a default key virtualization. Translate the keyboard
  // code c into a menu request code.
  // The default implementation provides a hopefully straightforward
  // mapping for the most common keystrokes and menu requests.
  virtual int virtualize(int c);


  // Operators
  NCursesMenuItem& operator[](int i) const
  {
    if ( (i < 0) || (i >= ::item_count (menu)) )
      OnError (E_BAD_ARGUMENT);
    return *(my_items[i]);
  }

  // Perform the menu's operation
  // Return the item where you left the selection mark.
  virtual NCursesMenuItem& operator()(void);

  // --------------------
  // Exception handlers
  // Called by operator()
  // --------------------

  // Called if the request is denied
  virtual void On_Request_Denied(int c) const {
    beep();
  }
  
  // Called if the item is not selectable
  virtual void On_Not_Selectable(int c) const {
    beep();
  }

  // Called if pattern doesn't match
  virtual void On_No_Match(int c) const {
    beep();
  }

  // Called if the command is unknown
  virtual void On_Unknown_Command(int c) const {
    beep();
  }
};


// This is the typical C++ typesafe way to allow to attach
// user data to an item of a menu. Its assumed that the user
// data belongs to some class T. Use T as template argument
// to create a UserItem.
template<class T> class NCursesUserItem : public NCursesMenuItem
{
public:
  NCursesUserItem (const char* p_name     = NULL,
		   const char* p_descript = NULL )
    : NCursesMenuItem (p_name, p_descript) 
  {};

  NCursesUserItem (const T*    p_UserData,
		   const char* p_name,
		   const char* p_descript = NULL )
    : NCursesMenuItem (p_name, p_descript)
  {
    if (item)
      OnError (::set_item_userptr (item, (const void *)p_UserData));
  };

  virtual ~NCursesUserItem() {};

  const T* UserData (void) const
  {
    return (const T*)::item_userptr (item);
  };

  virtual void setUserData(const T* p_UserData) {
    if (item)
      OnError (::set_item_userptr (item, (const void *)p_UserData));
  }
};

// The same mechanism is used to attach user data to a menu
template<class T> class NCursesUserMenu : public NCursesMenu
{
public:
  NCursesUserMenu (NCursesMenuItem menu[])
    : NCursesMenu (menu)
   {};

  NCursesUserMenu (const T* p_UserData,
		   NCursesMenuItem menu[])
    : NCursesMenu (menu)
  {
    if (m)
      set_user ((const void *)p_UserData);
  };

  NCursesUserMenu (const T* p_UserData,
		   NCursesMenuItem menu[],
		   int lines, 
		   int cols, 
		   int begin_y, 
		   int begin_x,
		   bool with_frame=FALSE)
    : NCursesMenu (menu, lines, cols, begin_y, begin_x, with_frame)
  {
    if (m)
      set_user ((const void *)p_UserData);
  };  

  virtual ~NCursesUserMenu() {};

  const T* UserData (void) const
  {
    return (const T*)get_user ();
  };

  virtual void setUserData (const T* p_UserData) {
    if (m)
      set_user ((const void *)p_UserData);
  }
};

#endif // _CURSESM_H
