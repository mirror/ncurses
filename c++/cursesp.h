// * This makes emacs happy -*-Mode: C++;-*-
#ifndef _CURSESP_H
#define _CURSESP_H

#include <cursesw.h>
#include <etip.h>

extern "C" {
#include <assert.h>
#include <panel.h>
}

class NCursesPanel : public NCursesWindow {

protected:
  PANEL *p;

private:
  // This structure is used for the panel's user data field to link the
  // PANEL* to the C++ object and to provide extra space for a user pointer.
  typedef struct {
    const void*         m_user;      // the pointer for the user's data
    const NCursesPanel* m_back;      // backward pointer to C++ object
    const PANEL*        m_owner;     // the panel itself
  } UserHook;

protected:
  void set_user(const void *user) {
    UserHook* uptr = (UserHook*)::panel_userptr (p);
    assert (uptr && uptr->m_back==this && uptr->m_owner==p);
    uptr->m_user = user;
  }
  
  const void *get_user() {
    UserHook* uptr = (UserHook*)::panel_userptr (p);
    assert (uptr && uptr->m_back==this && uptr->m_owner==p);
    return uptr->m_user;
  }
  
  void OnError (int err) const THROWS((NCursesPanelException)) {
    if (err != OK)
      THROW(new NCursesPanelException (this, err));
  }

public:
  NCursesPanel(int lines   = 0,
	       int cols    = 0,
	       int begin_y = 0,
	       int begin_x = 0);

  virtual ~NCursesPanel();
  
  // basic manipulation
  inline void hide() {
    OnError (::hide_panel(p));
  }

  inline void show() {
    OnError (::show_panel(p));
  }

  inline void top() {
    OnError (::top_panel(p));
  }
  
  inline void bottom() {
    OnError (::bottom_panel(p));
  }
  
  inline void mvpan(int y, int x) {
    OnError (::move_panel(p, y, x));
  }

  inline void mvwin(int y, int x) {
    OnError (::move_panel(p, y, x));
  }
  
  inline bool hidden() const {
    return ::panel_hidden (p);
  }

  static void redraw();       	// redraw all panels
  static void refresh();	// update screen
  
  // decorations
  virtual void frame(const char *title=NULL, const char* btitle=NULL);
  virtual void boldframe(const char *title=NULL, const char* btitle=NULL);
  virtual void label(const char *topLabel, const char *bottomLabel);
  virtual void centertext(int row,const char *label);
  
};


/* We use templates to provide a typesafe mechanism to associate
 * user data with a panel. A NCursesUserPanel<T> is a panel 
 * associated with some user data of type T.
 */
template<class T> class NCursesUserPanel : public NCursesPanel
{
public:
  NCursesUserPanel (int lines   = 0,
		    int cols    = 0,
		    int begin_y = 0,
		    int begin_x = 0)
    : NCursesPanel (lines, cols, begin_y, begin_x) {
  };
  
  NCursesUserPanel (const T* p_UserData,
		    int lines   = 0,
		    int cols    = 0,
		    int begin_y = 0,
		    int begin_x = 0)
    : NCursesPanel (lines, cols, begin_y, begin_x) {
      if (p)
	set_user ((const void *)p_UserData);
  };
  
  virtual ~NCursesUserPanel() {};

  const T* UserData (void) const {
    return (const T*)get_user ();
  };

  virtual void setUserData (const T* p_UserData) {
    if (p)
      set_user ((const void *)p_UserData);
  }
};

#endif // _CURSESP_H
