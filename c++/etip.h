#ifndef _ETIP_H
#define _ETIP_H

#ifdef __GNUG__
#if HAVE_TYPEINFO
#  include <typeinfo>
#endif
#endif

#include <eti.h>

// Forward Declarations
class NCursesPanel;
class NCursesMenu;
class NCursesForm;

class NCursesException
{
public:
  int errorno;
  const char *message;

  NCursesException (const char* msg, int err)
    : message(msg), errorno (err)
    {};

  NCursesException (const char* msg)
    : message(msg), errorno (E_SYSTEM_ERROR)
    {};

  virtual const char *classname() const {
    return "NCursesWindow";
  }
};

class NCursesPanelException : public NCursesException
{
public:
  const NCursesPanel* p;

  NCursesPanelException (const char *msg, int err) : 
    NCursesException (msg, err),
    p ((NCursesPanel*)0)
    {};

  NCursesPanelException (const NCursesPanel* panel,
			 const char *msg,
			 int err) : 
    NCursesException (msg, err),
    p (panel)
    {};

  NCursesPanelException (int err) : 
    NCursesException ("panel library error", err),
    p ((NCursesPanel*)0)
    {};

  NCursesPanelException (const NCursesPanel* panel,
			 int err) : 
    NCursesException ("panel library error", err),
    p (panel)
    {};

  virtual const char *classname() const {
    return "NCursesPanel";
  }

};

class NCursesMenuException : public NCursesException
{
public:
  const NCursesMenu* m;

  NCursesMenuException (const char *msg, int err) : 
    NCursesException (msg, err),
    m ((NCursesMenu *)0)
    {};

  NCursesMenuException (const NCursesMenu* menu,
			const char *msg,
			int err) : 
    NCursesException (msg, err),
    m (menu)
    {};

  NCursesMenuException (int err) : 
    NCursesException ("menu library error", err),
    m ((NCursesMenu *)0)
    {};

  NCursesMenuException (const NCursesMenu* menu,
			int err) : 
    NCursesException ("menu library error", err),
    m (menu)
    {};

  virtual const char *classname() const {
    return "NCursesMenu";
  }

};

class NCursesFormException : public NCursesException
{
public:
  const NCursesForm* f;

  NCursesFormException (const char *msg, int err) : 
    NCursesException (msg, err),
    f ((NCursesForm*)0)
    {};

  NCursesFormException (const NCursesForm* form,
			const char *msg,
			int err) : 
    NCursesException (msg, err),
    f (form)
    {};

  NCursesFormException (int err) : 
    NCursesException ("form library error", err),
    f ((NCursesForm*)0)
    {};

  NCursesFormException (const NCursesForm* form,
			int err) : 
    NCursesException ("form library error", err),
    f (form)
    {};

  virtual const char *classname() const {
    return "NCursesForm";
  }

};

inline void THROW(const NCursesException *e) {
#if defined(__GNUG__)
  (*lib_error_handler)(e?e->classname():"",e?e->message:"");
#else  // #elif defined(__SUNPRO_CC)
  genericerror(1, ((e != 0) ? (char *)(e->message) : ""));
#endif     
}

#define THROWS(s)
 
#endif
