#ifndef CPLUS_INTERNAL_H
#define CPLUS_INTERNAL_H 1

#include <ncurses_cfg.h>

#ifdef USE_RCS_IDS
#define MODULE_ID(id) static const char Ident[] = id;
#else
#define MODULE_ID(id) /*nothing*/
#endif

#define CTRL(x) ((x) & 0x1f)

#endif
