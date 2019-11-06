#ifndef RECUMARK_HXX
#define RECUMARK_HXX

#include "sexpr.hxx"

namespace escheme
{

inline void setmark( SEXPR n ) { n->recu = 1; }
inline void resetmark( SEXPR n ) { n->recu = 0; }
inline int markedp( SEXPR n ) { return n->recu; }

class RECURSIVE_MARKER
{
   SEXPR node;
public:
   explicit RECURSIVE_MARKER( SEXPR n ) : node(n) { setmark(node); }
   ~RECURSIVE_MARKER() { resetmark(node); }
};

}

#endif
