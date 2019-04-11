#ifndef RECUMARK_HXX
#define RECUMARK_HXX

#define setmark(n) ((n)->recu = 1)
#define resetmark(n) ((n)->recu = 0)
#define markedp(n) ((n)->recu)

class RECURSIVE_MARKER
{
   SEXPR node;
public:
   explicit RECURSIVE_MARKER( SEXPR n ) : node(n) { setmark(node); }
   ~RECURSIVE_MARKER() { resetmark(node); }
}; 

#endif
