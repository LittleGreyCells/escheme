/* ipcsoc.cxx - sockets */

#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <unistd.h>
#include <cerrno>


// sockets
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <strings.h>      /* bzero */

//
// Change History
//
// 9/21/2012 -- Added support for UDP sockets.
// 4/26/2013 -- Added support for host addresses.
// 8/07/2014 -- Adapted to escheme
//

const  int sa_limit = 100;
static int sa_next = 1;

sockaddr_in sa_database[sa_limit];

static int allocate_sa()
{
   if ( sa_next >= sa_limit )
      return 0;
   else
   {
      const int sa = sa_next;
      sa_next += 1;
      return sa;
   }
}

static int addr_in_range( int addr )
{
   return (0 <= addr) && (addr < sa_limit);
}

//
// Sockets
//

static void soc_create_address( sockaddr_in& addr, const char* host_addr, int port )
{
   bzero( reinterpret_cast<char*>(&addr), sizeof(addr) );

   addr.sin_family      = AF_INET;
   addr.sin_addr.s_addr = inet_addr( host_addr );
   addr.sin_port        = htons( port );
}

//
//   TCP/connection-oriented socket function--accept.
//

static int soc_create_tcp( int server_flag = 0 )
{
   // CREATE TCP SOCKET
   const int fd = ::socket(AF_INET, SOCK_STREAM, 0);

   if (fd < 0)
   {
      perror( "soc_create_tcp: cannot open stream socket" );
      return fd;
   }

   if ( server_flag != 0 )
   {
      const int on = 1;

      if (::setsockopt( fd, SOL_SOCKET, SO_REUSEADDR, reinterpret_cast<const char*>(&on), sizeof(on) ) )
      {
	 perror( "soc_create_tcp: setsockopt failed" );
	 return -1;
      }
   }

   return fd;
}

static int soc_listen( int sockfd, int count )
{
   // LISTEN
   return ::listen( sockfd, count );
}

static int soc_accept( int fd )
{
   sockaddr_in cli_addr;
   socklen_t clilen = sizeof(cli_addr);

   // ACCEPT
   const int newfd = ::accept( fd, reinterpret_cast<sockaddr*>(&cli_addr), &clilen );

   if (newfd < 0)
   {
      perror( "soc_accept: accept error" );
      close(fd);  // why?
      return newfd;
   }

   close(fd);  // why?

   return newfd;
}

//
// Generic socket interface
//

static int soc_bind( int fd, const char* host_addr, int port )
{
   sockaddr_in addr;

   soc_create_address( addr, host_addr, port );

   // BIND
   if ( ::bind( fd, reinterpret_cast<sockaddr*>(&addr), sizeof(addr)) < 0 )
   {
      perror( "soc_bind: cannot bind local address" );
      return 0;
   }

   return fd;
}

static int soc_bind_address( int fd, sockaddr_in& addr )
{
   // BIND
   if ( ::bind( fd, reinterpret_cast<sockaddr*>(&addr), sizeof(sockaddr_in)) < 0 )
   {
      perror( "soc_bind_address: cannot bind local address" );
      return 0;
   }

   return fd;
}

//
//   Generic connect
//

static int soc_connect( int fd, const char* server_host_addr, int server_port, int numtries )
{
   sockaddr_in serv_addr;

   soc_create_address( serv_addr, server_host_addr, server_port );

   try_again:

   // CONNECT
   if ( ::connect( fd, reinterpret_cast<sockaddr*>(&serv_addr), sizeof(serv_addr)) < 0 )
   {
      if (errno == ECONNREFUSED)
      {
	 if (numtries == 0)
	 {
	    // try forever
	    goto try_again;
	 }
	 else if (numtries > 1)
	 {
	    // try again
	    --numtries;
	    goto try_again;
	 }
	 else
	 {
	    // we were asked to try once
	 }
      }

      perror( "soc_connect: cannot connect to server" );
      return 0;
   }

   return fd;
}

//
// UDP connectionless socket interface
//

static int soc_create_udp()
{
   // CREATE UDP SOCKET
   const int fd = ::socket(AF_INET, SOCK_DGRAM, 0);

   if (fd < 0)
   {
      perror( "soc_create_udp: cannot create datagram socket" );
      return fd;
   }

   return fd;
}

////////////////////////////////////////////////
//
// EScheme Bindings
//
////////////////////////////////////////////////

#include "sexpr.hxx"
#include "error.hxx"
#include "memory.hxx"
#include "regstack.hxx"
#include "argstack.hxx"

#include "ipcsoc.hxx"

inline void push(SEXPR s) { regstack.push(s); }
inline SEXPR pop() { return regstack.pop(); }
inline SEXPR& top() { return regstack.top(); }

//
// Socket Bindings
//

const int MaxReadBufferSize = 5000;
static unsigned char read_buffer[MaxReadBufferSize];

const int MaxWriteBufferSize = 5000;
static unsigned char write_buffer[MaxReadBufferSize];

SEXPR IPCSOC::read()
{
   //
   // syntax: (SOCKET-READ <sockfd> [<numbytes>]) -> <byte-vector>
   //
   ArgstackIterator iter;
   const int fd       = getfixnum(guard(iter.getarg(), fixnump));
   const int numbytes = iter.more() ? static_cast<int>(getfixnum(guard(iter.getarg(), fixnump))) : 0;
   iter.done();

   int n = 0;

   if ( numbytes > 0 )
   {
      // READ the requested number of bytes
      n = ::read( fd, reinterpret_cast<char*>(read_buffer), numbytes );

      if ( n != numbytes )
	 ERROR::severe( "read: did not read required bytes; actual: ", MEMORY::fixnum(static_cast<FIXNUM>(n)) );
   }
   else
   {
      // READ what's available
      n = ::read( fd, reinterpret_cast<char*>(read_buffer), MaxReadBufferSize );

      if ( n < 0 )
	 ERROR::severe( "read: error ", MEMORY::fixnum(n) );
   }

   SEXPR v = MEMORY::byte_vector( n );

   push( v ); // protect from GC

   for (int i = 0; i < n; ++i)
      bvecset( v, i, read_buffer[i] );

   return pop();
}

SEXPR IPCSOC::write()
{
   //
   // syntax: (SOCKET-WRITE <sockfd> <byte-vector>) -> <fixnum>
   //
   ArgstackIterator iter;
   const int fd  = static_cast<int>(getfixnum(guard(iter.getarg(), fixnump)));
   const SEXPR v = guard(iter.getlast(), bvecp);

   const int numbytes = getbveclength(v);

   if ( numbytes > MaxWriteBufferSize )
      ERROR::severe( "write: write buffer buffer too small.", MEMORY::fixnum(static_cast<FIXNUM>(numbytes)) );

   for (int i = 0; i < numbytes; ++i)
      write_buffer[i] = bvecref(v, i);

   // WRITE
   const int n = ::write( fd, reinterpret_cast<char*>(write_buffer), numbytes );

   if ( n != numbytes )
      ERROR::severe( "write: did not write required bytes; actual", MEMORY::fixnum(static_cast<FIXNUM>(n)) );

   return null;
}

//
// [Start unconnected i/o]
//

SEXPR IPCSOC::recvfrom()
{
   //
   // syntax: (SOCKET-RECVFROM <sockfd> <numbytes> <from-addr>) -> <byte-vector>
   //
   //    (Note: The <from-addr> object will be overwritten.)
   //
   ArgstackIterator iter;
   const int fd       = static_cast<int>(getfixnum(guard(iter.getarg(), fixnump)));
   const int numbytes = static_cast<int>(getfixnum(guard(iter.getarg(), fixnump)));
   const int from_sa  = static_cast<int>(getfixnum(guard(iter.getlast(), fixnump)));

   if ( numbytes > MaxReadBufferSize )
      ERROR::severe( "recvfrom: read buffer buffer too small.", MEMORY::fixnum(static_cast<FIXNUM>(numbytes)) );
   else if ( !addr_in_range( from_sa ) )
      ERROR::severe( "recvfrom: <from-addr> not in range.", MEMORY::fixnum(static_cast<FIXNUM>(from_sa)) );

   // RECVFROM
   socklen_t from_len = sizeof(sockaddr_in);

   const int n = ::recvfrom( fd,
			     reinterpret_cast<char*>(read_buffer),
			     numbytes,
			     MSG_WAITALL,
			     reinterpret_cast<sockaddr*>(&sa_database[from_sa]),
			     &from_len );  
   if ( n != numbytes )
      ERROR::severe( "recvfrom: did not read required bytes; actual: ", MEMORY::fixnum(static_cast<FIXNUM>(n)) );

   SEXPR v = MEMORY::byte_vector( n );

   push( v ); // protect from GC

   for (int i = 0; i < numbytes; ++i)
      bvecset( v, i, read_buffer[i] );

   return pop();
}

SEXPR IPCSOC::recv()
{
   //
   // syntax: (SOCKET-RECV <sockfd> <numbytes>) -> <byte-vector>
   //
   ArgstackIterator iter;
   const int fd       = static_cast<int>(getfixnum(guard(iter.getarg(), fixnump)));
   const int numbytes = static_cast<int>(getfixnum(guard(iter.getlast(), fixnump)));

   if ( numbytes > MaxReadBufferSize )
      ERROR::severe( "recvfrom: read buffer buffer too small.", MEMORY::fixnum(static_cast<FIXNUM>(numbytes)) );

   // RECV
   const int n = ::recv( fd, reinterpret_cast<char*>(read_buffer), numbytes, MSG_WAITALL );
  
   if ( n != numbytes )
      ERROR::severe( "recv: did not read required bytes; actual: ", MEMORY::fixnum(static_cast<FIXNUM>(n)) );

   const SEXPR v = MEMORY::byte_vector( n );

   push( v ); // protect from GC

   for (int i = 0; i < numbytes; ++i)
      bvecset( v, i, read_buffer[i] );

   return pop();
}

SEXPR IPCSOC::sendto()
{
   //
   // syntax: (SOCKET-SENDTO <sockfd> <byte-vector> <to-address>) -> <fixnum>
   //
   ArgstackIterator iter;
   const int fd    = static_cast<int>(getfixnum(guard(iter.getarg(), fixnump)));
   const SEXPR v   = guard(iter.getarg(), vectorp);
   const int to_sa = static_cast<int>(getfixnum(guard(iter.getlast(), fixnump)));

   const int numbytes = getbveclength(v);

   if ( numbytes > MaxWriteBufferSize )
      ERROR::severe( "sendto: write buffer buffer too small.", MEMORY::fixnum(static_cast<FIXNUM>(numbytes)) );

   for (int i = 0; i < numbytes; ++i)
      write_buffer[i] = bvecref(v, i);

   // SENDTO
   const int n = ::sendto( fd,
			   reinterpret_cast<char*>(write_buffer),
			   numbytes,
			   0,
			   reinterpret_cast<sockaddr*>(&sa_database[to_sa]),
			   sizeof(sockaddr_in) );

   if ( n != numbytes )
      ERROR::severe( "sendto: did not write required bytes; actual", MEMORY::fixnum(static_cast<FIXNUM>(n)) );

   return null;
}

//
// [End unconnected i/o]
//

SEXPR IPCSOC::create_tcp()
{
   //
   // syntax: (SOCKET-CREATE-TCP [<server-flag>]) -> <sockfd>
   //
   ArgstackIterator iter;
   const int server_flag = iter.more() ? static_cast<int>(getfixnum(guard(iter.getlast(), fixnump))) : 0;
   iter.done();

   return MEMORY::fixnum( soc_create_tcp( server_flag ) );
}

SEXPR IPCSOC::listen()
{
   //
   // syntax: (SOCKET-LISTEN <sockfd> [<backlog>]) -> <result>
   //
   ArgstackIterator iter;
   const int fd      = static_cast<int>(getfixnum(guard(iter.getarg(), fixnump)));
   const int backlog = iter.more() ? static_cast<int>(getfixnum(guard(iter.getarg(), fixnump))) : 5;
   iter.done();

   // LISTEN
   const int result = soc_listen( fd, backlog);

   return MEMORY::fixnum( result );
}

SEXPR IPCSOC::accept()
{
   //
   // syntax: (SOCKET-ACCEPT <sockfd>) -> <sockfd>
   //
   ArgstackIterator iter;
   const int fd = static_cast<int>(getfixnum(guard(iter.getlast(), fixnump)));

   // ACCEPT
   const int socket_fd = soc_accept( fd );

   // create and return a <socket> object.
   return MEMORY::fixnum(socket_fd);
}

//
// connect is weird because of backward compatibility.
//

SEXPR IPCSOC::connect()
{
   //
   // syntax: (SOCKET-CONNECT <fd> <server-host-addr> <server-port> [<numtries>=1] ) -> <sockfd>
   //
   ArgstackIterator iter;

   const int fd                 = static_cast<int>(getfixnum(guard(iter.getarg(), fixnump)));
   const char* server_host_addr = getstringdata(guard(iter.getarg(), stringp));
   const int server_port        = static_cast<int>(getfixnum(guard(iter.getarg(), fixnump)));
   const int numtries           = iter.more() ? static_cast<int>(getfixnum(guard(iter.getarg(), fixnump))) : 1;

   iter.done();

   // CONNECT
   const int sockfd = soc_connect( fd, server_host_addr, server_port, numtries );

   return MEMORY::fixnum(sockfd);
}

SEXPR IPCSOC::disconnect()
{
   //
   // syntax: (SOCKET-DISCONNECT <socket>) -> <fixnum>
   //
   ArgstackIterator iter;
   const int socket_fd = static_cast<int>(getfixnum(guard(iter.getlast(), fixnump)));

   // DISCONNECT/CLOSE
   const int result = ::close( socket_fd );

   return MEMORY::fixnum(result);
}

SEXPR IPCSOC::close()
{
   //
   // syntax: (SOCKET-CLOSE <socket>) -> <fixnum>
   //
   return disconnect();
}

//
// UDP "Connectionless" Function Set
//

SEXPR IPCSOC::create_udp()
{
   //
   // syntax: (SOCKET-CREATE-UDP) -> <sockfd>
   //
   //
   argstack.noargs();
   return MEMORY::fixnum( soc_create_udp() );
}

SEXPR IPCSOC::bind()
{
   //
   // syntax: (SOCKET-BIND <sockfd> <host-addr> <port> ) -> <sockfd>
   //
   //
   ArgstackIterator iter;
   const int fd          = static_cast<int>(getfixnum(guard(iter.getarg(), fixnump)));
   const char* host_addr = getstringdata(guard(iter.getarg(), stringp));
   const int port        = static_cast<int>(getfixnum(guard(iter.getlast(), fixnump)));

   // BIND
   const int sockfd = soc_bind( fd, host_addr, port );

   // return the bind return value
   return MEMORY::fixnum(sockfd);
}

SEXPR IPCSOC::bind_address()
{
   //
   // syntax: (SOCKET-BIND-ADDRESS <sockfd> <address> ) -> <sockfd>
   //
   ArgstackIterator iter;
   const int fd = static_cast<int>(getfixnum(guard(iter.getarg(), fixnump)));
   const int sa = static_cast<int>(getfixnum(guard(iter.getlast(), fixnump)));

   // BIND ADDRESS
   const int sockfd = soc_bind_address( fd, sa_database[sa] );

   // return the bind return value
   return MEMORY::fixnum(sockfd);
}

SEXPR IPCSOC::create_address()
{
   //
   // syntax: (SOCKET-CREATE-ADDRESS <host-addr> <port> ) -> <address>
   //
   ArgstackIterator iter;
   char* host_addr = getstringdata(guard(iter.getarg(), stringp));
   int port        = static_cast<int>(getfixnum(guard(iter.getlast(), fixnump)));

   const int sa = allocate_sa();

   soc_create_address( sa_database[sa], host_addr, port );

   return MEMORY::fixnum(sa);
}

//
// All
//
static int max( int a, int b ) { return a > b ? a : b; }

SEXPR IPCSOC::read_select()
{
   //
   // syntax: (READ-SELECT <fd-list>) --> <ready-list>
   //
   ArgstackIterator iter;
   SEXPR rfds = guard(iter.getarg(), listp);

   int max_fdp1 = 0;

   // set the readfds set
   fd_set readfds;
   FD_ZERO(&readfds);

   while (rfds)
   {
      SEXPR e = car(rfds);

      if (fixnump(e))
      {
	 const int fd = getfixnum(e);
	 max_fdp1 = max(max_fdp1, fd+1);
	 FD_SET(fd, &readfds);
      }
      rfds = cdr(rfds);
   }

   printf( "max_fdp1 = %d\n", max_fdp1 );

   static struct timeval timeout;
   timeout.tv_sec = 0;
   timeout.tv_usec = 0;

   // SELECT
   const int r = ::select( max_fdp1, &readfds, nullptr, nullptr, &timeout );

   if (r < 0)
      ERROR::severe( "soc_select: select error", MEMORY::fixnum(r) );

   SEXPR ready_fds = null;
   push(ready_fds);

   for (int i = 0; i < max_fdp1; ++i)
   {
      if (FD_ISSET(i, &readfds))
      {
	 push(MEMORY::fixnum(i));
	 SEXPR cell = MEMORY::cons(null, null);
	 rplaca(cell, pop());
	 rplacd(cell, pop());
	 push(cell);
      }
   }

   return pop();
}
