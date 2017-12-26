#ifndef IPCSOC_HXX
#define IPCSOC_HXX

// syntax: (SOCKET-CREATE-TCP [<server-flag>]) -> <sockfd>
// syntax: (SOCKET-CREATE-UDP) -> <sockfd>

// syntax: (SOCKET-READ <sockfd> <numbytes>) -> <byte-vector>
// syntax: (SOCKET-WRITE <sockfd> <byte-vector>) -> <fixnum>
// syntax: (SOCKET-RECVFROM <sockfd> <numbytes> <from-addr>) -> <byte-vector>
// syntax: (SOCKET-RECV <sockfd> <numbytes>) -> <byte-vector>
// syntax: (SOCKET-SENDTO <sockfd> <byte-vector> <to-address>) -> <fixnum>

// syntax: (SOCKET-BIND <sockfd> <host-addr> <port> ) -> <sockfd>
// syntax: (SOCKET-BIND-ADDRESS <sockfd> <address> ) -> <sockfd>
// syntax: (SOCKET-CREATE-ADDRESS <host-addr> <port> ) -> <address>

// syntax: (SOCKET-LISTEN <sockfd> [<backlog>]) -> <result>
// syntax: (SOCKET-ACCEPT <sockfd> ) -> <sockfd>
// syntax: (SOCKET-CONNECT <fd> <server-host-addr> <server-port> [<numtries>=1] ) -> <sockfd>
// syntax: (SOCKET-DISCONNECT <socket>) -> <fixnum>
// syntax: (SOCKET-CLOSE <socket>) -> <fixnum>

// syntax: (READ-SELECT <fd-list>) --> <ready-list>

namespace IPCSOC
{
   SEXPR read();
   SEXPR write();
   SEXPR recvfrom();
   SEXPR recv();
   SEXPR sendto();
   
   SEXPR create_tcp();
   SEXPR create_udp();
   
   SEXPR bind();
   SEXPR bind_address();
   SEXPR create_address();
   
   SEXPR listen();
   SEXPR accept();
   SEXPR connect();
   SEXPR disconnect();
   SEXPR close();
   
   SEXPR read_select();
};

#endif
