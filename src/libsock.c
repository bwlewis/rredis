/* Minimalist socket support functions */
#include <stdio.h>
#ifdef WIN32
#include <winsock2.h>
#include <windows.h>
#include <ws2tcpip.h>
#else
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h> // TCP_NODELAY
#include <arpa/inet.h>
#include <netdb.h>
#include <unistd.h>
#endif

#include <R.h>
#include <Rinternals.h>

/* Set the Nagle socket option for socket S to an integer value VAL,
 * returning the set value.
 */
SEXP SOCK_NAGLE(SEXP S, SEXP VAL)
{
  int val = INTEGER(VAL)[0];
#ifdef WIN32
  SOCKET s = (SOCKET)INTEGER(S)[0];
#else
  int s = INTEGER(S)[0];
#endif
  if(val==1)
  {
    setsockopt(s, IPPROTO_TCP, TCP_NODELAY, &val, sizeof(val));
  }
  return ScalarInteger(val);
}
