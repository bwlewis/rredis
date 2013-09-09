#define BACKLOG 100
#define RXBUF 16384             /* TCP receive buffer */
#define MBUF 1048576            /* Message buffer base size */

int tcpconnect (char *, int, int);
SEXP SOCK_CLOSE (SEXP);
SEXP SOCK_CONNECT (SEXP, SEXP, SEXP);
SEXP SOCK_GETLINE(SEXP);
SEXP SOCK_NAME (SEXP);
SEXP SOCK_POLL (SEXP, SEXP, SEXP);
SEXP SOCK_RECV (SEXP, SEXP, SEXP, SEXP);
SEXP SOCK_RECV_N(SEXP, SEXP);
SEXP SOCK_SEND (SEXP, SEXP);

#ifdef WIN32
#include <time.h>
#include <sys/time.h>
#include <ws2tcpip.h>
#include <Winsock2.h>
#include <windows.h>
/* winsock doesn't feature poll(), so there is a version implemented in terms
 * of select() in mingw.c. The following definitions are copied from linux man
 * pages. A poll() macro is defined to call the version in mingw.c.
 */
#define POLLIN      0x0001    /* There is data to read */
#define POLLPRI     0x0002    /* There is urgent data to read */
#define POLLOUT     0x0004    /* Writing now will not block */
#define POLLERR     0x0008    /* Error condition */
#define POLLHUP     0x0010    /* Hung up */
#define POLLNVAL    0x0020    /* Invalid request: fd not open */
struct pollfd {
  SOCKET fd;        /* file descriptor */
  short events;     /* requested events */
  short revents;    /* returned events */
};
int
mingw_poll (struct pollfd *, unsigned int, int);
#define poll(x, y, z)        mingw_poll(x, y, z)
#endif
