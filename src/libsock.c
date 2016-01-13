/* Minimalist socket support functions to support setting TCP_NODELAY
 * on socket connections and to infer socket descriptor for R connections.
 *
 * Copyright (C) 2016 Bryan W. Lewis
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#include <stdio.h>
#ifdef WIN32
#include <winsock2.h>
#include <windows.h>
#include <ws2tcpip.h>
#else
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h> // TCP_NODELAY
#include <arpa/inet.h>
#include <netdb.h>
#include <unistd.h>
#define CHECKFD 8192
#endif

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>

/* Crudely report open file descriptors on unix-like systems This is used to
 * infer the active socket descriptor to apply TCP_NODELAY. This is a
 * provisional approach. In the next package version, we'll define a new
 * R_new_custom_connection object for rredis instead.
 */
SEXP OPEN_FD()
{
#ifdef WIN32
  return R_NilValue;
#else
  SEXP ans;
  struct stat s;
  int j, k, l = 0;
  int buf[CHECKFD];
  for(j=0;j<CHECKFD;++j)
  {
    k = fstat(j,&s);
    if(k>=0)
    {
      buf[l] = j;
      l++;
    }
  }
  PROTECT (ans = allocVector (INTSXP, l));
  for(j=0;j<l;++j)
  {
    INTEGER(ans)[j] = buf[j];
  }
  UNPROTECT (1);
  return ans;
#endif
}


/* Set the Nagle socket option for socket S to an integer value VAL,
 * returning the set value.
 */
SEXP SOCK_NAGLE(SEXP S, SEXP VAL)
{
  socklen_t len;
  int val = INTEGER(VAL)[0];
#ifdef WIN32
  SOCKET s = (SOCKET)INTEGER(S)[0];
#else
  int s = INTEGER(S)[0];
#endif
  len = sizeof(val);
  if(val>=0)
  {
    val = (int)(setsockopt(s, IPPROTO_TCP, TCP_NODELAY, (const void *)&val, len) == 0);
    if(val<0) error("Error setting TCP_NODELAY");
  }
  if(getsockopt(s, IPPROTO_TCP, TCP_NODELAY, (void *)&val, &len) < 0) error("Error setting TCP_NODELAY");
  return ScalarInteger(val);
}
