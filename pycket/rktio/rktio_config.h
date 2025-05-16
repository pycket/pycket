/* rktio_config.h.  Generated from rktio_config.h.in by configure.  */
#if ((defined(_MSC_VER) || defined(__MINGW32__))                        \
     && (defined(__WIN32__) || defined(WIN32) || defined(_WIN32)))
# define RKTIO_SYSTEM_WINDOWS
#else
# define RKTIO_SYSTEM_UNIX
#endif

/* Whether `intptr_t' is available. */
#define HAVE_INTPTR_T 1

/* Whether `uintptr_t' is available. */
#define HAVE_UINTPTR_T 1

#ifdef HAVE_INTPTR_T
# include <inttypes.h>
#endif
#ifndef HAVE_INTPTR_T
typedef long intptr_t;
#endif
#ifndef HAVE_UINTPTR_T
typedef unsigned long uintptr_t;
#endif

#ifdef RKTIO_SYSTEM_WINDOWS
# ifdef _MSC_VER
typedef _int64 rktio_int64_t;
typedef unsigned _int64 rktio_uint64_t;
# else
typedef __int64 rktio_int64_t;
typedef unsigned __int64 rktio_uint64_t;
# endif
#else
typedef long long rktio_int64_t;
typedef unsigned long long rktio_uint64_t;
#endif

/* Endianness. */
/* #undef RKTIO_BIG_ENDIAN */

/* Whether pthread is available */
#define RKTIO_USE_PTHREADS 1

/* When poll(), epoll(), kqueue(), etc. is available: */
#define HAVE_POLL_SYSCALL 1
#define HAVE_EPOLL_SYSCALL 1
#define HAVE_INOTIFY_SYSCALL 1
/* #undef HAVE_KQUEUE_SYSCALL */

/* Whether getaddrinfo() is available: */
#define HAVE_GETADDRINFO 1

/* Whether nl_langinfo works. */
#define RKTIO_HAVE_CODESET 1

/* In case you want to avoid dynamic sizing of `fd_set` arrays: */
/* #undef RKTIO_STATIC_FDSET_SIZE */

/* In case you want to use fcntl for file locks */
/* #undef RKTIO_USE_FCNTL_AND_FORK_FOR_FILE_LOCKS */

/* In case iconv is not available: */
/* #undef RKTIO_NO_ICONV */

/* Fields of struct dirent */
/* #undef HAVE_DIRENT_NAMLEN */
/* #undef HAVE_DIRENT_NAMELEN */

/* In case xlocale is not available: */
#define RKTIO_USE_XLOCALE 1
/* #undef RKTIO_USE_XLOCALE_HEADER */

#define RKTIO_HAS_CLOEXEC 1
