#if defined(__APPLE__) && defined(__MACH__) && !defined(OS_X)
# define OS_X
#endif

#include "rktio_platform.h"

#ifdef RKTIO_SYSTEM_WINDOWS
# if _WIN32_WINNT < 0x602
#  undef _WIN32_WINNT
#  define _WIN32_WINNT 0x602
# endif
# include <winsock2.h>
# include <windows.h>
#endif
#ifdef RKTIO_USE_PTHREADS
# include <pthread.h>
#endif
#ifdef RKTIO_USE_XLOCALE
# ifdef RKTIO_USE_XLOCALE_HEADER
#  include <xlocale.h>
# else
#  include <locale.h>
# endif
#endif

#if defined(RKTIO_SYSTEM_UNIX) && !defined(RKTIO_STATIC_FDSET_SIZE)
# define USE_DYNAMIC_FDSET_SIZE
#endif

#ifdef RKTIO_SYSTEM_WINDOWS
# define USE_FAR_RKTIO_FDCALLS
#endif
#ifdef USE_DYNAMIC_FDSET_SIZE
# define USE_FAR_RKTIO_FDCALLS
#endif
#ifdef HAVE_POLL_SYSCALL
# define USE_FAR_RKTIO_FDCALLS
#endif

#if defined(RKTIO_SYSTEM_UNIX) && defined(RKTIO_USE_PTHREADS)
# define SUPPORT_BACKGROUND_SLEEP_THREAD
struct background_sleep_t;
#endif

/*========================================================================*/
/* File-descriptor actions without a rktio_t                              */
/*========================================================================*/

intptr_t rktio_internal_fd_system_fd(rktio_fd_t *rfd);
rktio_ok_t rktio_internal_close(rktio_t *rktio /* may be NULL */, rktio_fd_t *rfd, int set_error);

/*========================================================================*/
/* Globals, as gathered into `rktio_t`                                    */
/*========================================================================*/

struct rktio_t {
  int errid;
  int errkind;
  int errstep;
#ifdef RKTIO_SYSTEM_WINDOWS
  char *last_err_str;
#endif

#ifdef RKTIO_SYSTEM_UNIX
  struct group_member_cache_entry_t *group_member_cache;
  int external_event_fd;
  int put_external_event_fd;
  int long_term_poll_set_fd;
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  int windows_nt_or_later;
  HANDLE break_semaphore;
  int wsr_size;
  struct rktio_socket_t *wsr_array;
  int made_progress;
  DWORD max_sleep_time;
  int got_hires_freq;
  LARGE_INTEGER hires_freq;
#endif
#ifdef USE_FAR_RKTIO_FDCALLS
  /* A single fdset that can be reused for immediate actions: */
  struct rktio_poll_set_t *rktio_global_poll_set;
#endif
#ifdef RKTIO_GROWABLE_FDSET
  int max_fd_so_far;
#endif

#if defined(RKTIO_SYSTEM_WINDOWS) || defined(RKTIO_USE_PTHREADS)
  int ghbn_started, ghbn_run;
  struct rktio_addrinfo_lookup_t *ghbn_requests;
# ifdef RKTIO_USE_PTHREADS
  pthread_t ghbn_th;
  pthread_mutex_t ghbn_lock;
  pthread_cond_t ghbn_start;
# endif
# ifdef RKTIO_SYSTEM_WINDOWS
  HANDLE ghbn_th;
  HANDLE ghbn_lock;
  HANDLE ghbn_start;
# endif
#endif

#if defined(RKTIO_SYSTEM_UNIX) && !defined(RKTIO_USE_PTHREADS)
  struct System_Child *system_children;
  volatile int need_to_check_children;
  int in_sigchld_chain;
  struct rktio_t *next; /* chaining for SIGCHLD handling */
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  uintptr_t process_children_msecs;
  HANDLE process_job_object;
#endif

#ifdef HAVE_INOTIFY_SYSCALL
  struct rin_inotify_state_t *inotify_server;
#endif

#ifdef RKTIO_SYSTEM_WINDOWS
  intptr_t wide_buffer_size;
  wchar_t *wide_buffer;
#endif

#ifdef RKTIO_USE_FCNTL_AND_FORK_FOR_FILE_LOCKS
  struct rktio_hash_t *locked_fd_process_map;
#endif

#ifdef RKTIO_SYSTEM_WINDOWS
  HANDLE hEventLog;
#endif

  int pending_os_signals[RKTIO_NUM_OS_SIGNALS];

  int processor_count;

  struct rktio_dll_t *all_dlls;
  struct rktio_hash_t *dlls_by_name;
#ifdef RKTIO_SYSTEM_UNIX
  char *dll_error;
#endif

#ifdef SUPPORT_BACKGROUND_SLEEP_THREAD
  struct background_sleep_t *background;
#endif

#ifdef OS_X
  int macos_kernel_version; /* e.g., 10 => 10.6, 15 => 10.11 */
#endif

#ifdef RKTIO_USE_XLOCALE
  locale_t locale;
#endif
};

/*========================================================================*/
/* Poll sets                                                              */
/*========================================================================*/

void rktio_alloc_global_poll_set(rktio_t *rktio);
void rktio_free_global_poll_set(rktio_t *rktio);
int rktio_initialize_signal(rktio_t *rktio);
void rktio_free_signal(rktio_t *rktio);

#ifdef USE_FAR_RKTIO_FDCALLS

rktio_poll_set_t *rktio_get_fdset(rktio_poll_set_t *fdarray, int pos);
void rktio_fdzero(rktio_poll_set_t *fd);
void rktio_fdset(rktio_poll_set_t *fd, intptr_t n);
void rktio_fdclr(rktio_poll_set_t *fd, intptr_t n);
int rktio_fdisset(rktio_poll_set_t *fd, intptr_t n);
  
# define DECL_FDSET(n, c) rktio_poll_set_t *n
# define INIT_DECL_FDSET(r, w, e) { \
   r = RKTIO_GET_FDSET(rktio->rktio_global_poll_set, 0 ); \
   w = RKTIO_GET_FDSET(rktio->rktio_global_poll_set, 1 ); \
   e = RKTIO_GET_FDSET(rktio->rktio_global_poll_set, 2 ); \
 }
# define INIT_DECL_RD_FDSET(r) r = RKTIO_GET_FDSET(rktio->rktio_global_poll_set, 0 )
# define INIT_DECL_WR_FDSET(r) r = RKTIO_GET_FDSET(rktio->rktio_global_poll_set, 1 )
# define INIT_DECL_ER_FDSET(r) r = RKTIO_GET_FDSET(rktio->rktio_global_poll_set, 2 )

# define RKTIO_GET_FDSET(p, n) rktio_get_fdset(p, n)
# define RKTIO_FD_ZERO(p) rktio_fdzero(p)
# define RKTIO_FD_SET(n, p) rktio_fdset(p, n)
# define RKTIO_FD_CLR(n, p) rktio_fdclr(p, n)
# define RKTIO_FD_ISSET(n, p) rktio_fdisset(p, n)

# if !defined(HAVE_POLL_SYSCALL) && !defined(RKTIO_SYSTEM_WINDOWS)
#  ifdef RKTIO_GROWABLE_FDSET
#   define RKTIO_FDS(p) ((fd_set *)rktio_resolve_fds(p))
void *rktio_resolve_fds(rktio_poll_set_t *fd);
#  else
#   define RKTIO_FDS(p) ((fd_set *)p)
#  endif
# endif

#else

#include <sys/select.h>
struct rktio_poll_set_t { fd_set data; int nosleep; };

/* Need "far" call to fdzero to deal with `nosleep`: */
void rktio_fdzero(rktio_poll_set_t *fd);

# define DECL_FDSET(n, c) rktio_poll_set_t n[c]
# define INIT_DECL_FDSET(r, w, e) /* empty */
# define INIT_DECL_RD_FDSET(r) /* empty */
# define INIT_DECL_WR_FDSET(r) /* empty */
# define INIT_DECL_ER_FDSET(r) /* empty */

# define RKTIO_FDS(p) (&(p)->data)

# define RKTIO_GET_FDSET(p, n) ((p)+(n))
# define RKTIO_FD_ZERO(p) rktio_fdzero(p)
# define RKTIO_FD_SET(n, p) FD_SET(n, RKTIO_FDS(p))
# define RKTIO_FD_CLR(n, p) FD_CLR(n, RKTIO_FDS(p))
# define RKTIO_FD_ISSET(n, p) FD_ISSET(n, RKTIO_FDS(p))

#endif

void rktio_merge_fd_sets(rktio_poll_set_t *fds, rktio_poll_set_t *src_fds);
void rktio_clean_fd_set(rktio_poll_set_t *fds);
int rktio_get_fd_limit(rktio_poll_set_t *fds);

#if defined(HAVE_KQUEUE_SYSCALL) || defined(HAVE_EPOLL_SYSCALL)
int rktio_ltps_get_fd(rktio_ltps_t *lt);
#else
rktio_poll_set_t *rktio_ltps_get_fd_set(rktio_ltps_t *lt); 
#endif

#if defined(HAVE_POLL_SYSCALL)
int rktio_get_poll_count(rktio_poll_set_t *fds);
struct pollfd *rktio_get_poll_fd_array(rktio_poll_set_t *fds);
#endif

/*========================================================================*/
/* Network                                                                */
/*========================================================================*/

void rktio_socket_init(rktio_t *rktio, rktio_fd_t *rfd);

int rktio_socket_close(rktio_t *rktio, rktio_fd_t *rfd, int set_error);
void rktio_socket_own(rktio_t *rktio, rktio_fd_t *rfd);
void rktio_socket_forget_owned(rktio_t *rktio, rktio_fd_t *rfd);
rktio_fd_t *rktio_socket_dup(rktio_t *rktio, rktio_fd_t *rfd);

int rktio_socket_poll_write_ready(rktio_t *rktio, rktio_fd_t *rfd);
int rktio_socket_poll_read_ready(rktio_t *rktio, rktio_fd_t *rfd);

intptr_t rktio_socket_write(rktio_t *rktio, rktio_fd_t *rfd, const char *buffer, intptr_t len);
intptr_t rktio_socket_read(rktio_t *rktio, rktio_fd_t *rfd, char *buffer, intptr_t len);
  
void rktio_free_ghbn(rktio_t *rktio);

const char *rktio_gai_strerror(rktio_t *rktio, int errnum);

/*========================================================================*/
/* Processes                                                              */
/*========================================================================*/

int rktio_process_init(rktio_t *rktio);
void rktio_process_deinit(rktio_t *rktio);

void rktio_cloexec_lock();
void rktio_cloexec_unlock();

#ifdef RKTIO_HAS_CLOEXEC
# define RKTIO_CLOEXEC O_CLOEXEC
#else
# define RKTIO_CLOEXEC 0
#endif
  
/*========================================================================*/
/* Strings                                                                */
/*========================================================================*/

#ifdef RKTIO_SYSTEM_WINDOWS
# define MSC_IZE(n) _ ## n
# define MSC_W_IZE(n) _w ## n
# define MSC_WIDE_PATH_temp(n) WIDE_PATH_temp(n)
#else
# define MSC_IZE(n) n
# define MSC_W_IZE(n) MSC_IZE(n)
# define MSC_WIDE_PATH_temp(n) n
#endif

#ifdef RKTIO_SYSTEM_WINDOWS

wchar_t *rktio_convert_to_wchar(rktio_t *rktio, const char *s, int do_copy);
char *rktio_convert_from_wchar(const wchar_t *ws, int free_given);

# define WIDE_PATH_temp(s) rktio_convert_to_wchar(rktio, s, 0)
# define WIDE_PATH_copy(s) rktio_convert_to_wchar(rktio, s, 1)

# define NARROW_PATH_copy(ws) rktio_convert_from_wchar(ws, 0)
# define NARROW_PATH_copy_then_free(ws) rktio_convert_from_wchar(ws, 1)

typedef wchar_t WIDE_PATH_t;

#else

typedef char WIDE_PATH_t;

#endif

void rktio_convert_init(rktio_t *rktio);
void rktio_convert_deinit(rktio_t *rktio);

/*========================================================================*/
/* Hash table                                                             */
/*========================================================================*/

/* Maps keys that aren't -1 to non-NULL values */

typedef struct rktio_hash_t rktio_hash_t;

rktio_hash_t *rktio_hash_new(void);
void rktio_hash_free(rktio_hash_t *ht, int free_values);
int rktio_hash_is_empty(rktio_hash_t *ht);
void *rktio_hash_get(rktio_hash_t *ht, intptr_t key);
void rktio_hash_remove(rktio_hash_t *ht, intptr_t key, int dont_rehash);
void rktio_hash_set(rktio_hash_t *ht, intptr_t key, void *v);

intptr_t rktio_hash_size(rktio_hash_t *ht);
intptr_t rktio_hash_get_key(rktio_hash_t *ht, intptr_t i);

intptr_t rktio_hash_string(const char *s);

/*========================================================================*/
/* Misc                                                                   */
/*========================================================================*/

/* On Mac OS, for example, `read` and `write` expect a value less than
   2GB. Use 32MB as a limit that is very large, but still likely small
   enough for all OSes. */
#define MAX_READ_WRITE_REQUEST_BYTES (32 * 1048576)
#define LIMIT_REQUEST_SIZE(n) (((n) > MAX_READ_WRITE_REQUEST_BYTES) ? MAX_READ_WRITE_REQUEST_BYTES : (n))

void rktio_get_posix_error(rktio_t *rktio);
#define get_posix_error() rktio_get_posix_error(rktio)

void rktio_set_racket_error(rktio_t *rktio, int errid);
#define set_racket_error(e) rktio_set_racket_error(rktio, e)

#ifdef RKTIO_SYSTEM_WINDOWS
void rktio_get_windows_error(rktio_t *rktio);
# define get_windows_error() rktio_get_windows_error(rktio)
void rktio_set_windows_error(rktio_t *rktio, int errid);
# define set_windows_error(errid) rktio_set_windows_error(rktio, errid)
#endif

void rktio_error_clean(rktio_t *rktio);

void rktio_dll_clean(rktio_t *rktio);
#ifdef RKTIO_SYSTEM_WINDOWS
HANDLE rktio_load_library(rktio_const_string_t name);
void *rktio_get_proc_address(HANDLE m, rktio_const_string_t name);
#endif

#if defined(USE_FNDELAY_O_NONBLOCK)
# define RKTIO_NONBLOCKING FNDELAY
#else
# define RKTIO_NONBLOCKING O_NONBLOCK
#endif

#ifndef RKTIO_BINARY
# define RKTIO_BINARY 0
#endif

#ifdef RKTIO_SYSTEM_UNIX
int rktio_reliably_close_err(intptr_t s);
void rktio_reliably_close(intptr_t s);
int rktio_close_fds_len();
void rktio_close_fds_after_fork(int len, int skip1, int skip2, int skip3);
#endif

void rktio_fd_cloexec(intptr_t fd);

int rktio_system_fd_is_terminal(rktio_t *rktio, intptr_t fd);

#if defined(RKTIO_USE_PTHREADS) && !defined(NO_PTHREAD_CANCEL)
# define RKTIO_USE_PENDING_OPEN
#endif

#ifdef RKTIO_USE_PENDING_OPEN
struct open_in_thread_t;
rktio_fd_t *rktio_pending_system_fd(rktio_t *rktio, struct open_in_thread_t *oit, int modes);
void rktio_update_system_fd(rktio_t *rktio, rktio_fd_t *rfd, int fd, int modes);
int rktio_fd_is_pending_open(rktio_t *rktio, rktio_fd_t *rfd);
int rktio_pending_open_poll(rktio_t *rktio, rktio_fd_t *rfd, struct open_in_thread_t *oit);
void rktio_poll_add_pending_open(rktio_t *rktio, rktio_fd_t *rfd, struct open_in_thread_t *oit, rktio_poll_set_t *fds);
void rktio_pending_open_detach(rktio_t *rktio, struct open_in_thread_t *oit);
void rktio_pending_open_attach(rktio_t *rktio, struct open_in_thread_t *oit);
void rktio_pending_open_retain(rktio_t *rktio, struct open_in_thread_t *oit);
int rktio_pending_open_release(rktio_t *rktio, struct open_in_thread_t *oit);
#endif

int rktio_environ_init(rktio_t *rktio);
void rktio_getenv_lock();
void rktio_getenv_unlock();
void *rktio_envvars_to_block(rktio_t *rktio, rktio_envvars_t *envvars);

void rktio_stop_fs_change(rktio_t *rktio);

void rktio_init_time(rktio_t *rktio);

void rktio_init_cpu(rktio_t *rktio);

#ifdef RKTIO_SYSTEM_WINDOWS
int rktio_winsock_init(rktio_t *rktio);
void rktio_winsock_done(rktio_t *rktio);
#endif
void rktio_init_wide(rktio_t *rktio);

#ifdef RKTIO_USE_FCNTL_AND_FORK_FOR_FILE_LOCKS
void rktio_release_lockf(rktio_t *rktio, int fd);
#endif

int rktio_make_os_pipe(rktio_t *rktio, intptr_t *a, int flags);

#ifdef RKTIO_SYSTEM_UNIX
char **rktio_get_environ_array(void);
#endif

void rktio_syslog_init(rktio_t* rktio);
void rktio_syslog_clean(rktio_t* rktio);

void rktio_stop_background(rktio_t *rktio);

#ifdef USE_TRANSITIONAL_64_FILE_OPS
# define BIG_OFF_T_IZE(n) n ## 64
#else
# define BIG_OFF_T_IZE(n) n
#endif

char *rktio_strndup(char *s, intptr_t len);

#ifdef RKTIO_SYSTEM_UNIX
void rktio_set_signal_handler(int sig_id, void (*proc)(int));
void rktio_restore_modified_signal_handlers();
#endif
void rktio_forget_os_signal_handler(rktio_t *rktio);


#ifdef RKTIO_SYSTEM_WINDOWS
int rktio_system_time_is_dst(SYSTEMTIME *st, TIME_ZONE_INFORMATION *_tz);
#endif

#ifdef RKTIO_SYSTEM_WINDOWS
void rktio_console_ctl_c(void);
void rktio_set_console_handler(void);
#endif
