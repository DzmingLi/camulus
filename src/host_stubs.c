#include <signal.h>
#include <pthread.h>
#include <errno.h>
#include <unistd.h>
#include <caml/mlvalues.h>

/* The wakeup FD is nonblocking. A deleted Emacs process must not kill the
   host with SIGPIPE. Block it on this thread for the write, consume only a
   newly generated SIGPIPE, then restore this thread's mask. No handler changes. */
CAMLprim value probe_notify(value fd) {
  sigset_t blocked, previous, pending;
  sigemptyset(&blocked);
  sigaddset(&blocked, SIGPIPE);
  if (pthread_sigmask(SIG_BLOCK, &blocked, &previous) != 0) return Val_false;
  sigpending(&pending);
  int already_pending = sigismember(&pending, SIGPIPE);
  ssize_t result;
  do { result = write(Int_val(fd), "!", 1); } while (result < 0 && errno == EINTR);
  int error = result < 0 ? errno : 0;
  if (error == EPIPE && !already_pending) {
    struct timespec zero = { 0, 0 };
    while (sigtimedwait(&blocked, NULL, &zero) < 0 && errno == EINTR) {}
  }
  pthread_sigmask(SIG_SETMASK, &previous, NULL);
  return Val_bool(result == 1 || error == EAGAIN || error == EWOULDBLOCK);
}
