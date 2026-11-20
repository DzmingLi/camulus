#include <signal.h>
#include <pthread.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <caml/mlvalues.h>

/* Emacs owns the SIGPIPE handler. Suppress SIGPIPE on the wakeup FD where
   supported, otherwise mask it for this write and consume the generated signal. */
CAMLprim value probe_notify(value fd) {
#ifdef F_SETNOSIGPIPE
  if (fcntl(Int_val(fd), F_SETNOSIGPIPE, 1) == -1) return Val_false;
#else
  sigset_t blocked, previous, pending;
  sigemptyset(&blocked);
  sigaddset(&blocked, SIGPIPE);
  if (pthread_sigmask(SIG_BLOCK, &blocked, &previous) != 0) return Val_false;
  sigpending(&pending);
  int already_pending = sigismember(&pending, SIGPIPE);
#endif
  ssize_t result;
  do { result = write(Int_val(fd), "!", 1); } while (result < 0 && errno == EINTR);
  int error = result < 0 ? errno : 0;
#ifndef F_SETNOSIGPIPE
  if (error == EPIPE && !already_pending) {
    struct timespec zero = { 0, 0 };
    while (sigtimedwait(&blocked, NULL, &zero) < 0 && errno == EINTR) {}
  }
  pthread_sigmask(SIG_SETMASK, &previous, NULL);
#endif
  return Val_bool(result == 1 || error == EAGAIN || error == EWOULDBLOCK);
}
