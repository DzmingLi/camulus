#include <signal.h>
#include <pthread.h>
#include <errno.h>
#include <unistd.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>

/* UI/test-thread-only snapshots; never install or restore a signal handler. */
static struct sigaction saved[2];
static const int signals[2] = { SIGPIPE, SIGCHLD };
static int initialized = 0;

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

CAMLprim value probe_snapshot_signals(value unit) {
  (void)unit;
  for (int i = 0; i < 2; i++)
    if (sigaction(signals[i], NULL, &saved[i]) != 0)
      caml_failwith("sigaction snapshot failed");
  initialized = 1;
  return Val_unit;
}

CAMLprim value probe_signals_unchanged(value unit) {
  (void)unit;
  if (!initialized) return Val_false;
  for (int i = 0; i < 2; i++) {
    struct sigaction current;
    if (sigaction(signals[i], NULL, &current) != 0) return Val_false;
    if (current.sa_handler != saved[i].sa_handler ||
        current.sa_flags != saved[i].sa_flags) return Val_false;
    for (int sig = 1; sig < NSIG; sig++)
      if (sigismember(&current.sa_mask, sig) != sigismember(&saved[i].sa_mask, sig))
        return Val_false;
  }
  return Val_true;
}
