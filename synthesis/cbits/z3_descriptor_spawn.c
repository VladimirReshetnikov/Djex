#define _GNU_SOURCE 1

#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <linux/memfd.h>
#include <pthread.h>
#include <signal.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#ifndef AT_EMPTY_PATH
#define AT_EMPTY_PATH 0x1000
#endif

enum {
  DJEX_DESCRIPTOR_EXEC_STAGE_PROCESS_GROUP = 1,
  DJEX_DESCRIPTOR_EXEC_STAGE_STANDARD_INPUT = 2,
  DJEX_DESCRIPTOR_EXEC_STAGE_STANDARD_OUTPUT = 3,
  DJEX_DESCRIPTOR_EXEC_STAGE_STANDARD_ERROR = 4,
  DJEX_DESCRIPTOR_EXEC_STAGE_WORKING_DIRECTORY = 5,
  DJEX_DESCRIPTOR_EXEC_STAGE_CLOSE_DESCRIPTORS = 6,
  DJEX_DESCRIPTOR_EXEC_STAGE_SIGNAL_MASK = 7,
  DJEX_DESCRIPTOR_EXEC_STAGE_EXECVEAT = 8
};

struct djex_descriptor_exec_failure {
  int32_t stage;
  int32_t error_number;
};

static int djex_move_above_standard_descriptors(int descriptor) {
  int moved;
  if (descriptor > STDERR_FILENO) {
    return descriptor;
  }
  moved = fcntl(descriptor, F_DUPFD_CLOEXEC, STDERR_FILENO + 1);
  if (moved < 0) {
    (void) close(descriptor);
    return -1;
  }
  (void) close(descriptor);
  return moved;
}

static int djex_pipe_cloexec(int descriptors[2]) {
  if (pipe2(descriptors, O_CLOEXEC) < 0) {
    return -1;
  }
  descriptors[0] = djex_move_above_standard_descriptors(descriptors[0]);
  if (descriptors[0] < 0) {
    (void) close(descriptors[1]);
    return -1;
  }
  descriptors[1] = djex_move_above_standard_descriptors(descriptors[1]);
  if (descriptors[1] < 0) {
    (void) close(descriptors[0]);
    return -1;
  }
  return 0;
}

static void djex_close_pair(int descriptors[2]) {
  if (descriptors[0] >= 0) {
    (void) close(descriptors[0]);
  }
  if (descriptors[1] >= 0) {
    (void) close(descriptors[1]);
  }
}

static void djex_report_exec_failure(int descriptor, int stage) {
  struct djex_descriptor_exec_failure failure;
  ssize_t ignored;
  failure.stage = (int32_t) stage;
  failure.error_number = (int32_t) errno;
  do {
    ignored = write(descriptor, &failure, sizeof(failure));
  } while (ignored < 0 && errno == EINTR);
  (void) ignored;
}

static int djex_child_descriptor_limit(void) {
  struct rlimit limit;
  long configured;
  if (getrlimit(RLIMIT_NOFILE, &limit) == 0 && limit.rlim_cur != RLIM_INFINITY) {
    if (limit.rlim_cur > (rlim_t) INT_MAX) {
      return INT_MAX;
    }
    return (int) limit.rlim_cur;
  }
  configured = sysconf(_SC_OPEN_MAX);
  if (configured < 0 || configured > INT_MAX) {
    return 65536;
  }
  return (int) configured;
}

static int djex_close_range(unsigned int first, unsigned int last) {
  if (first > last) {
    return 0;
  }
#ifdef SYS_close_range
  return (int) syscall(SYS_close_range, first, last, 0U);
#else
  errno = ENOSYS;
  return -1;
#endif
}

static int djex_close_child_descriptors(
    int executable_descriptor,
    int status_descriptor,
    int descriptor_limit) {
  unsigned int first_kept;
  unsigned int second_kept;
  int descriptor;

  if (executable_descriptor < status_descriptor) {
    first_kept = (unsigned int) executable_descriptor;
    second_kept = (unsigned int) status_descriptor;
  } else {
    first_kept = (unsigned int) status_descriptor;
    second_kept = (unsigned int) executable_descriptor;
  }
  if (djex_close_range(STDERR_FILENO + 1U, first_kept - 1U) == 0 &&
      djex_close_range(first_kept + 1U, second_kept - 1U) == 0 &&
      djex_close_range(second_kept + 1U, UINT_MAX) == 0) {
    return 0;
  }
  if (errno != ENOSYS) {
    return -1;
  }

  /* Linux before close_range: finite exact fallback over the inherited table. */
  for (descriptor = STDERR_FILENO + 1;
       descriptor < descriptor_limit;
       ++descriptor) {
    if (descriptor != executable_descriptor && descriptor != status_descriptor) {
      int closed;
      do {
        closed = close(descriptor);
      } while (closed < 0 && errno == EINTR);
      if (closed < 0 && errno != EBADF) {
        return -1;
      }
    }
  }
  return 0;
}

int djex_z3_open_executable_descriptor(const char *path) {
  return open(
      path,
      O_RDONLY | O_CLOEXEC | O_NOCTTY | O_NOFOLLOW | O_NONBLOCK);
}

int djex_z3_create_staged_executable(void) {
  unsigned int flags = MFD_CLOEXEC | MFD_ALLOW_SEALING;
  return (int) syscall(SYS_memfd_create, "djex-z3-main-image", flags);
}

int djex_z3_seal_staged_executable(
    int descriptor,
    unsigned int source_mode,
    int64_t expected_size) {
  static const int required_seals =
      F_SEAL_WRITE | F_SEAL_GROW | F_SEAL_SHRINK | F_SEAL_SEAL;
  struct stat status;
  int observed_seals;

  /* Carry ordinary rwx bits only: set-id and file capabilities are omitted. */
  if (fchmod(descriptor, (mode_t) (source_mode & 0777U)) < 0) {
    return -1;
  }
  if (fcntl(descriptor, F_ADD_SEALS, required_seals) < 0) {
    return -1;
  }
  observed_seals = fcntl(descriptor, F_GET_SEALS);
  if (observed_seals < 0 ||
      (observed_seals & required_seals) != required_seals) {
    errno = EPERM;
    return -1;
  }
  if (fstat(descriptor, &status) < 0 || status.st_size != expected_size) {
    errno = EIO;
    return -1;
  }
  return 0;
}

/*
 * Return a forked child plus parent-owned pipe descriptors.  The status pipe
 * is CLOEXEC: EOF means execveat installed the opened main image; an eight-byte
 * payload means the child rejected an earlier stage.  No pathname is used by
 * the child and every descriptor other than stdio is closed by exec success.
 */
int djex_z3_descriptor_spawn(
    int executable_descriptor,
    int working_directory_descriptor,
    char *const argv[],
    char *const envp[],
    int *child_pid,
    int *standard_input,
    int *standard_output,
    int *standard_error,
    int *exec_status) {
  int input_pipe[2] = {-1, -1};
  int output_pipe[2] = {-1, -1};
  int error_pipe[2] = {-1, -1};
  int status_pipe[2] = {-1, -1};
  int executable_copy = -1;
  int working_directory_copy = -1;
  int descriptor_limit;
  int signal_result;
  int signals_blocked = 0;
  sigset_t blocked_signals;
  sigset_t original_signals;
  pid_t pid;

  executable_copy = fcntl(
      executable_descriptor, F_DUPFD_CLOEXEC, STDERR_FILENO + 1);
  if (executable_copy < 0) {
    goto failed;
  }
  working_directory_copy = fcntl(
      working_directory_descriptor, F_DUPFD_CLOEXEC, STDERR_FILENO + 1);
  if (working_directory_copy < 0) {
    goto failed;
  }
  if (djex_pipe_cloexec(input_pipe) < 0 ||
      djex_pipe_cloexec(output_pipe) < 0 ||
      djex_pipe_cloexec(error_pipe) < 0 ||
      djex_pipe_cloexec(status_pipe) < 0) {
    goto failed;
  }
  descriptor_limit = djex_child_descriptor_limit();
  if (sigfillset(&blocked_signals) < 0) {
    goto failed;
  }
  signal_result = pthread_sigmask(
      SIG_SETMASK, &blocked_signals, &original_signals);
  if (signal_result != 0) {
    errno = signal_result;
    goto failed;
  }
  signals_blocked = 1;
  pid = fork();
  if (pid < 0) {
    goto failed;
  }

  if (pid == 0) {
    (void) close(input_pipe[1]);
    (void) close(output_pipe[0]);
    (void) close(error_pipe[0]);
    (void) close(status_pipe[0]);

    if (setpgid(0, 0) < 0) {
      djex_report_exec_failure(
          status_pipe[1], DJEX_DESCRIPTOR_EXEC_STAGE_PROCESS_GROUP);
      _exit(127);
    }
    if (dup2(input_pipe[0], STDIN_FILENO) < 0) {
      djex_report_exec_failure(
          status_pipe[1], DJEX_DESCRIPTOR_EXEC_STAGE_STANDARD_INPUT);
      _exit(127);
    }
    if (dup2(output_pipe[1], STDOUT_FILENO) < 0) {
      djex_report_exec_failure(
          status_pipe[1], DJEX_DESCRIPTOR_EXEC_STAGE_STANDARD_OUTPUT);
      _exit(127);
    }
    if (dup2(error_pipe[1], STDERR_FILENO) < 0) {
      djex_report_exec_failure(
          status_pipe[1], DJEX_DESCRIPTOR_EXEC_STAGE_STANDARD_ERROR);
      _exit(127);
    }
    if (fchdir(working_directory_copy) < 0) {
      djex_report_exec_failure(
          status_pipe[1], DJEX_DESCRIPTOR_EXEC_STAGE_WORKING_DIRECTORY);
      _exit(127);
    }
    (void) close(working_directory_copy);
    if (djex_close_child_descriptors(
          executable_copy, status_pipe[1], descriptor_limit) < 0) {
      djex_report_exec_failure(
          status_pipe[1], DJEX_DESCRIPTOR_EXEC_STAGE_CLOSE_DESCRIPTORS);
      _exit(127);
    }

    if (sigprocmask(SIG_SETMASK, &original_signals, NULL) < 0) {
      djex_report_exec_failure(
          status_pipe[1], DJEX_DESCRIPTOR_EXEC_STAGE_SIGNAL_MASK);
      _exit(127);
    }

    (void) syscall(
        SYS_execveat,
        executable_copy,
        "",
        argv,
        envp,
        AT_EMPTY_PATH);
    djex_report_exec_failure(
        status_pipe[1], DJEX_DESCRIPTOR_EXEC_STAGE_EXECVEAT);
    _exit(127);
  }

  (void) close(executable_copy);
  (void) close(working_directory_copy);
  (void) close(input_pipe[0]);
  (void) close(output_pipe[1]);
  (void) close(error_pipe[1]);
  (void) close(status_pipe[1]);
  signal_result = pthread_sigmask(SIG_SETMASK, &original_signals, NULL);
  signals_blocked = 0;
  if (signal_result != 0) {
    int status;
    (void) kill(-pid, SIGKILL);
    (void) kill(pid, SIGKILL);
    while (waitpid(pid, &status, 0) < 0 && errno == EINTR) {
    }
    (void) close(input_pipe[1]);
    (void) close(output_pipe[0]);
    (void) close(error_pipe[0]);
    (void) close(status_pipe[0]);
    errno = signal_result;
    return -1;
  }
  *child_pid = (int) pid;
  *standard_input = input_pipe[1];
  *standard_output = output_pipe[0];
  *standard_error = error_pipe[0];
  *exec_status = status_pipe[0];
  return 0;

failed:
  {
    int saved_errno = errno;
    if (signals_blocked) {
      signal_result = pthread_sigmask(SIG_SETMASK, &original_signals, NULL);
      if (signal_result != 0) {
        saved_errno = signal_result;
      }
    }
    if (executable_copy >= 0) {
      (void) close(executable_copy);
    }
    if (working_directory_copy >= 0) {
      (void) close(working_directory_copy);
    }
    djex_close_pair(input_pipe);
    djex_close_pair(output_pipe);
    djex_close_pair(error_pipe);
    djex_close_pair(status_pipe);
    errno = saved_errno;
    return -1;
  }
}

void djex_z3_abort_descriptor_spawn(int child_pid) {
  pid_t pid = (pid_t) child_pid;
  int status;
  if (pid <= 0) {
    return;
  }
  (void) kill(-pid, SIGKILL);
  (void) kill(pid, SIGKILL);
  while (waitpid(pid, &status, 0) < 0 && errno == EINTR) {
  }
}
