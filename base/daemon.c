/**
 * @file   daemon.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-04-30 22:07:49
 * 
 * @brief  
 * 
 * 
 */

#include "common.h"
#include "daemon.h"

int daemon_fork_base(void)
{
     /* Forking the parent process. */
     pid_t pid;
     pid = fork();
     if ( pid < 0 ){
          syslog(LOG_ERR, "Fork error: %s", strerror(errno));
          exit(EXIT_FAILURE);
     }
 
     /* Exit the parent process. */
     if ( pid > 0 ){
          /* exit(EXIT_SUCCESS); */
          return pid;
     }

     /* Changing the file mode mask (umask) */
     umask(0);
     

     /* Opening logs for writing. */


     /* Creating a unique session id (SID). */
     pid_t sid;
     sid = setsid();
     if ( sid < 0 ){
          syslog(LOG_ERR, "Creating unique session id error: %s", strerror(errno));
          exit(EXIT_FAILURE);
     }

     /* Changing the working directory. */
     if ( chdir("/") < 0 ){
          syslog(LOG_ERR, "Changing the working directory / error: %s", strerror(errno));
          exit(EXIT_FAILURE);
     }


     /* Closing standard file descriptors. */
     close(STDIN_FILENO);
     close(STDOUT_FILENO);
     close(STDERR_FILENO);

     return 0;
}

int daemon_fork(DAEMON_LOOP daemon_loop, void *data)
{
    int pid = daemon_fork_base();
    if ( pid == 0 ){
        daemon_loop(data);
    }
    return pid;
}

int daemon_fork2(DAEMON_LOOP daemon_loop1, DAEMON_LOOP daemon_loop2, void *data)
{
     if ( daemon_fork(daemon_loop1, data) > 0 ){
          if ( daemon_fork(daemon_loop2, data) > 0 ){
               exit(EXIT_SUCCESS);
          }
     }
     return 0;
}

int daemon_fork3(DAEMON_LOOP daemon_loop1, DAEMON_LOOP daemon_loop2, DAEMON_LOOP daemon_loop3, void *data)
{
     if ( daemon_fork2(daemon_loop1, daemon_loop2, data) > 0 ){
          if ( daemon_fork(daemon_loop3, data) > 0 ){
               exit(EXIT_SUCCESS);
          }
     }
     return 0;
}

int daemon_fork4(DAEMON_LOOP daemon_loop1, DAEMON_LOOP daemon_loop2, DAEMON_LOOP daemon_loop3, DAEMON_LOOP daemon_loop4, void *data)
{
     if ( daemon_fork3(daemon_loop1, daemon_loop2, daemon_loop3, data) > 0 ){
          if ( daemon_fork(daemon_loop4, data) > 0 ){
               exit(EXIT_SUCCESS);
          }
     }
     return 0;
}

int daemon_forks(DAEMON_LOOP daemon_loops[], size_t n, void *data)
{
    size_t i = 0;
    for ( i = 0 ; i < n ; i++ ){
        if ( daemon_fork(daemon_loops[i], data) > 0 ){
            continue;
        };
        exit(EXIT_SUCCESS);
    };

    return 0;
}
