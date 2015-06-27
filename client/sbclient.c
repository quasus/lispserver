#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>

#define BUFSIZE 1024
#define STRSIZE 256

int strlen_utf8(char *s) 
{
    int i = 0, j = 0;
    while (s[i]) 
    {
        if ((s[i] & 0xc0) != 0x80) j++;
        i++;
    }
    return j;
}

void do_msg(int fdin, int fdout)
{
    char buf[BUFSIZE];
    unsigned int len;

    int flags = fcntl(fdin, F_GETFL, 0);
    fcntl(fdin, F_GETFL, flags | (~O_NONBLOCK));

    read(fdin, buf, 6);
    buf[6] = '\0';
    sscanf(buf, "%x", &len);
    for(; len >= BUFSIZE; len -= BUFSIZE) {
        read(fdin, buf, BUFSIZE);
        write(fdout, buf, BUFSIZE);
    }
    read(fdin, buf, len);
    write(fdout, buf, len);
    fcntl(fdin, F_GETFL, flags);
}

int do_return_msg(int fd)
{
    int flags = fcntl(fd, F_GETFL, 0);
    fcntl(fd, F_GETFL, flags | (~O_NONBLOCK));

    char c;
    read(fd, &c, 1);

    fcntl(fd, F_GETFL, flags);

    return c;
}

int main(int argc, char* argv[])
{
    if (argc == 1) {
        fprintf(stderr, "socket needed\n");
        return 1;
    }

    char *socket_file = argv[1];

    int i;
    char *msg;
    size_t msg_len;
    size_t msg_len_utf8;
    char s_msg_len[6];

    char buf[BUFSIZE];

    int socket_fd;
    struct sockaddr_un name;

    fd_set read_fds;

    socket_fd = socket(PF_LOCAL, SOCK_STREAM, 0);
    name.sun_family = AF_LOCAL;
    strcpy(name.sun_path, socket_file);
    if (connect(socket_fd, (struct sockaddr *) &name, SUN_LEN(&name)) == -1) {
        perror(argv[0]);
    }

    int flags = fcntl(socket_fd, F_GETFL, 0);
    fcntl(socket_fd, F_GETFL, flags | O_NONBLOCK);
    
    flags = fcntl(0, F_GETFL, 0);
    fcntl(0, F_GETFL, flags | O_NONBLOCK);

    /* Protocol: send the number of strings, then send the strings prepending their sizes */

    sprintf(s_msg_len, "%06x", argc - 2);
    write(socket_fd, s_msg_len, 6);

    for(i = 2; i < argc; ++i) {
        msg = argv[i];
        msg_len = strlen(msg);
        msg_len_utf8 = strlen_utf8(msg);
        sprintf(s_msg_len, "%06x", msg_len_utf8);
        write(socket_fd, s_msg_len, 6);
        write(socket_fd, msg, msg_len);
    }
    fflush(NULL);

    int s = 0;
    int n = 0;

    while (1) {

        FD_ZERO(&read_fds);
        FD_SET(0, &read_fds);
        FD_SET(socket_fd, &read_fds);

        s = select(FD_SETSIZE, &read_fds, NULL, NULL, NULL);

     if (s < 0) {
            break;
        } else if (s == 0) {
            continue;
        }

        if (FD_ISSET(socket_fd, &read_fds)) {
            n = read(socket_fd, buf, 1);

            if (n == 0) {
                break;
            }
            switch (*buf) {
                case 'o':
                    do_msg(socket_fd, 1);
                    break;
                case 'e':
                    do_msg(socket_fd, 2);
                    break;
                case 'r':
                    return do_return_msg(socket_fd);
                default:
                    fprintf(stderr, "Protocol error.\n");
                    return -1;
            }
        }

        if (FD_ISSET(0, &read_fds)) {
            n = read(0, buf, BUFSIZE);
            write(socket_fd, buf, n);
            fflush(NULL);
            if (n == 0) {
                break;
            }
        }

    }

    close (socket_fd);
    return 0;
}
