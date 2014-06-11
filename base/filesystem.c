/**
 * @file   filesystem.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-05-19 19:24:38
 * 
 * @brief  
 * 
 * 
 */

#include "filesystem.h"
#include "common.h"
#include "logger.h"

int mkdir_if_not_exist(const char *dirname)
{
    if ( access(dirname, F_OK) == -1 ){
        return mkdir(dirname, 0750);
    }
    return 0;
}

int get_file_parent_full_path(const char *filename, char *apath, int size)
{
    int cnt = strlen(filename);

    if ( cnt > 0 ) {
        int i;
        for ( i = cnt ; i >= 0 ; i-- ){
            if ( filename[i] == '/' ) {
                memcpy(apath, filename, i);
                apath[i] = '\0';
                break;
            }
        }
        return i;
    }
    return 0;
}

/**
 * apath must bin exe filename type: .../bin/foo
 */
int get_instance_parent_full_path(char* apath, int size)
{
    int cnt;
#ifdef OS_LINUX
    cnt = readlink("/proc/self/exe", apath, size);
#endif

#ifdef OS_DARWIN
    realpath("./", apath);
    strcat(apath, "/bin/foo");
    cnt = strlen(apath);
#endif

    if ( cnt > 0 ) {
        int i;
        for ( i = cnt ; i >= 0 ; i-- ){
            if ( apath[i] == '/' ) {
                break;
            }
        }
        int j;
        for ( j = i - 1 ; j >= 0 ; j-- ){
            if ( apath[j] == '/' ) {
                apath[j] = '\0';
                break;
            }
        }
        return 0;
    } else {
        error_log("%d\n%d\n%s\n", cnt, errno, apath);
        return -1;
    }
}


