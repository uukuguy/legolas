/** 
 * @file   udfsmount.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   Fri Mar  2 19:19:53 2012
 * 
 * @brief  
 * 
 * 
 */

#define FUSE_USE_VERSION 26
#define _FILE_OFFSET_BITS 64
#include <fuse.h>

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/time.h>

#include "common.h"
#include "logger.h"
#include "cboost.h"
#include "everdata.h"

/* static const char *udfs_str = "Hello World!\n"; */

static const char *udfs_path = "/hello"; 
static const char *tmp_path = "/tmp"; 

static const char *g_current_path = NULL;

static void* edfs_init(struct fuse_conn_info *conn)
{
    syslog(LOG_DEBUG, "edfs_init()"); 

    void* private_data = NULL; 

    return private_data; 
}

static void edfs_destroy(void *private_data)
{
    syslog(LOG_DEBUG, "edfs_destroy()"); 
}

/*
struct stat {
    dev_t         st_dev;       //文件的设备编号
    ino_t         st_ino;       //节点
    mode_t        st_mode;      //文件的类型和存取的权限
    nlink_t       st_nlink;     //连到该文件的硬连接数目，刚建立的文件值为1
    uid_t         st_uid;       //用户ID
    gid_t         st_gid;       //组ID
    dev_t         st_rdev;      //(设备类型)若此文件为设备文件，则为其设备编号
    off_t         st_size;      //文件字节数(文件大小)
    unsigned long st_blksize;   //块大小(文件系统的I/O 缓冲区大小)
    unsigned long st_blocks;    //块数
    time_t        st_atime;     //最后一次访问时间
    time_t        st_mtime;     //最后一次修改时间
    time_t        st_ctime;     //最后一次改变时间(指属性)
};

S_IFMT   0170000    文件类型的位遮罩
    S_IFSOCK 0140000    scoket
    S_IFLNK 0120000     符号连接
    S_IFREG 0100000     一般文件
    S_IFBLK 0060000     区块装置
    S_IFDIR 0040000     目录
    S_IFCHR 0020000     字符装置
    S_IFIFO 0010000     先进先出

    S_ISUID 04000     文件的(set user-id on execution)位
    S_ISGID 02000     文件的(set group-id on execution)位
    S_ISVTX 01000     文件的sticky位

    S_IRUSR(S_IREAD) 00400     文件所有者具可读取权限
    S_IWUSR(S_IWRITE)00200     文件所有者具可写入权限
    S_IXUSR(S_IEXEC) 00100     文件所有者具可执行权限

    S_IRGRP 00040             用户组具可读取权限
    S_IWGRP 00020             用户组具可写入权限
    S_IXGRP 00010             用户组具可执行权限

    S_IROTH 00004             其他用户具可读取权限
    S_IWOTH 00002             其他用户具可写入权限
    S_IXOTH 00001             其他用户具可执行权限

  上述的文件类型在POSIX中定义了检查这些类型的宏定义：
    S_ISLNK (st_mode)    判断是否为符号连接
    S_ISREG (st_mode)    是否为一般文件
    S_ISDIR (st_mode)    是否为目录
    S_ISCHR (st_mode)    是否为字符装置文件
    S_ISBLK (s3e)        是否为先进先出
    S_ISSOCK (st_mode)   是否为socket    
*/

static int udfs_getattr(const char *path, struct stat *stbuf)
{
     syslog(LOG_DEBUG, "edfs_getattr() : %s", path); 

    int res = 0; 

     memset(stbuf, 0, sizeof(struct stat)); 

     if(strcmp(path, "/") == 0) { 
         stbuf->st_mode = S_IFDIR | 0755; 
         stbuf->st_nlink = 2; 
     } 
     else if(strcmp(path, tmp_path) == 0) { 
          stbuf->st_mode = S_IFREG | 0444; 
          stbuf->st_nlink = 1; 
          stbuf->st_size = strlen(path); 
     } 
     else {
         stbuf->st_mode = S_IFDIR | 0755; 
         stbuf->st_nlink = 2; 
     }
     /*else */
          /*res = -ENOENT; */

    return res; 
}

/** Open directory
 *
 * Unless the 'default_permissions' mount option is given,
 * this method should check if opendir is permitted for this
 * directory. Optionally opendir may also return an arbitrary
 * filehandle in the fuse_file_info structure, which will be
 * passed to readdir, closedir and fsyncdir.
 *
 * Introduced in version 2.3
 */
static int edfs_opendir(const char *path, struct fuse_file_info *fi)
{
    syslog(LOG_INFO, "edfs_opendir() : %s", path);

    int ret = 0;

    g_current_path = path;
    /*if ( opendir(path) == NULL )*/
    /*return -errno;*/

    return ret;
}

/** Release directory
 *
 * Introduced in version 2.3
 */
static int edfs_releasedir(const char *path, struct fuse_file_info *fi)
{
    syslog(LOG_DEBUG, "edfs_releasedir() : %s", path);

     int ret = 0;

     g_current_path = NULL;

     return ret;
}

static int edfs_readdir(const char *path, void *buf, fuse_fill_dir_t filler,
                        off_t offset, struct fuse_file_info *fi)
{
    syslog(LOG_DEBUG, "edfs_readdir() : %s", path); 

    filler(buf, ".", NULL, 0); 
    filler(buf, "..", NULL, 0); 

    filler(buf, udfs_path + 1, NULL, 0); 
    filler(buf, tmp_path + 1, NULL, 0); 

    return 0; 
}

/** Synchronize directory contents
 *
 * If the datasync parameter is non-zero, then only the user data
 * should be flushed, not the meta data
 *
 * Introduced in version 2.3
 */
static int edfs_fsyncdir(const char *path, int parm, struct fuse_file_info *fi)
{
    syslog(LOG_DEBUG, "edfs_fsyncdir() : %s", path);

    int ret = 0;
    return ret;
}

static int edfs_open(const char *path, struct fuse_file_info *fi)
{
    syslog(LOG_INFO, "edfs_open() : %s", path); 

    /*  if(strcmp(path, udfs_path) != 0) */
    /*       return -ENOENT; */

    /*  if((fi->flags & 3) != O_RDONLY) */
    /*       return -EACCES; */

    return 0; 
}

static int edfs_read(const char *path, char *buf, size_t size, off_t offset,
                     struct fuse_file_info *fi)
{
    syslog(LOG_DEBUG, "edfs_read() : %s size : %zu", path, size); 

    /*size_t len = 0; */
    /* (void) fi; */
    /* if(strcmp(path, udfs_path) != 0) */
    /*      return -ENOENT; */

    /* len = strlen(udfs_str); */
    /* if (offset < len) { */
    /*      if (offset + size > len) */
    /*           size = len - offset; */
    /*      memcpy(buf, udfs_str + offset, size); */
    /* } else */
    /*      size = 0; */

    return size; 
}

/** Write data to an open file
 *
 * Write should return exactly the number of bytes requested
 * except on error.	 An exception to this is when the 'direct_io'
 * mount option is specified (see read operation).
 *
 * Changed in version 2.2
 */
static int edfs_write(const char *path, const char *buf, size_t size, off_t offset,
		      struct fuse_file_info *fi)
{
    syslog(LOG_DEBUG, "edfs_write() : %s size : %zu", path, size); 

    return size; 
}

static int edfs_flush(const char *path, struct fuse_file_info *fi)
{
     
    syslog(LOG_DEBUG, "edfs_flush() : %s", path); 

    int ret = 0; 
    return ret; 
}

/** Release an open file
 *
 * Release is called when there are no more references to an open
 * file: all file descriptors are closed and all memory mappings
 * are unmapped.
 *
 * For every open() call there will be exactly one release() call
 * with the same flags and file descriptor.	 It is possible to
 * have a file opened more than once, in which case only the last
 * release will mean, that no more reads/writes will happen on the
 * file.  The return value of release is ignored.
 *
 * Changed in version 2.2
 */
static int edfs_release(const char *path, struct fuse_file_info *fi)
{
     syslog(LOG_DEBUG, "edfs_release() : %s", path);

     int ret = 0;
     return ret;
}

static int edfs_unlink(const char *filename)
{
    syslog(LOG_DEBUG, "udfs_unlink() : %s", filename);

    int ret = 0;
    return ret;
}

static int edfs_mknod(const char *path, mode_t mode, dev_t rdev)
{
    syslog(LOG_DEBUG, "edfs_mknod() : %s", path);

    int res = 0;

    /* On Linux this could just be 'mknod(path, mode, rdev)' but this
       is more portable */
    /* if (S_ISREG(mode)) { */
    /*      res = open(path, O_CREAT | O_EXCL | O_WRONLY, mode); */
    /*      if (res >= 0) */
    /*           res = close(res); */
    /* } else if (S_ISFIFO(mode)) */
    /*      res = mkfifo(path, mode); */
    /* else */
    /*      res = mknod(path, mode, rdev); */
    /* if (res == -1) */
    /*      return -errno; */

    return res;
}

static int edfs_mkdir(const char *path, mode_t path_mode)
{
     syslog(LOG_DEBUG, "edfs_mkdir() : %s", path); 

    int ret = 0; 
    /* path_mode |= S_IFDIR; */

    return ret; 
}

static int edfs_rmdir(const char *path)
{
    syslog(LOG_DEBUG, "edfs_rmdir() : %s", path); 

    int ret = 0; 
    return ret; 
}

static int edfs_rename(const char *oldpath, const char *newpath)
{
    syslog(LOG_DEBUG, "udfs_rename() from : %s to : %s", oldpath, newpath);

    int ret = 0;
    return ret;
}

static int edfs_symlink(const char *path, const char *linkname) 
{ 
    int ret = 0; 
    return ret; 
} 

static int edfs_readlink(const char *linkname, char *buf, size_t size) 
{ 
    int ret = 0; 
    return ret; 
} 


static int edfs_chmod(const char *path, mode_t file_mode)
{
    syslog(LOG_DEBUG, "edfs_chmod() : %s", path);

    int ret = 0;
    return ret;
}

static int edfs_chown(const char *path, uid_t uid, gid_t gid)
{
    syslog(LOG_DEBUG, "edfs_chown() : %s", path);

    int ret = 0;
    return ret;
}

static struct fuse_operations udfs_oper = {
    .init       = edfs_init,
    .destroy    = edfs_destroy,

    .getattr	= udfs_getattr,

    .opendir    = edfs_opendir, 
    .readdir	= edfs_readdir,
    .releasedir = edfs_releasedir, 
    .fsyncdir   = edfs_fsyncdir, 

    .open	    = edfs_open,
    .read	    = edfs_read,
    .write      = edfs_write,
    .flush      = edfs_flush,
    .release    = edfs_release,

    .unlink     = edfs_unlink, 
    .mknod		= edfs_mknod,

    .mkdir      = edfs_mkdir,
    .rmdir      = edfs_rmdir,

    .rename     = edfs_rename, 
    .symlink    = edfs_symlink, 
    .readlink   = edfs_readlink, 
    .chmod      = edfs_chmod, 
    .chown      = edfs_chown, 
};

int main(int argc, char *argv[])
{
    int ret = fuse_main(argc, argv, &udfs_oper, NULL);

    return ret;
}

