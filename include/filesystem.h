/**
 * @file   filesystem.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-05-19 19:24:38
 * 
 * @brief  
 * 
 * 
 */

#ifndef __FILESYSTEM_H__
#define __FILESYSTEM_H__

extern int file_exist(const char *filename);
extern int mkdir_if_not_exist(const char *dirname);
extern int get_instance_parent_full_path(char* apath, int size);
extern int get_file_parent_full_path(const char *filename, char *apath, int size);
extern int get_path_file_name(const char *path_name, char *file_name, int size);

#endif /* __FILESYSTEM_H__ */

