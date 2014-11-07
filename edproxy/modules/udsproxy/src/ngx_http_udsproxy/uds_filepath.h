#ifndef __uds_filepath_h__
#define __uds_filepath_h__

#include <ngx_config.h>
#include <ngx_core.h>
#include <ngx_http.h>

#ifdef __cplusplus
extern "C" {
#endif

    ngx_int_t get_flv_absolue_path(ngx_http_request_t *r, ngx_str_t *host, int port, ngx_str_t *uri, ngx_str_t *getargs, ngx_str_t *ngx_rootpath, ngx_str_t *path);

    int get_flv_real_path(ngx_str_t *host, int port, ngx_str_t *uri, ngx_str_t *getargs, ngx_str_t *filepath);

    ngx_int_t get_uds_file_url(ngx_http_request_t *r, ngx_str_t *host, int port, ngx_str_t *uri, ngx_str_t *getargs, ngx_str_t *url);

#ifdef __cplusplus
}
#endif

#endif // __uds_filepath_h__

