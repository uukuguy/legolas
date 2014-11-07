
#include "uds_filepath.h"

#ifdef __cplusplus
extern "C" {
#endif

    char* get_uds_filepath(const char* host, int port, const char* uri, const char* getargs, char** filepath); 

    int get_flv_real_path(ngx_str_t *host, int port, ngx_str_t *uri, ngx_str_t *getargs, ngx_str_t *filepath)
    {
        char* last;
        last = get_uds_filepath((const char*)host->data, port, (const char*)uri->data, (const char*)getargs->data, (char **)&filepath->data); 
        if ( last != NULL ){
            filepath->len = last - (char*)filepath->data;
            return 1;
        } else
            return 0;
    }

    ngx_int_t get_flv_absolue_path(ngx_http_request_t *r, ngx_str_t *host, int port, ngx_str_t *uri, ngx_str_t *getargs, ngx_str_t *ngx_rootpath, ngx_str_t *path)
    {
        ngx_log_t *log;
        ngx_str_t relative_path;
        u_char *last_path;

        log = r->connection->log;
        if (r->args.len) {

            get_flv_real_path(host, port, uri, getargs, &relative_path);
            ngx_log_error(NGX_LOG_ERR, log, NGX_EACCES, "Got flv relative path: %V", &relative_path);

            if ( relative_path.len > 0 ) {
                path->len = ngx_rootpath->len + relative_path.len;
                path->data = (u_char*)ngx_pnalloc(r->pool, path->len);
                if ( path->data == NULL ) {
                    return NGX_HTTP_INTERNAL_SERVER_ERROR;
                }
                last_path = ngx_sprintf(path->data, "%V%V", ngx_rootpath, &relative_path);
                if ( last_path == NULL ){
                    return NGX_HTTP_INTERNAL_SERVER_ERROR;
                }
                *last_path = 0;
                ngx_log_error(NGX_LOG_ERR, log, NGX_EACCES, "Got flv absolute path: %V", path);
                return NGX_OK;
            } else {
                ngx_log_error(NGX_LOG_ERR, log, NGX_EACCES, "Got relative path = NULL. host: %V port: %d uri: %V getargs: %V", host, port, uri, getargs); 
                return NGX_HTTP_INTERNAL_SERVER_ERROR;
            }
        } else {
            ngx_log_error(NGX_LOG_ERR, log, NGX_EACCES, "Cann't get flv relative path. host: %V port: %d uri: %V getargs: %V", host, port, uri, getargs); 
            return NGX_HTTP_INTERNAL_SERVER_ERROR;
        }
    }

    ngx_int_t get_uds_file_url(ngx_http_request_t *r, ngx_str_t *host, int port, ngx_str_t *uri, ngx_str_t *getargs, ngx_str_t *url)
    {
        ngx_log_t *log;
        ngx_str_t relative_path;
        u_char *last_path;

        ngx_str_t myhost;
        ngx_str_set(&myhost, "");

        log = r->connection->log;
        if (r->args.len) {

            get_flv_real_path(host, port, uri, getargs, &relative_path);
            ngx_log_error(NGX_LOG_ERR, log, NGX_EACCES, "Got uds file relative path: %V", &relative_path);

            if ( relative_path.len > 0 ) {
                /*url->len = 7 + myhost.len  + relative_path.len;*/
                url->len = myhost.len  + relative_path.len;
                url->data = (u_char*)ngx_pnalloc(r->pool, url->len);
                if ( url->data == NULL ) {
                    return NGX_HTTP_INTERNAL_SERVER_ERROR;
                }
                /*last_path = ngx_sprintf(url->data, "http://%V%V", &myhost, &relative_path);*/
                last_path = ngx_sprintf(url->data, "%V", &relative_path);
                if ( last_path == NULL ){
                    return NGX_HTTP_INTERNAL_SERVER_ERROR;
                }
                *last_path = 0;
                ngx_log_error(NGX_LOG_ERR, log, NGX_EACCES, "Got uds file url : %V", url);
                return NGX_OK;
            } else {
                ngx_log_error(NGX_LOG_ERR, log, NGX_EACCES, "Got relative path = NULL. host: %V port: %d uri: %V getargs: %V", host, port, uri, getargs); 
                return NGX_HTTP_INTERNAL_SERVER_ERROR;
            }
        } else {
            ngx_log_error(NGX_LOG_ERR, log, NGX_EACCES, "Cann't get uds file url. host: %V port: %d uri: %V getargs: %V", host, port, uri, getargs); 
            return NGX_HTTP_INTERNAL_SERVER_ERROR;
        }
    }
#ifdef __cplusplus
} // extern "C"
#endif
