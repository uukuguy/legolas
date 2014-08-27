
/*
 * Copyright (C) Igor Sysoev
 * Copyright (C) Nginx, Inc.
 */

#include <ngx_config.h>
#include <ngx_core.h>
#include <ngx_http.h>
#include "uds_filepath.h"

static char *ngx_http_flvplay(ngx_conf_t *cf, ngx_command_t *cmd, void *conf);
static void *ngx_http_flvplay_create_conf(ngx_conf_t *cf);
static char *ngx_http_flvplay_merge_conf(ngx_conf_t *cf, void *parent, void *child);
static char *ngx_http_uds_host(ngx_conf_t *cf, ngx_command_t *cmd, void *conf);
static char *ngx_http_uds_port(ngx_conf_t *cf, ngx_command_t *cmd, void *conf);
static char *ngx_http_uds_uri(ngx_conf_t *cf, ngx_command_t *cmd, void *conf);

typedef struct {
    ngx_str_t  uds_host;
    ngx_int_t  uds_port;
    ngx_str_t  uds_uri;
} ngx_http_flvplay_conf_t;

static ngx_command_t  ngx_http_flvplay_commands[] = {

    { ngx_string("flvplay"),
      NGX_HTTP_LOC_CONF|NGX_CONF_NOARGS,
      ngx_http_flvplay,
      0,
      0,
      NULL },

    { ngx_string("uds_host"),
      NGX_HTTP_MAIN_CONF|NGX_HTTP_SRV_CONF|NGX_HTTP_LOC_CONF|NGX_CONF_TAKE1,
      ngx_http_uds_host,
      NGX_HTTP_LOC_CONF_OFFSET,
      0,
      NULL },

    { ngx_string("uds_port"),
      NGX_HTTP_MAIN_CONF|NGX_HTTP_SRV_CONF|NGX_HTTP_LOC_CONF|NGX_CONF_TAKE1,
      ngx_http_uds_port,
      NGX_HTTP_LOC_CONF_OFFSET,
      0,
      NULL },

    { ngx_string("uds_uri"),
      NGX_HTTP_MAIN_CONF|NGX_HTTP_SRV_CONF|NGX_HTTP_LOC_CONF|NGX_CONF_TAKE1,
      ngx_http_uds_uri,
      NGX_HTTP_LOC_CONF_OFFSET,
      0,
      NULL },

      ngx_null_command
};


static u_char  ngx_flvplay_header[] = "FLV\x1\x5\0\0\0\x9\0\0\0\0";


static ngx_http_module_t  ngx_http_flvplay_module_ctx = {
    NULL,                          /* preconfiguration */
    NULL,                          /* postconfiguration */

    NULL,                          /* create main configuration */
    NULL,                          /* init main configuration */

    NULL,                          /* create server configuration */
    NULL,                          /* merge server configuration */

    ngx_http_flvplay_create_conf,  /* create location configuration */
    ngx_http_flvplay_merge_conf    /* merge location configuration */
};


ngx_module_t  ngx_http_flvplay_module = {
    NGX_MODULE_V1,
    &ngx_http_flvplay_module_ctx,      /* module context */
    ngx_http_flvplay_commands,         /* module directives */
    NGX_HTTP_MODULE,               /* module type */
    NULL,                          /* init master */
    NULL,                          /* init module */
    NULL,                          /* init process */
    NULL,                          /* init thread */
    NULL,                          /* exit thread */
    NULL,                          /* exit process */
    NULL,                          /* exit master */
    NGX_MODULE_V1_PADDING
};

ngx_int_t
_ngx_http_other_handler(ngx_http_request_t *r, ngx_str_t *p_path);

ngx_int_t
_ngx_http_flvplay_handler(ngx_http_request_t *r, ngx_str_t *p_path);

extern ngx_int_t
_ngx_http_mp4play_handler(ngx_http_request_t *r, ngx_str_t *p_path);

static ngx_int_t
ngx_http_flvplay_handler(ngx_http_request_t *r)
{
    /*u_char                    *last;*/
    /*size_t                     root;*/

    /*off_t                      start, len;*/
    /*ngx_int_t                  rc;*/
    /*ngx_uint_t                 level, i;*/
    /*ngx_str_t                  path, value;*/
    /*ngx_log_t                 *log;*/
    /*ngx_buf_t                 *b;*/
    /*ngx_chain_t                out[2];*/
    /*ngx_open_file_info_t       of;*/
    /*ngx_http_core_loc_conf_t  *clcf;*/
    /*ngx_http_flvplay_conf_t *frcf;*/



    ngx_int_t                  rc;
    ngx_log_t                 *log;
    ngx_str_t                  path;
    ngx_http_core_loc_conf_t  *clcf;
    ngx_http_flvplay_conf_t *frcf;

    /* ********************************************************* */
    /*ngx_str_t host;*/
    /*int port = 18086;*/
    /*ngx_str_t uri;*/
    
    /*ngx_str_set(&host, "localhost");*/
    /*ngx_str_set(&uri, "/miniuds");*/
    /* ********************************************************* */

    log = r->connection->log;

    ngx_log_error(NGX_LOG_ERR, log, 0, "==!!== uri: %V args: %V exten: %V request_line: %V unparsed_uri: %V.", &r->uri, &r->args, &r->exten, &r->request_line, &r->unparsed_uri);

    if (!(r->method & (NGX_HTTP_GET|NGX_HTTP_HEAD))) {
        return NGX_HTTP_NOT_ALLOWED;
    }


    if (r->uri.data[r->uri.len - 1] == '/') {
        return NGX_DECLINED;
    }


    rc = ngx_http_discard_request_body(r);

    if (rc != NGX_OK) {
        return rc;
    }

    clcf = ngx_http_get_module_loc_conf(r, ngx_http_core_module);

    /* ********************************************************* */
    /*last = ngx_http_map_uri_to_path(r, &path, &root, 0);*/
    /*if (last == NULL) {*/
        /*return NGX_HTTP_INTERNAL_SERVER_ERROR;*/
    /*}*/
    /*path.len = last - path.data;*/

    /* --------------------------------------------------------- */

    frcf = ngx_http_get_module_loc_conf(r, ngx_http_flvplay_module);
    ngx_log_error(NGX_LOG_ERR, log, NGX_EACCES, "frcf host: %V port: %d uri: %V", &frcf->uds_host, frcf->uds_port, &frcf->uds_uri);

    if ( r->args.len > 0 ){
        rc = get_flv_absolue_path(r, &frcf->uds_host, frcf->uds_port, &frcf->uds_uri, &r->args, &clcf->root, &path);
        if ( rc != NGX_OK ) {
            return rc;
        }
    } else {
        int filepathlen = r->uri.len - 11;
        u_char* filepath;
        filepath = ngx_palloc(r->pool, filepathlen);
        ngx_cpystrn(filepath, &r->uri.data[12], filepathlen);
        ngx_str_t relative_path;
        relative_path.data = filepath;
        relative_path.len = filepathlen;

        path.len = clcf->root.len + relative_path.len;
        u_char* last_path = ngx_sprintf(path.data, "%V%V", &clcf->root, &relative_path);
        if ( last_path == NULL ){
            return NGX_HTTP_INTERNAL_SERVER_ERROR;
        }
        *last_path = 0;
        ngx_log_error(NGX_LOG_ERR, log, NGX_EACCES, "==xx== filepath: %V", &path);
    }

    /*return _ngx_http_flvplay_handler(r, &path);*/
    if ( path.len >= 5 && 
            path.data[path.len - 3] == 'f' &&
            path.data[path.len - 2] == 'l' &&
            path.data[path.len - 1] == 'v' ) {
        return _ngx_http_flvplay_handler(r, &path);
    } else if ( path.len >= 5 &&
            path.data[path.len - 3] == 'm' &&
            path.data[path.len - 2] == 'p' &&
            path.data[path.len - 1] == '4' ) {
        return _ngx_http_mp4play_handler(r, &path);
    } else {
        /*return NGX_DECLINED;*/
        return _ngx_http_other_handler(r, &path);
    }

}

/* ******************************************************************* */

ngx_int_t
_ngx_http_other_handler(ngx_http_request_t *r, ngx_str_t *p_path){
    off_t                      start, len;
    ngx_int_t                  rc;
    ngx_uint_t                 level, i;
    ngx_str_t                  path;
    ngx_log_t                 *log;
    ngx_buf_t                 *b;
    ngx_chain_t                out[2];
    ngx_open_file_info_t       of;
    ngx_http_core_loc_conf_t  *clcf;
    
    log = r->connection->log;

    path = *p_path;
    ngx_log_debug1(NGX_LOG_DEBUG_HTTP, log, 0,
                   "http flv filename: \"%V\"", &path);

    clcf = ngx_http_get_module_loc_conf(r, ngx_http_core_module);

    ngx_memzero(&of, sizeof(ngx_open_file_info_t));

    of.read_ahead = clcf->read_ahead;
    of.directio = clcf->directio;
    of.valid = clcf->open_file_cache_valid;
    of.min_uses = clcf->open_file_cache_min_uses;
    of.errors = clcf->open_file_cache_errors;
    of.events = clcf->open_file_cache_events;

    if (ngx_http_set_disable_symlinks(r, clcf, &path, &of) != NGX_OK) {
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    if (ngx_open_cached_file(clcf->open_file_cache, &path, &of, r->pool)
        != NGX_OK)
    {
        switch (of.err) {

        case 0:
            return NGX_HTTP_INTERNAL_SERVER_ERROR;

        case NGX_ENOENT:
        case NGX_ENOTDIR:
        case NGX_ENAMETOOLONG:

            level = NGX_LOG_ERR;
            rc = NGX_HTTP_NOT_FOUND;
            break;

        case NGX_EACCES:
#if (NGX_HAVE_OPENAT)
        case NGX_EMLINK:
        case NGX_ELOOP:
#endif

            level = NGX_LOG_ERR;
            rc = NGX_HTTP_FORBIDDEN;
            break;

        default:

            level = NGX_LOG_CRIT;
            rc = NGX_HTTP_INTERNAL_SERVER_ERROR;
            break;
        }

        if (rc != NGX_HTTP_NOT_FOUND || clcf->log_not_found) {
            ngx_log_error(level, log, of.err,
                          "%s \"%s\" failed", of.failed, path.data);
        }

        return rc;
    }

    if (!of.is_file) {

        if (ngx_close_file(of.fd) == NGX_FILE_ERROR) {
            ngx_log_error(NGX_LOG_ALERT, log, ngx_errno,
                          ngx_close_file_n " \"%s\" failed", path.data);
        }

        return NGX_DECLINED;
    }

    r->root_tested = !r->error_page;

    start = 0;
    len = of.size;
    i = 1;

    /*if (r->args.len) {*/

        /*if (ngx_http_arg(r, (u_char *) "start", 5, &value) == NGX_OK) {*/

            /*start = ngx_atoof(value.data, value.len);*/

            /*if (start == NGX_ERROR || start >= len) {*/
                /*start = 0;*/
            /*}*/

            /*if (start) {*/
                /*len = sizeof(ngx_flvplay_header) - 1 + len - start;*/
                /*i = 0;*/
            /*}*/
        /*}*/
    /*}*/

    log->action = "sending other type file to client";

    r->headers_out.status = NGX_HTTP_OK;
    r->headers_out.content_length_n = len;
    r->headers_out.last_modified_time = of.mtime;

    int isHtml = 0;
    if ( path.len >= 6 && 
            path.data[path.len - 4] == 'h' &&
            path.data[path.len - 3] == 't' &&
            path.data[path.len - 2] == 'm' &&
            path.data[path.len - 1] == 'l' ) {
        isHtml = 1;
    }

    if ( path.len >= 5 && 
            path.data[path.len - 3] == 'h' &&
            path.data[path.len - 2] == 't' &&
            path.data[path.len - 1] == 'm' ) {
        isHtml = 1;
    }
    
    if ( isHtml ) {
        ngx_str_t contentType;
        ngx_str_set(&contentType, "text/html" );
        r->headers_out.content_type_len = contentType.len;
        r->headers_out.content_type = contentType;
    } else {
        if ( ngx_http_set_content_type(r) != NGX_OK) {
            return NGX_HTTP_INTERNAL_SERVER_ERROR;
        }
    }

    /*if (i == 0) {*/
        /*b = ngx_pcalloc(r->pool, sizeof(ngx_buf_t));*/
        /*if (b == NULL) {*/
            /*return NGX_HTTP_INTERNAL_SERVER_ERROR;*/
        /*}*/

        /*b->pos = ngx_flvplay_header;*/
        /*b->last = ngx_flvplay_header + sizeof(ngx_flvplay_header) - 1;*/
        /*b->memory = 1;*/

        /*out[0].buf = b;*/
        /*out[0].next = &out[1];*/
    /*}*/


    b = ngx_pcalloc(r->pool, sizeof(ngx_buf_t));
    if (b == NULL) {
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    b->file = ngx_pcalloc(r->pool, sizeof(ngx_file_t));
    if (b->file == NULL) {
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    r->allow_ranges = 1;

    rc = ngx_http_send_header(r);

    if (rc == NGX_ERROR || rc > NGX_OK || r->header_only) {
        return rc;
    }

    b->file_pos = start;
    b->file_last = of.size;

    b->in_file = b->file_last ? 1: 0;
    b->last_buf = (r == r->main) ? 1 : 0;
    b->last_in_chain = 1;

    b->file->fd = of.fd;
    b->file->name = path;
    b->file->log = log;
    b->file->directio = of.is_directio;

    out[1].buf = b;
    out[1].next = NULL;

    return ngx_http_output_filter(r, &out[i]);
}

/* ******************************************************************* */

ngx_int_t
_ngx_http_flvplay_handler(ngx_http_request_t *r, ngx_str_t *p_path){

    /*u_char                    *last;*/
    /*size_t                     root;*/

    off_t                      start, len;
    ngx_int_t                  rc;
    ngx_uint_t                 level, i;
    ngx_str_t                  path, value;
    ngx_log_t                 *log;
    ngx_buf_t                 *b;
    ngx_chain_t                out[2];
    ngx_open_file_info_t       of;
    ngx_http_core_loc_conf_t  *clcf;
    
    log = r->connection->log;

    path = *p_path;
    ngx_log_debug1(NGX_LOG_DEBUG_HTTP, log, 0,
                   "http flv filename: \"%V\"", &path);

    clcf = ngx_http_get_module_loc_conf(r, ngx_http_core_module);

    ngx_memzero(&of, sizeof(ngx_open_file_info_t));

    of.read_ahead = clcf->read_ahead;
    of.directio = clcf->directio;
    of.valid = clcf->open_file_cache_valid;
    of.min_uses = clcf->open_file_cache_min_uses;
    of.errors = clcf->open_file_cache_errors;
    of.events = clcf->open_file_cache_events;

    if (ngx_http_set_disable_symlinks(r, clcf, &path, &of) != NGX_OK) {
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    if (ngx_open_cached_file(clcf->open_file_cache, &path, &of, r->pool)
        != NGX_OK)
    {
        switch (of.err) {

        case 0:
            return NGX_HTTP_INTERNAL_SERVER_ERROR;

        case NGX_ENOENT:
        case NGX_ENOTDIR:
        case NGX_ENAMETOOLONG:

            level = NGX_LOG_ERR;
            rc = NGX_HTTP_NOT_FOUND;
            break;

        case NGX_EACCES:
#if (NGX_HAVE_OPENAT)
        case NGX_EMLINK:
        case NGX_ELOOP:
#endif

            level = NGX_LOG_ERR;
            rc = NGX_HTTP_FORBIDDEN;
            break;

        default:

            level = NGX_LOG_CRIT;
            rc = NGX_HTTP_INTERNAL_SERVER_ERROR;
            break;
        }

        if (rc != NGX_HTTP_NOT_FOUND || clcf->log_not_found) {
            ngx_log_error(level, log, of.err,
                          "%s \"%s\" failed", of.failed, path.data);
        }

        return rc;
    }

    if (!of.is_file) {

        if (ngx_close_file(of.fd) == NGX_FILE_ERROR) {
            ngx_log_error(NGX_LOG_ALERT, log, ngx_errno,
                          ngx_close_file_n " \"%s\" failed", path.data);
        }

        return NGX_DECLINED;
    }

    r->root_tested = !r->error_page;

    start = 0;
    len = of.size;
    i = 1;

    if (r->args.len) {

        if (ngx_http_arg(r, (u_char *) "start", 5, &value) == NGX_OK) {

            start = ngx_atoof(value.data, value.len);

            if (start == NGX_ERROR || start >= len) {
                start = 0;
            }

            if (start) {
                len = sizeof(ngx_flvplay_header) - 1 + len - start;
                i = 0;
            }
        }
    }

    log->action = "sending flv to client";

    r->headers_out.status = NGX_HTTP_OK;
    r->headers_out.content_length_n = len;
    r->headers_out.last_modified_time = of.mtime;

    if (ngx_http_set_content_type(r) != NGX_OK) {
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    if (i == 0) {
        b = ngx_pcalloc(r->pool, sizeof(ngx_buf_t));
        if (b == NULL) {
            return NGX_HTTP_INTERNAL_SERVER_ERROR;
        }

        b->pos = ngx_flvplay_header;
        b->last = ngx_flvplay_header + sizeof(ngx_flvplay_header) - 1;
        b->memory = 1;

        out[0].buf = b;
        out[0].next = &out[1];
    }


    b = ngx_pcalloc(r->pool, sizeof(ngx_buf_t));
    if (b == NULL) {
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    b->file = ngx_pcalloc(r->pool, sizeof(ngx_file_t));
    if (b->file == NULL) {
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    r->allow_ranges = 1;

    rc = ngx_http_send_header(r);

    if (rc == NGX_ERROR || rc > NGX_OK || r->header_only) {
        return rc;
    }

    b->file_pos = start;
    b->file_last = of.size;

    b->in_file = b->file_last ? 1: 0;
    b->last_buf = (r == r->main) ? 1 : 0;
    b->last_in_chain = 1;

    b->file->fd = of.fd;
    b->file->name = path;
    b->file->log = log;
    b->file->directio = of.is_directio;

    out[1].buf = b;
    out[1].next = NULL;

    return ngx_http_output_filter(r, &out[i]);
}


static void *
ngx_http_flvplay_create_conf(ngx_conf_t *cf)
{
    ngx_http_flvplay_conf_t  *conf;

    conf = ngx_pcalloc(cf->pool, sizeof(ngx_http_flvplay_conf_t));
    if (conf == NULL) {
        return NULL;
    }

    /*
     * set by ngx_pcalloc():
     *
     *     conf->uds_host = { 0, NULL };
     *     conf->uds_port = 0;
     *     conf->uds_uri = { 0, NULL };
     */

    /*conf->vars = NGX_CONF_UNSET_PTR;*/

    return conf;
}


static char *
ngx_http_flvplay_merge_conf(ngx_conf_t *cf, void *parent, void *child)
{
    ngx_http_flvplay_conf_t *prev = parent;
    ngx_http_flvplay_conf_t *conf = child;

    ngx_conf_merge_str_value(conf->uds_host, prev->uds_host, "localhost");
    if ( conf->uds_port == 0 ) {
        if ( prev->uds_port > 0 ) {
            conf->uds_port = prev->uds_port;
        }
    }
    ngx_conf_merge_str_value(conf->uds_uri, prev->uds_uri, "");

    return NGX_CONF_OK;
}

static char *
ngx_http_uds_host(ngx_conf_t *cf, ngx_command_t *cmd, void *conf)
{
    ngx_http_flvplay_conf_t *frcf = conf;

    ngx_str_t        *value;

    if (frcf->uds_host.data != NULL) {
        return "uds_host is duplicate";
    }

    value = cf->args->elts;

    if (value[1].len == 0) {
        frcf->uds_host.len = 0;
        frcf->uds_host.data = (u_char*)0;
    } else 
        frcf->uds_host = value[1];

    return NGX_CONF_OK;
}


static char *
ngx_http_uds_port(ngx_conf_t *cf, ngx_command_t *cmd, void *conf)
{
    ngx_http_flvplay_conf_t *frcf = conf;

    ngx_str_t        *value;

    value = cf->args->elts;

    if (value[1].len == 0) {
        frcf->uds_port = 0;
    } else
        frcf->uds_port = atoi((const char*)value[1].data);
        /*frcf->uds_port = value[1];*/

    return NGX_CONF_OK;
}

static char *
ngx_http_uds_uri(ngx_conf_t *cf, ngx_command_t *cmd, void *conf)
{
    ngx_http_flvplay_conf_t *frcf = conf;

    ngx_str_t        *value;

    if (frcf->uds_uri.data != NULL) {
        return "uds_uri is duplicate";
    }

    value = cf->args->elts;

    if (value[1].len == 0) {
        frcf->uds_uri.len = 0;
        frcf->uds_uri.data = (u_char*)0;
    } else
        frcf->uds_uri = value[1];
    

    return NGX_CONF_OK;
}

static char *
ngx_http_flvplay(ngx_conf_t *cf, ngx_command_t *cmd, void *conf)
{
    ngx_http_core_loc_conf_t  *clcf;

    clcf = ngx_http_conf_get_module_loc_conf(cf, ngx_http_core_module);
    clcf->handler = ngx_http_flvplay_handler;

    return NGX_CONF_OK;
}
