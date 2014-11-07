
/*
 * Copyright (C) Igor Sysoev
 * Copyright (C) Nginx, Inc.
 */

#include <ngx_config.h>
#include <ngx_core.h>
#include <ngx_http.h>

static char *ngx_http_miniuds(ngx_conf_t *cf, ngx_command_t *cmd, void *conf);

static ngx_command_t  ngx_http_miniuds_commands[] = {

    { ngx_string("miniuds"),
      NGX_HTTP_LOC_CONF|NGX_CONF_NOARGS,
      ngx_http_miniuds,
      0,
      0,
      NULL },

      ngx_null_command
};


static ngx_http_module_t  ngx_http_miniuds_module_ctx = {
    NULL,                          /* preconfiguration */
    NULL,                          /* postconfiguration */

    NULL,                          /* create main configuration */
    NULL,                          /* init main configuration */

    NULL,                          /* create server configuration */
    NULL,                          /* merge server configuration */

    NULL,                          /* create location configuration */
    NULL                           /* merge location configuration */
};


ngx_module_t  ngx_http_miniuds_module = {
    NGX_MODULE_V1,
    &ngx_http_miniuds_module_ctx,      /* module context */
    ngx_http_miniuds_commands,         /* module directives */
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

static ngx_int_t
ngx_http_miniuds_handler(ngx_http_request_t *r)
{
    ngx_buf_t *b;
    ngx_chain_t out;
    ngx_str_t relative_path;

    int bGetHtml = 0;

    ngx_log_error(NGX_LOG_ERR, r->connection->log, 0, "==!!== uri: %V args: %V exten: %V request_line: %V unparsed_uri: %V.", &r->uri, &r->args, &r->exten, &r->request_line, &r->unparsed_uri);
    if (r->args.len) {
        if ( ngx_strstrn(r->args.data, "htmlId", 5) != 0 ){
                bGetHtml = 1;
        }
    }

    if ( bGetHtml == 1 ) {
        ngx_str_set(&relative_path, "{\"success\":true,\"media_relative_path\":\"/media/rrtd/enter.htm\"}");
        /*ngx_str_set(&relative_path, "{\"success\":true,\"relative_path\":\"/media/index.html\"}");*/
    } else {
        ngx_str_set(&relative_path, "{\"success\":true,\"media_relative_path\":\"/media/spring.flv\"}");
        /*ngx_str_set(&relative_path, "{\"success\":true,\"relative_path\":\"/media/test.mp4\"}");*/
        /*ngx_str_set(&relative_path, "{\"success\":false,\"error_info\":\"flv relative path error. 错误信息\"}");*/
    }
 
    /*ngx_http_core_loc_conf_t *elcf;*/
    /*elcf = ngx_http_get_module_loc_conf(r, ngx_http_core_module);*/
 
    if(!(r->method & (NGX_HTTP_GET)))
    {
        return NGX_HTTP_NOT_ALLOWED;
    }
 
    b = ngx_pcalloc(r->pool, sizeof(ngx_buf_t));
    if(b == NULL)
    {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0, "Failed to allocate response buffer.");
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }
    out.buf = b;
    out.next = NULL;
 
    b->pos = relative_path.data;
    b->last = relative_path.data + (relative_path.len);
    b->memory = 1;
    b->last_buf = 1;

    return ngx_http_output_filter(r, &out);
}

static char *
ngx_http_miniuds(ngx_conf_t *cf, ngx_command_t *cmd, void *conf)
{
    ngx_http_core_loc_conf_t  *clcf;

    clcf = ngx_http_conf_get_module_loc_conf(cf, ngx_http_core_module);
    clcf->handler = ngx_http_miniuds_handler;

    return NGX_CONF_OK;
}
