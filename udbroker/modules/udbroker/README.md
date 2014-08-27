udbroder
=======



配置
----

`conf/nginx.conf`

完整的UDS访问地址=http://${uds_host}:${uds_port}${uds_uri}

`uds_host`指定UDS服务器主机地址。

`uds_port`指定UDS服务器端口。

`uds_uri`用于灵活构造UDS访问地址。特殊设置`/miniuds` 用于在没有UDS平台时，模拟UDS访问接口。 


    server {
        listen       18086;

        uds_host localhost;
        uds_port 18086;
        uds_uri /miniuds;

        location ~ /udsfs/uploadfile$ {
            udbroder;

            access_log on;
            limit_rate_after 1m; # rate speed after downloaded 1M.
            limit_rate 100k;     # rate speed to 100k
        }

        location ~ /miniuds$ {
            miniuds;
        }
    }



Nginx插件开发需求
-----------------
 
###描述

