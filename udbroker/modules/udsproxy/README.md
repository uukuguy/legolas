udsproxy
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

        location ~ /udsproxy$ {
            udsproxy;

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

业务系统请求Nginx服务器上的服务，地址如

    http://localhost:18086/Contextpath/udsproxy?sysCheckNo=123%26documentId=test%26versionId=1
    
其中，udsproxy是Nginx服务器提供的请求服务。开发udsproxy请求服务，通过该服务解析相应的请求参数，封装请求参数成json串格式

    {"sysCheckNo":"系统验证码","documentId":"文档ID","versionId":"版本ID"}

通过调用非结构化平台提供的Http接口(post方式)，获取该flv在Nginx上的相对路径，
如`/flv/00/0b/01.flv`，由此请求服务组装该flv在Nginx上的实际路径
`http://IP:Port/Contextpath/flv/00/0b/01.flv`，并转向到组装后的请求地址，使得该flv在IE上能够实现播放。
