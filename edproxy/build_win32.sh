#BOOST_SRC=../../boost_1_54_0_32
BOOST_SRC=F:/cq/boost_1_54_0_32
#JSONCPP_SRC=../../extlib/win/jsoncpp/jsoncpp
#JSONCPP_SRC=F:/cq/jsoncpp/jsoncpp
JSONC_SRC=F:/cq/jsonc
CURL_SRC=F:/cq/curl
#CURL_SRC=../../extlib/win/curl
auto/configure --with-cc=cl \
    --prefix= \
    --with-cc-opt="-D_CURL_ -DFD_SETSIZE=1024 -DBUILDING_LIBCURL -DHTTP_ONLY /Y- /EHsc -I${BOOST_SRC} -I${JSONC_SRC}/include -I${CURL_SRC}/include" \
    --with-pcre=../pcre-8.31 \
    --with-zlib=../zlib-1.2.7 \
    --with-openssl=../openssl-1.0.1c \
    --builddir=objs \
    --conf-path=conf/nginx.conf \
    --pid-path=logs/nginx.pid \
    --http-log-path=logs/access.log \
    --error-log-path=logs/error.log \
    --sbin-path=nginx.exe \
    --http-client-body-temp-path=temp/client_body_temp \
    --http-proxy-temp-path=temp/proxy_temp \
    --http-fastcgi-temp-path=temp/fastcgi_temp \
    --with-select_module --with-http_ssl_module \
    --with-ipv6  \
    --with-ld-opt="/link /LIBPATH:${BOOST_SRC}\stage\lib \
                   libboost_date_time-vc100-mt-s-1_54.lib \
                   libboost_regex-vc100-mt-s-1_54.lib \
                   LIBBoost_system-vc100-mt-s-1_54.lib \
                   /LIBPATH:${JSONC_SRC}\rel \
                   jsonc_vc10.lib \
                   /LIBPATH:${CURL_SRC} \
                   libcurl.lib \
                   ws2_32.lib \
                   winmm.lib \
                   wldap32.lib \
                   Advapi32.lib" \
    --add-module=../../modules/udsproxy/src/ngx_http_udsproxy\
	--add-module=../../modules/miniuds/src/ngx_http_miniuds \
	#--add-module=../../modules/rtmp
    #--add-module=../../src/helloworld 
sed -e 's/-WX//' objs/Makefile > objs/Makefile.w32
