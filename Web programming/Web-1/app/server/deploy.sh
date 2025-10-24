killall httpd
killall java
httpd -f ~/web1/httpd.conf -k restart
java -jar -DFCGI_PORT=9000 ./httpd-root/fcgi-bin/server.jar