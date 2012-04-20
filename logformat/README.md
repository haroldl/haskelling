LogFormat is a Haskell module that makes it trivial to parse access
log records.

LogFormat will take the [LogFormat configuration][mod_log_config] from
your httpd.conf file and a log file and give you your log records as
Maps where the key is the field name.

The Apache httpd configuration files allow you to customize the format
of your log file records using the LogFormat directive. For example,
if you want every line of your access log to read "Hello Web" you can
do that like so:

    LogFormat "Hello Web" custom

More often you'll use a value such as

    LogFormat "%h %l %u %t \"%r\" %>s %b" common

and receive log records like

    127.0.0.1 - frank [10/Oct/2000:13:55:36 -0700] "GET /apache_pb.gif HTTP/1.0" 200 2326

[mod_log_config]: http://httpd.apache.org/docs/2.0/mod/mod_log_config.html
