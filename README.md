Setconcile
==========

Server for reconciliation of remote datasets while trying to minimize
the amount of data exchanged over network.

Example usage:

    Server A:
    /opt/setconcile/_rel/setconcile/bin/setconcile console -sname a

    Server B:
    /opt/setconcile/_rel/setconcile/bin/setconcile console -sname b

    Initiate reconciliation:
    curl -v -X POST http://<server A>:8080/api/datasets/symm/recons

Copyright (c) 2015 Ulf Leopold.
