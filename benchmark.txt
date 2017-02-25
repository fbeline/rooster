#rooster

Running 30s test @ http://127.0.0.1:8080/products
  12 threads and 400 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    15.20ms   23.80ms 421.94ms   97.62%
    Req/Sec     2.66k   386.98     8.31k    85.98%
  953921 requests in 30.10s, 259.27MB read
Requests/sec:  31694.00
Transfer/sec:      8.61MB

#Nodejs - express

Running 30s test @ http://127.0.0.1:8080/products
  12 threads and 400 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    38.10ms    4.04ms 293.00ms   96.91%
    Req/Sec     0.86k   118.19     1.33k    67.69%
  308812 requests in 30.04s, 64.20MB read
Requests/sec:  10280.39
Transfer/sec:      2.14MB


#Flask

Running 30s test @ http://127.0.0.1:5000/products
  12 threads and 400 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    78.61ms   78.90ms   1.67s    96.71%
    Req/Sec   199.35    164.93   585.00     58.68%
  23486 requests in 30.04s, 4.03MB read
  Socket errors: connect 0, read 66, write 0, timeout 275
Requests/sec:    781.86
Transfer/sec:    137.44KB

#Sinatra

Running 30s test @ http://127.0.0.1:4567/products
  12 threads and 400 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   205.32ms  102.44ms   1.91s    82.15%
    Req/Sec   123.67     51.87   330.00     62.83%
  14472 requests in 30.05s, 3.40MB read
  Socket errors: connect 0, read 26, write 0, timeout 12
Requests/sec:    481.57
Transfer/sec:    115.83KB







