#!/bin/sh

echo 'no password'
curl  -i -b cookiejar -c cookiejar -H "Content-Type: application/json" -X POST -d '{"email":"x@xyz.com","pass":"yyyyyy"}' http://localhost:8080/login
echo
echo 'all data ok'
curl  -i -b cookiejar -c cookiejar -H "Content-Type: application/json" -X POST -d '{"email":"x@xyz.com","pass":"xyzXYZ"}' http://localhost:8080/login
echo
