#!/bin/sh
curl -i -b cookiejar -H "Content-Type: application/json" -X GET http://localhost:8080/
echo
