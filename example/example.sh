#!/usr/bin/env bash

curl --json '{"name":"The Lord of the Rings", "author":"J.R.R. Tolkien", "year":1955}' http://localhost/scgi-example/book
curl --json '{"name":"The Way of Kings", "author":"Brandon Sanderson", "year":2010}' http://localhost/scgi-example/book
curl --json '{"name":"Words of Radiance", "author":"Brandon Sanderson", "year":2014}' http://localhost/scgi-example/book
curl --json '{"name":"Oathbringer", "author":"Brandon Sanderson", "year":2017}' http://localhost/scgi-example/book
curl --json '{"name":"Rhythm of War", "author":"Brandon Sanderson", "year":2020}' http://localhost/scgi-example/book
curl --json '{"name":"Wind and True", "author":"Brandon Sanderson", "year":2020}' http://localhost/scgi-example/book
curl --request "PATCH" --json '{"year":2024}' http://localhost/scgi-example/book/5
