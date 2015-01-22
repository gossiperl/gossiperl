# Gossip middleware

Gossiperl is a language agnostic, Apache Thrift based gossip middleware and message bus.

The purpose of gossiperl is to provide a gossip as a service for applications running on a host.

Gossiperl is written in Erlang. Once a gossiperl daemon is compiled and installed on the host, an application can connect and start exchanging data with other members. This is achieved by allowing applications to register their own overlays. Every overlay can be secured using dedicated pair of symmetric key and iv (AES-256). Only the member who has access to a given set of symmetric key and iv has access to the data exchanged on that overlay. Additionally, every gossip client uses their own secret key to prevent spoofing by other overlay members. Messages between applications can be shared only via gossiperl daemon. The daemon knows the secret keys of every application and will not attempt passing any messages from the application if the key doesn’t match.

Every overlay uses its own separate configuration. Such overlays behave as completely separate systems.

Gossiperl client applications can publish and subscribe to digest types provided by other applications.

## Client libraries:

- [Erlang](https://github.com/gossiperl/gossiperl-client-erlang)
- [Ruby](https://github.com/gossiperl/gossiperl-client-ruby)
- [JVM languages](https://github.com/gossiperl/gossiperl-client-jvm)
- [.NET languages](https://github.com/gossiperl/gossiperl-client-dotnet)
- [JavaScript](https://github.com/gossiperl/gossiperl-client-js)

## Documentation

https://github.com/gossiperl/gossiperl/wiki

## Unit tests

    ./rebar clean get-deps compile eunit

## Author

Radoslaw Gruchalski <radek@gruchalski.com>
https://github.com/radekg

## License

The MIT License (MIT)

Copyright (c) 2014 Radoslaw Gruchalski <radek@gruchalski.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.