# Gossip middleware

![Build status](https://travis-ci.org/gossiperl/gossiperl.svg?branch=master)

Gossiperl is a language agnostic, Apache Thrift based gossip middleware and message bus.

## Features:

 - language agnostic, communication via Apache Thrift interface
 - running as a service on the host
 - multiple overlays, applications may use their own overlays
 - every overlay behaves like a separate application and may use different configuration
 - security with AES-256 encryption, clients secured with their own secrets
 - managed with a REST API
 - multicast overlays
 - IPv6
 - simple publish/subscribe capabilities

## Client libraries:

- [Erlang](https://github.com/gossiperl/gossiperl-client-erlang)
- [Ruby](https://github.com/gossiperl/gossiperl-client-ruby)
- [JVM languages](https://github.com/gossiperl/gossiperl-client-jvm)
- [.NET languages](https://github.com/gossiperl/gossiperl-client-dotnet)
- [JavaScript](https://github.com/gossiperl/gossiperl-client-js)

## Documentation

Full documentation avaialable [here](https://github.com/gossiperl/gossiperl/wiki).

## Unit tests

    rebar clean get-deps compile && rebar eunit skip_deps=true

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