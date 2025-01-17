# clojure.data.json.clr #

JSON parser/generator to/from Clojure data structures.


A port of [clojure/data.json](https://github.com/clojure/data.json) library to ClojureCLR.

# Releases

Latest stable release: 2.5.1

[clj](https://clojure.org/guides/getting_started) dependency information:
```clojure
io.github.clojure/clr.data.json {:git/tag "v2.5.1" :git/sha "f84cb88"}
```

Nuget reference:

```
    PM> Install-Package clojure.data.json -Version=2.5.1
```

Leiningen/Clojars reference:

```
   [org.clojure.clr/data.json "2.5.1"]
```   

# Usage / API #

See the [README on the parent project](https://github.com/clojure/data.json/blob/master/README.md).

See the [API documentation for the parent project](http://clojure.github.com/data.json/).

All has been implemented except (1) the compatibility interface to the 0.1 API and (2) support for older releases of Clojure.
   
   
# Copyright and License #

Original ClojureJVM code:


> Copyright (c) Stuart Sierra, 2012. All rights reserved.  The use and distribution terms for this 
> software are covered by the Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) 
> which can be found in the file epl-v10.html at the root of this distribution.  By using this software 
> in any fashion, you are agreeing to be bound by the terms of this license.  You must not remove this 
> notice, or any other, from this software.

This project copies extensively from that codebase.  Edits and inclusions are marked in an obvious manner.  The modified code is:

> Copyright (c) David Miller, 2013. All rights reserved.  The use and distribution terms for this 
> software are covered by the Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) 
> which can be found in the file epl-v10.html at the root of this distribution.  By using this software 
> in any fashion, you are agreeing to be bound by the terms of this license.  You must not remove this 
> notice, or any other, from this software.

