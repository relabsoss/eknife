# eknife
Erlang Swiss Army Knife 

Set of tools for building erlang applications.

Consist of:
* Macros for logger, supervisor and timer;
* Pub/Sub simpler interface (based on gproc); 
* Translit + slugs generator;
* Images type detect and check size (jpeg only);
* Rest & Outh 2 clients (based on gun);
* Some utilites:
  * buitify stacktrace (using in exception);
  * type cast functions;
  * date/time operations;
  * json operations (based on jiffy);
  * hash operations.
  
## Usage  

Put 
```
    {eknife,  {git, "https://github.com/relabsoss/eknife.git", {branch, "master"}}}
```
to your rebar.config
and
```
-include_lib("eknife/include/eknife.hrl").
```
to common include file.

The code is too small for per line documentation. Check it by yourself.
