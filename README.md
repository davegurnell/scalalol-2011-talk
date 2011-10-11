A Route To The Three 'R's: Reading, Writing and the REST
========================================================

Slides, notes and code samples for my talk at Scala Lift-Off London 2011.

Abstract
--------

The mappings between URLs and code form an integral part of any web application. Many web frameworks help map from URLs to code, but the reverse mapping is often neglected, leaving developers to construct URLs via haphazard string manipulation. Furthermore, many frameworks do not match URLs in a type-safe manner.

Scala provides all the tools we need to address this problem in a more comprehensive manner. In this talk we will walk through the creation of [Bigtop Routes], a bidirectional mapping library that is both type-safe and developer-friendly. We will pay particular attention to the ways in which Scala language features, such as flexible syntax, implicit conversions, and a touch of type-level programming, help to simplify the task at hand.

[Bigtop Routes]: http://bigtopweb.com/routes

Slides
------

The slides are available here:

 - Keynote file: https://github.com/davegurnell/scalalol-2011-talk/blob/master/0-slides/slides.key
 - PDF export:   https://github.com/davegurnell/scalalol-2011-talk/blob/master/0-slides/slides.pdf

The slides probably won't be of much use to you on their own. Sorry about that - it's just that kind of talk. Hopefully the Skills Matter guys will record the talk and put up a video. If/when they do, I'll post the link here.

Code
----

In the talk I build the routes library in stages from the ground up. There is a code sample associated with each stage. In each case I recommend you check out the unit tests to see what's going on, and then invoke the Scala console to play with the code yourself. For example:

    bash$ cd 1-args                            # change to the example directory


    bash$ ls src/test/scala                    # see what test files there are
    ArgSpec.scala


    less src/test/scala/ArgSpec.scala          # have a look at the tests to see what's going on
    << file contents appears here >>


    ./sbt test                                 # run the tests to check it all works
    [info] Set current project to 1-args (in build file:/blah/blah/1-args/)
    [info]   + IntArg.encode encodes as expected
    [info]   + IntArg.decode decodes integers correctly
    [info]   + IntArg.decode only decodes integers
    [info]   + StringArg.encode does not (un)escape reserved characters
    [info]   + StringArg.decode does not (un)escape reserved characters
    [info] Passed: : Total 5, Failed 0, Errors 0, Passed 5, Skipped 0
    [success] Total time: 4 s, completed Oct 10, 2011 3:54:14 PM


    ./sbt console                              # start an interactive shell to try it for yourself
    [info] Set current project to 1-args (in build file:/blah/blah/1-args/)
    [info] Starting scala interpreter...
    [info] 
    Welcome to Scala version 2.9.1.final (Java HotSpot(TM) 64-Bit Server VM, Java 1.6.0_26).
    Type in expressions to have them evaluated.
    Type :help for more information.


    scala> IntArg.decode("123")                # run some Scala commands to see what happens
    res0: Option[Int] = Some(123)


    scala> :quit                               # quit the Scala console

Licence
-------

<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/3.0/"><img alt="Creative Commons License" style="border-width:0" src="http://i.creativecommons.org/l/by-nc-sa/3.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/3.0/">Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License</a>.
