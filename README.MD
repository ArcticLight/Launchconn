# LaunchConn

LaunchConn is a simple(ish) Scala library to connect to a Novation(TM) launchpad and control it with a program.

The current version has basic functionality, but there may be a few bugs left, and there are many improvements still that could be had. Please file issues to help make this project better!

+ This library is NOT thread safe. Don't use it across multiple threads.
+ Initialize it with the initializer function, then use your launchpad!
+ To receive button presses, pass a function which takes an int and a boolean to the handler. Your function will be called whenever a button is pushed.