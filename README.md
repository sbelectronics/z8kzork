
# Z8kzork
This is a port of Ryan C. Gordon's mojozork z-machine to run on smbaker's Z-8000 CP/M-8000 retrocomputer. It compiles natively using the zcc compiler in the CP/M-8000 ver 1.1 distribution. This compiler has many quirks and limitations and made the originally clean code downright ugly by the time I was done with it. This was a quick and dirty hack to make it work, nothing more.

This resulting z-machine will run as split-I/D mode with up to 64KB of code and 64KB of data. Zork1 is about 90KB in size, so a paging scheme is utilized to load the story on demand in 512 byte blocks, and then reap less-used blocks and replace them.

The Makefile will also compile it on Linux, as it's easier to test on a modern Linux box than on a 40-year old obsolete CPU running at 8 MHz.

-Scott

Original documentation for Mojozork follows...


```
>read leaflet
```

Hello sailor!

This is an implementation of Infocom's Z-Machine. The Z-Machine is a virtual
machine that's something like a high-level CPU. To keep their games portable
and easier to write, Infocom's games all use this fake processor and ship
with a platform-specific Z-Machine "emulator" ... so a game could run wherever
someone had implemented the Z-Machine.

This project is just for fun; everyone should write this at some point as an
educational activity. If you want a serious Z-Machine implementation, there
are certainly better ones out there (I personally recommend
["Frotz"](http://frotz.sourceforge.net/) and there are many others, too).

This program currently supports most of the Version 3 Z-Machine. This is
enough to play the vast majority of Infocom's catalog. Later Infocom games
used version 4, 5, and 6 of the Z-Machine, and those will currently not run
here. Most modern Interactive Fiction is built with a tool called Inform and
usually targets version 5 at the lowest; these games currently don't work
with this project. Maybe later.

Activision, who acquired Infocom in the 1990's, gives out Zork I, II, and III
for free, so I've included Zork I's data files with this project. If you want
to see Zork I run through from start to finish, you can run a pre-written
script to complete the entire game from the command line, like this:

If you want to write your own Z-Machine, there is an "official" specification
on how to implement it, written by people that spent significant time
reverse engineering the originals from Infocom, and extending the ecosystem
with new tools. You can find that specification
[here](http://inform-fiction.org/zmachine/standards/).

As usual, Wikipedia offers a wonderful rabbit hole to fall down, too, in
their [Z-machine article](https://en.wikipedia.org/wiki/Z-machine).


Enjoy!

--ryan.

