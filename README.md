relplot 1.5
-----------

This is a program for generating attractive plots of the solutions
to equations. It uses interval arithmetic to efficiently approximate
these solutions. Output is generated as PostScript, which has some
nice properties:

   - the output is resolution-independent
   - the output is a program that can be edited by hand to change its
     appearance or to overlay additional information.

Relplot is mostly written in SML/NJ but also includes some PostScript. Use
"make" to build it with the SML executable.

The shell script "relplot" is used to generate PostScript output. "relplot-pdf"
generates PDF output. "plot.pl" is designed to be invoked from a web interface,
as at the installation at:

    http://www.cs.cornell.edu/w8/~andru/relplot

The script install-relplot allows setting up a new relplot web server.

-----

This is free, unsupported software that comes with no warranty whatsoever.
Under no circumstances will the authors or providers of the software be
liable for damages pertaining to this software or its use. It may be
freely distributed for noncommercial use, provided that this notice is
maintained.
