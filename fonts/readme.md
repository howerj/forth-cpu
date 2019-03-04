# Font Directory

This directory contains fonts which can used with the VGA terminal emulator
component.

All characters are 8 pixels wide, and 12 high. To ease editing and viewing of
the font, they are stored in a text file as a pixel map, with a '1' character
representing a switched on pixel. No prizes for guessing which character
represents an off pixel.

The format has two other advantages; it is easy for it to be read in by the HDL
tool-chain, and it can be converted to a viewable [PBM][] file quite easily. The
conversion just requires prepending a header.

The [Terminus][] font used could do with some cleaning up - apart from the
normal printable ASCII characters, the characters outside of that range were
picked at random.

## Copyright

The fonts are under copyright. I believe the [Terminus][] font to be licensed under
the 'SIL OPEN FONT LICENSE Version 1.1', the iso-8859-15 Latin-0 font I am 
unsure of, it came with the original project and was generated from an open
source font package.

[PBM]: https://en.wikipedia.org/wiki/Netpbm_format
[Terminus]: http://terminus-font.sourceforge.net/
