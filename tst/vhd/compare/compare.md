## compare.md

64-bit stream comparator with compare and do not cares:

    data shift in > [ b0 .. b1 .. b2 .. b3 .. b64 ] > shift out
                      |     |     |     |     |   
    and with:       [ 1  .. 0  .. 1  .. 1  .. x   ]
                      |     |     |     |     |   
    or with:        [ 0  .. 0  .. 1  .. 0  .. x   ]
                      |     |     |     |     |   
    is match?       <-&-----&-----&-----&--..-&

A counter will also run to demarcate boundaries of bits, unless it is decided
that loading words at a time instead of bits will be better.

If we want to match a bit we want to a bit pattern we want to perform a bitwise
AND with the pattern to match and a bit wise OR with the bits we do not care if
they match (a "don't care function").

The VHDL is pretty simple for either words or bits.

Operating on a word:

    if ((DATA and MATCHMASK) or DONTCAREMASK) = (others => '1') then
      is_match <= '1';
    else
      is_match <= '0';
    end if;

This must be done on every word, then if they are all true, we have our match.
