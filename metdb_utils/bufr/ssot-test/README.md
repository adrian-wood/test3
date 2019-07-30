## ssot-test

This directory contains a work-in-progress which can be used to generate the beginnings
of an SSOT config file from an elements index.  It includes test versions of other modules 
that are still in development (the original modules are up a level in /var/moods/metdb_utils/bufr).

To use it, get a local copy of the elements_index you want the config file for then:

```
module load scitools

python lookup.py aatsr
```

You will then need to check the output against the subtype page, filling in the remaining details as per
Technote 44.


> Guaranteed not to work with merge sequences, non-bufr elements indexes and old-style indexes with
wrap-around lines!



Sheila



