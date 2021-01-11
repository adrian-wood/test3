# Canned Data Files

Canned files are provided here for unit testing. An explanation of each follows.

## The `data_access.log` File
This is the first 1000 lines of an actual MetDB `data_access.log` file.

### Counts by Datatype
The following command can be used to determine the datatypes retrieved and the number of retrievals for each datatype:

```
awk -F"," '{print $6}' data_access.log | tr -d ' ' | sort | uniq -c | sort -h
      1 subtype[SNOW]
      2 subtype[ESACSR]
      2 subtype[MWRI]
      2 subtype[WOW]
      3 subtype[AMSR2]
...
     48 subtype[SREW]
     50 subtype[RADRRATE]
     55 subtype[BUOY]
     83 subtype[LNDSYN]
     86 subtype[SHPSYN]
```
The command can be explained as follows:
1. Use `awk` to obtain the 6th field (where the field separator is a comma) of each line, this field being `subtype[<datatype>]`;
1. use `tr` to remove all spaces;
1. sort the lines;
1. Use `uniq -c` to count the number of occurences of each datatype;
1. sort again to display the results in order.

We can see that there is one retrieval of datatype `SNOW`, 2 for `ESACSR` and so on. Datatype `SHPSYN` has had the most retrievals with 86.

The total number of retrievals in this file can be determined by a simple line count:
```
wc -l data_access.log
1000 data_access.log
```

We can also sum the datatype counts as determined above as follows:
```
awk -F"," '{print $6}' data_access.log | sort | uniq -c | sort -h | cut -f1 -d's' | paste -sd+ | bc
1000
```
So there are a total of 1,000 retrievals in this file.

### Counts by User ID
A similar command can be used to determine the counts of retrievals by `userid`:

```
awk -F"," '{print $3}' data_access.log | sort | uniq -c | sort -h
      1 userid[jroberts]
      2 userid[ahorsema]
      2 userid[chthomas]
      2 userid[frdv]
      2 userid[nouser ]
...
     28 userid[frjm]
     56 userid[jwaller]
     77 userid[frwm]
    143 userid[freb]
    556 userid[uktrials]
```
We can see that user `jroberts` has performed 1 retrieval, `ahorsema` has done 2, and so on. User `uktrials` has done the most retrievals with 556.

### Counts by Contact
A similar command can be used to determine the counts of retrievals by `contact`:

```
awk -F"," '{print $5}' data_access.log | sort | uniq -c | sort -h
      1 contact[mi-ba076]
      1 contact[mi-ba077]
      1 contact[mi-ba329]
      1 contact[mi-ba340]
      1 contact[standalone.apps@metoffice.gov.uk]
...
      8 contact[srbest]
     12 contact[mi-ba291_mi-ba186]
     12 contact[mi-ba291_mi-ba187]
     24 contact[adam.maycock@metoffice.gov.uk]
    870 contact[OPS]
```
We can see that contact `mi-ba076` has performed 1 retrieval, `srbest` has done 8, and so on. Contact `OPS` has done the most retrievals with 870.

### Counts by userid, subtotalled by contact

It is useful to obtain for each `userid`, the numbers of retrievals that have been done for each `contact`. It is intended that the "contact" field contains the Suite Identifier. So for example, the `uktrials` userid runs thousands of retrievals, and we want subtotals by Suite (`contact`). We can do this with the following command:

```
grep 'userid\[uktrials\]' data_access.log | awk -F"," '{print $5}'| sort | uniq -c | sort -h
      1 contact[mi-ba076]
      1 contact[mi-ba077]
      1 contact[mi-ba329]
      1 contact[mi-ba340]
      3 contact[mi-az998]
      3 contact[mi-ba002]
      3 contact[mi-ba003]
    543 contact[OPS]
```
First we `grep` for all the retrievals for the `uktrials` userid, `awk` for the `contact` field and do the usual sorting etc.
We can see that for retrievals done by `uktrials`, the `contact` field has been set to `OPS` 543 times, `mi-ba003` 3 times etc. 

### Counts by contact, subtotalled by userid

Similarly to above, we may want to do the opposite, i.e. for each `contact`, obtain the subtotals of retrievals that have been done for each `userid`:

```
grep 'contact\[OPS\]' data_access.log | awk -F"," '{print $3}'| sort | uniq -c | sort -h
      1 userid[jroberts]
      2 userid[chthomas]
      2 userid[frdv]
      2 userid[frnb]
      3 userid[cmao]
      3 userid[gltrials]
      4 userid[frbg]
      4 userid[ppdev]
      5 userid[mjardak]
     11 userid[frlh]
     14 userid[hadci]
     56 userid[jwaller]
     77 userid[frwm]
    143 userid[freb]
    543 userid[uktrials]
```
We can see that for retrievals with `OPS` as the contact, 543 have been done by `uktrials`, 143 by `freb` etc. 

## The `retrieval_table` File
This is a cut-down MetDB `retrieval_table` file. It has 10 actual datatypes in it, together with the "pseudo" datatypes (ASSOC, ELEMENTS, ELEMIDX, OFFLINE, STNABRV, STNICAO, STNIND and STNMAS).

Some of the datatypes have multiple datasets.

The following command can be used to determine the datatypes and the number of datasets they have:

```
grep "MOODS" retrieval_table | cut -c1-9 | sort | egrep -v "ASSOC|ELEMENTS|ELEMIDX|OFFLINE|STNABRV|STNICAO|STNIND|STNMAS" | uniq -c

      1  AATSR
      1  JASON2
      1  LIDAR
     10  LNDSYN
      8  METARS
     13  SHPSYN
      1  SUNPHOTO
      2  TAFS
     12  TEMP
      1  VA_LIDAR
```
The command can be explained as follows:
1. `grep` for the string MOODS - appears on every "dataset" line;
1. Use `cut` to obtain the first 9 characters of those lines, i.e. just the datatype;
1. sort the lines;
1. get rid of the "pseudo" datatypes with `egrep -v`;
1. Use `uniq -c` to count the number of occurences of each datatype, which is the number of datasets for each.

So we can see that there are 10 Dataset entried for the `LNDSYN` Datatype, 13 for `SHPSYN`, 12 for `TEMP` and so on. Most Dataypes have just 1 Dataset. There are 10 lines, so the total number of Datatypes is 10.

