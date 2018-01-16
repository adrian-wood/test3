#!/usr/local/sci/bin/python2.7
"""Produce a summary listing of HAWIS site IDs, names and positions
   from the XML Measurement Site Publication provided by Highways
   England.

   Utility for use with TDC (but not part of the TDC code-base).
   Run from the command line with the name of the MST file as an
   argument.
"""

import xml.etree.cElementTree as ET
import sys


def getTextList(elemNode):
    """Convert all the text in this and child nodes
       to a list of strings.  Ignore any blanks.
    """

    allText = elemNode.itertext()
    textList = []
    for x in allText:
        if x.strip():
            textList.append(x)
    return textList


def getName(elemNode):
    """Concatenate all the text in this and child nodes
       into a single string.
    """

    allText = elemNode.itertext()
    name = ''
    for n in allText:
        name = name + n.strip()
    return name.strip()


def main(filename):
    """Use ElementTree to parse the given file using the DOM.
       Select the nodes containing the site name and position and
       extract id, name, lat and lon.  Add the details to a dictionary
       and finally print this out in ID order.
    """

    tree = ET.parse(filename)
    siteDict = {}
    siteTag = '{http://datex2.eu/schema/2_0RC2/2_0}measurementSiteRecord'
    nameTag = '{http://datex2.eu/schema/2_0RC2/2_0}measurementSiteName'
    locTag = '{http://datex2.eu/schema/2_0RC2/2_0}measurementSiteLocation'

    for elem in tree.iter(tag=siteTag):
        id = elem.attrib['id']
        siteElem = elem.find(nameTag)
        name = getName(siteElem)
        siteLoc = elem.find(locTag)
        text = getTextList(siteLoc)
        lat = float(text[2])
        lon = float(text[3])
        siteDict[id] = [name, lat, lon]

    for id in sorted(siteDict.iterkeys()):
        (name, lat, lon) = siteDict[id]
        print '{0:5} {1:35} {2: 15f} {3: 15f}'.format(id, name, lat, lon)


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print 'USAGE: ', sys.argv[0], '<filename>'
        sys.exit(1)
    main(sys.argv[1])
