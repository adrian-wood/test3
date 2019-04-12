"""Produce code snippets for metdb.subtypes.py file 

   Uses two templates to create two files:
     template_name = subtype_additions_template.txt produces
     code that you can include in your program to update the 
     metdb module
     
     template_name = subtypes_template.txt produces code 
     intended to be included in the subtypes.py module
     itself.
     
     The datatype is supplied as an argument."""

import sys
import os
from datetime import datetime
sys.path.append('/var/moods/metdb_utils/web')
from jinja_render import jinja_render
from configparser import SafeConfigParser

baseDir = "/var/moods/ssot/"
typesDir = ''. join([baseDir, "configs/datatypes/"])
templateDir = ''. join([baseDir, "templates/"])
defaultsDir = ''. join([baseDir, "configs/defaults/"])
template_name = "subtype_additions_template.txt"


def main():
    """Main program"""
    if len(sys.argv) != 2:
        print("supply arg")
        sys.exit()
    else:
        datatype = sys.argv[1]
        outDir = ''.join([typesDir, datatype])

    datestr = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    print(sys.argv[0], "starting at", datestr)
    print("------------------------------------------------------------")

    print("> Creating subtype.py code for Datatype", datatype)

    elements = []  # list for Jinja2

    # read configs for elements
    ElementConfig = SafeConfigParser()
    ElementConfig.read(''.join([defaultsDir, 'element_default.cfg']))
    ElementConfig.read(''.join([typesDir, datatype, '/elements.cfg']))

    for section in ElementConfig.sections():
        shortcut = ElementConfig.get(section, 'shortcut')
        returned_type = ElementConfig.get(section, 'returned_type')
        if returned_type == 'I':
            returned_type = 'i4'
        elif returned_type == 'R':
            returned_type = 'f4'

        if ElementConfig.has_option(section, 'reported_units'):
            reported_units = ElementConfig.get(section, 'reported_units')
        else:
            reported_units = ''
        if ElementConfig.has_option(section, 'returned_units'):
            returned_units = ElementConfig.get(section, 'returned_units')
        else:
            returned_units = ''

        if ElementConfig.has_option(section, 'description'):
            description  = ElementConfig.get(section, 'description')
        else:
            description = ''

        element = dict(element = section.strip(),
                       returned_type = returned_type.strip(),
                       description = description) 
        elements.append(element)


    print("------------------------------------------------------------")
    # Create the keyword:arguments dictionary for the Jinja template
    templateVars = {
        "datatype": datatype.upper(),
        "script": sys.argv[0],
        "timestamp": datestr,
        "elements": elements,
            }

    # Render the output from template
    htmlout = jinja_render(templateDir, "subtype_additions_template.txt", **templateVars)
    outFile = ''.join([outDir, "/subtype_snippet.py"])
    with open(outFile, "w") as outp:
        outp.write(htmlout)

    htmlout = jinja_render(templateDir, "subtypes_template.txt", **templateVars)
    outFile = ''.join([outDir, "/subtypes.py"])
    with open(outFile, "w") as outp:
        outp.write(htmlout)


    print(sys.argv[0], "finished at", \
    datetime.now().strftime('%Y-%m-%d %H:%M:%S'))

if __name__ == "__main__":
    main()
