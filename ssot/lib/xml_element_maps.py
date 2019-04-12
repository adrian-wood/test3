"""Produce MetDB to XML element map pages and an index."""

import sys
import os
sys.path.append('/var/moods/metdb_utils/web')
from jinja_render import jinja_render
from configparser import SafeConfigParser
import cgi

baseDir = "/var/moods/ssot/"
typesDir = ''.join([baseDir,"configs/datatypes/"])
templateDir = ''.join([baseDir,"templates/"])
defaultsDir = ''.join([baseDir,"configs/defaults/"])
webBase = "/var/www/html/ssot/element_maps/"
URLBase = "/ssot/element_maps/"
template_name = "xml_element_map_template.html"
index_template_name = "xml_element_map_index_template.html"
pages_created = {}
pages = []  # list for Jinja2

def main():
    """Main program"""

    for pairing in (('xmetars','metars'),
                    ('xtafs', 'tafs')):
        datatype = pairing[0]
        prevtype = pairing[1]

        # read configs for datatype and elements
        DatatypeConfig = SafeConfigParser()
        DatatypeConfig.read(''.join([typesDir, datatype, '/datatype.cfg']))

        Config = SafeConfigParser()
        Config.read(''.join([defaultsDir,'element_default.cfg']))
        Config.read(''.join([typesDir, datatype, '/elements.cfg']))

        elements = []  # list for Jinja2
        shortcuts = []  # list for Jinja2

        # Populate shortcuts list
        shortcut = dict(shortcut_name = "{ob}",
                        shortcut_desc = cgi.escape(DatatypeConfig.get('DATATYPE', 'ob')))
        shortcuts.append(shortcut)
        shortcut = dict(shortcut_name = "{indfix}",
                        shortcut_desc = "Fixed Entry in the Index")
        shortcuts.append(shortcut)
        shortcut = dict(shortcut_name = "{indvar}",
                        shortcut_desc = "Variable (additional) Entry in the Index")
        shortcuts.append(shortcut)
        shortcut = dict(shortcut_name = "{collection}",
                        shortcut_desc = "Collection of other Elements")
        shortcuts.append(shortcut)

        sub_elements = []

        for section in Config.sections():
            if section != "GENERAL":
                if Config.has_option(section,'elements'):
                    sub_elements = [x.strip()
                                         for x in Config.get(section, 'elements').split(',')]
                    element = dict(element_name = section,
                           description = "next " + str(len(sub_elements)) + " elements:")
                else:
                    if section in sub_elements:
                        element_name = "> " + section
                    else:
                        element_name = section

                    alternative_name = Config.get(section, 'alternative_name')
                    if alternative_name:
                        element_name = ''.join([element_name, " (or ", alternative_name, ")" ])

                    units = Config.get(section, 'units')
                    if units == "table":
                        units = Config.get(section, 'table_id')

                    replication_count = Config.get(section, 'replication_count')
                    if replication_count == "1":
                        replication_count = ""

                    element = dict(element_name = element_name,
                                   description = Config.get(section, 'description'),
                                   shortcut = Config.get(section, 'shortcut'),
                                   repeater = (Config.get(section, 'repeater')),
                                   element = cgi.escape(Config.get(section, 'element')),
                                   function_chain = cgi.escape(Config.get(section, 'function_chain')))
    
                elements.append(element)

        # Create the keyword:arguments dictionary for the Jinja template
        templateVars = {
            "datatype": datatype.upper(),
            "shortcuts": shortcuts,
            "elements": elements,
            }
        # Render the HTML from template
        htmlout = jinja_render(templateDir, template_name, **templateVars)
        outFile = ''.join([webBase, datatype, ".html"])
        
        with open(outFile, "w") as outp:
            outp.write(htmlout)

        page_created = dict(datatype = datatype.upper(),
                            URL = ''.join([URLBase, datatype, ".html"]))
        pages.append(page_created)

    # Create index page from pages_created dictionary
    templateVars = {
        "pages": pages,
        }
    htmlout = jinja_render(templateDir, index_template_name, **templateVars)
    outFile = ''.join([webBase, "index.html"])
        
    with open(outFile, "w") as outp:
        outp.write(htmlout)

if __name__ == "__main__":
    main()
