"""Produce new-style element index files."""

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
webBase = "/var/www/html/ssot/element_indexes/"
URLBase = "/ssot/element_indexes/"
template_name = "element_index_template.txt"
index_template_name = "element_index_index_template.html"
pages = []  # list for Jinja2
length_name = 20


def main():
    """Main program"""
    datestr = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    print(sys.argv[0], "starting at", datestr)
    print("------------------------------------------------------------")

    # for datatype in os.listdir(typesDir):
    # for datatype in ('xmetars', 'xtafs'):
    for datatype in ('xmetars','shipaws'):
        print("> Creating Element Index File for Datatype", datatype)

        elements = []  # list for Jinja2
        number_of_elements = 0

        # read config for datatype
        DatatypeConfig = SafeConfigParser()
        DatatypeConfig.read(''.join([defaultsDir, 'datatype_default.cfg']))
        DatatypeConfig.read(''.join([typesDir, datatype, '/datatype.cfg']))
        file_type = DatatypeConfig.get('DATATYPE', 'file_type')

        # read configs for elements
        ElementConfig = SafeConfigParser()
        ElementConfig.read(''.join([defaultsDir, 'element_default.cfg']))
        ElementConfig.read(''.join([typesDir, datatype, '/elements.cfg']))

        for section in ElementConfig.sections():
            shortcut = ElementConfig.get(section, 'shortcut')
            item = ElementConfig.get(section, 'item')
            returned_type = ElementConfig.get(section, 'returned_type')

            if ElementConfig.has_option(section, 'replicator'):
              replicator = ElementConfig.get(section, 'replicator')
            else:
              replicator = ''

            if ElementConfig.has_option(section, 'function_chain'):
              function_chain = ElementConfig.get(section, 'function_chain')
            else:
              function_chain = ''

            if ElementConfig.has_option(section, 'reported_units'):
              reported_units = ElementConfig.get(section, 'reported_units')
            else:
              reported_units = ''

            if ElementConfig.has_option(section, 'returned_units'):
              returned_units = ElementConfig.get(section, 'returned_units')
            else:
              returned_units = ''

            element = dict(element="element".ljust(length_name) +
                                   section,
                           shortcut="shortcut".ljust(length_name) +
                                   shortcut,
                           replicator="replicator".ljust(length_name) +
                                   replicator,
                           item="item".ljust(length_name) + item,
                           function_chain="function_chain".ljust(length_name) +
                                          function_chain,
                           reported_units="reported_units".ljust(length_name) +
                                          reported_units,
                           returned_units="returned_units".ljust(length_name) +
                                          returned_units,
                           returned_type="returned_type".ljust(length_name) +
                                          returned_type)
            elements.append(element)
            number_of_elements += 1

            # repeat if there is an alternative_name
            #ElementConfig._defaults = {}
            if ElementConfig.has_option(section, 'alternative_name'):
                alt_name = ElementConfig.get(section, 'alternative_name')
                element = dict(element="element".ljust(length_name) +
                                       alt_name,
                               shortcut="shortcut".ljust(length_name) +
                                        shortcut,
                               replicator="replicator".ljust(length_name) +
                                          replicator,
                               item="item".ljust(length_name) +
                                    item,
                               function_chain="function_chain".
                               ljust(length_name) + function_chain,
                               reported_units="reported_units".
                               ljust(length_name) + reported_units,
                               returned_units="returned_units".
                               ljust(length_name) + returned_units,
                               returned_type="returned_type".
                               ljust(length_name) + returned_type)

                elements.append(element)
                number_of_elements += 1

        print("------------------------------------------------------------")

        # Create the keyword:arguments dictionary for the Jinja template
        templateVars = {
            "fortran_format": "(A" + str(length_name) + ",A)",
            "file_type": "File Type".ljust(length_name) +
                         file_type.upper(),
            "element_count": "Number of Elements".ljust(length_name) +
                             str(number_of_elements),
            "datatype": datatype.upper(),
            "script": sys.argv[0],
            "timestamp": datestr,
            "elements": elements,
            }

        # Render the HTML from template
        htmlout = jinja_render(templateDir, template_name, **templateVars)
        outFile = ''.join([webBase, datatype])
        with open(outFile, "w") as outp:
            outp.write(htmlout)

        page_created = dict(datatype=datatype.upper(),
                            URL=''.join([URLBase, datatype]))
        pages.append(page_created)

    # Create index page from pages list
    templateVars = {
        "pages": pages,
        }
    htmlout = jinja_render(templateDir, index_template_name, **templateVars)
    outFile = ''.join([webBase, "index.html"])

    with open(outFile, "w") as outp:
        outp.write(htmlout)

    print(sys.argv[0], "finished at", \
        datetime.now().strftime('%Y-%m-%d %H:%M:%S'))

if __name__ == "__main__":
    main()
