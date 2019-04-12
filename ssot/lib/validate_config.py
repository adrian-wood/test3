"""Validate Configuration Files."""

import sys
import os
from datetime import datetime
from configparser import SafeConfigParser

baseDir = "/var/moods/ssot/"
typesDir = ''.join([baseDir, "configs/datatypes/"])
defaultsDir = ''.join([baseDir, "configs/defaults/"])


def validate_datatype(config_file, RulesConfig):
    """Function to Validate a datatype.cfg file against an
    Allowed configuration and the defaults"""
    valid = True
    # read "allowed" items from config
    allowed_datatype_file_types = [e.strip()
                          for e in RulesConfig.get("ALLOWED",
                                                     "datatype_file_types").split(',')]
    allowed_datatype_retrieval_methods = [e.strip()
                         for e in RulesConfig.get("ALLOWED",
                                                    "datatype_retrieval_methods")
                         .split(',')]

    allowed_datatype_options = [e.strip()
                         for e in RulesConfig.get("ALLOWED",
                                                    "datatype_options").split(',')]

    Config = SafeConfigParser()
    Config.read(config_file)

    for section in Config.sections():
        for option in Config.options(section):
            # is the option allowed?
            if option not in allowed_datatype_options:
                print(">>> ERROR: Invalid option", option, "in section", \
                      section)
                valid = False

    # file_type is compulsory
    if Config.has_option("DATATYPE", "file_type"):
        if Config.get("DATATYPE", "file_type") not in allowed_datatype_file_types:
            print(">>> ERROR: Invalid File Type", \
                  Config.get("DATATYPE", "file_type"), "in section", \
                  "DATATYPE")
            valid = False
    else:
        print(">>> ERROR: Missing File Type option")
        valid = False

    return valid


def validate_elements(config_file, RulesConfig):
    """Function to Validate an elements.cfg file against an
    Allowed configuration and the defaults"""
    valid = True
    # read "allowed" items from config
    allowed_units = [e.strip()
                     for e in RulesConfig.get("ALLOWED", "units").split(',')]
    allowed_element_shortcuts = [e.strip()
                         for e in RulesConfig.get("ALLOWED",
                                                    "element_shortcuts").split(',')]
    allowed_element_options = [e.strip()
                         for e in RulesConfig.get("ALLOWED",
                                                    "element_options").split(',')]

    Config = SafeConfigParser()
    Config.read(config_file)

    for section in Config.sections():
        for option in Config.options(section):
            # is the option allowed?
            if option not in allowed_element_options:
                print(">>> ERROR: Invalid option", option, "in section", \
                section)
                valid = False

        # are the units allowed?
        if Config.has_option(section, "reported_units"):
            if Config.get(section, "reported_units") not in allowed_units:
                print(">>> ERROR: Invalid units", \
                      Config.get(section, "reported_units"), "in section", \
                      section)
                valid = False

        # is the shortcut valid?
        if Config.has_option(section, "shortcut"):
            if Config.get(section, "shortcut") not in allowed_element_shortcuts:
                print(">>> ERROR: Invalid shortcut", \
                      Config.get(section, "shortcut"), "in section", \
                      section)
                valid = False

    return valid


def main():
    """Main program"""
    datestr = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    print(sys.argv[0], "starting at", datestr)

    print("------------------------------------------------------------")

    # Read in "rules" configuration file
    RulesConfig = SafeConfigParser()
    RulesConfig.read(''.join([defaultsDir, 'rules.cfg']))

    # Read in element defaults configuration file
    # ElementDefaultsConfig = SafeConfigParser()
    # ElementDefaultsConfig.read(''.join([defaultsDir, 'element_default.cfg']))

    # Read in datatype defaults configuration file
    # DatatypeDefaultsConfig = SafeConfigParser()
    # DatatypeDefaultsConfig.read(''.join([defaultsDir, 'datatype_default.cfg']))

    for datatype in os.listdir(typesDir):
        datatype_config_file = ''.join([typesDir, datatype, '/datatype.cfg'])
        print("> Validating Configuration Files for Datatype", datatype)
        print(">> 1) Datatype Definition in file", \
              datatype_config_file)
        if (validate_datatype(datatype_config_file, RulesConfig)):
           #DatatypeDefaultsConfig)):
            print(">> Valid Datatype Configuration File")
        else:
            print(">> Invalid Datatype Configuration File")

        element_config_file = ''.join([typesDir, datatype, '/elements.cfg'])
        print(">> 2) Elements Definition in file", \
              element_config_file)
        if (validate_elements(element_config_file, RulesConfig)):
           # ElementDefaultsConfig)):
            print(">> Valid Element Configuration File")
        else:
            print(">> Invalid Element Configuration File")
        print("------------------------------------------------------------")

    print(sys.argv[0], "finished at", \
        datetime.now().strftime('%Y-%m-%d %H:%M:%S'))

if __name__ == "__main__":
    main()
