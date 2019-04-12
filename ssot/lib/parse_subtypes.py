import configparser
import sys
import os


class Config():

    def __init__(self, cfg):
        """Read config file and initialise attributes from it."""
        self.parser = configparser.SafeConfigParser()
        self.subtypes = {}
        try:
            self.parser.read(cfg)
            for section in self.parser.sections():
                items = {}
                for name, value in self.parser.items(section):
                    # Expand the environment variables checking if they exist
                    # and have been set first
                    if value.find('$') >= 0:
                        expanded = os.path.expandvars(value)
                        if expanded == '' or expanded.find('$') >=0:
                            print(('ERROR environment variable not set in ', value))
                            sys.exit(2) 
                        items[name] =  expanded
                    else:
                        items[name] = value
                self.subtypes[section] = items
        except:
            print(('Error reading config file ', cfg))
            sys.exit(2)


    def __repr__(self):
        """Produce a nicely formatted string of config settings
           for printing.
        """
        output = ''
        for s, config_dict in sorted(self.subtypes.items()):
            output += '{:8s}\n'.format(s)
            for name, value in sorted(config_dict.items()):
                output += '{:15s} = {:s}\n'.format(name, value)
        return output


if __name__ == '__main__':
    infile = 'subtypes.cfg'
    config = Config(infile)
    print(config)

