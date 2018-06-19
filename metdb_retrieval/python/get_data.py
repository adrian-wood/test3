# ----------------------------------------------------------------------
#
# PROGRAM       : get_data.py
#
# PURPOSE       : Retrieves metdb data and outputs a csv file
#
# USAGE         : get_data.py -c config.cfg
#
# REVISION INFO :
# MB-1683: May 2018 Original.                            Sheila Needham
#
# ----------------------------------------------------------------------
# (C) CROWN COPYRIGHT 2018 - MET OFFICE. All Rights Reserved.
#
# Met Office, United Kingdom.
#
# The use, duplication and disclosure of this code is strictly
# prohibited without the permission of The Meteorological Database
# Team at the above address.
# ----------------------------------------------------------------------

import ConfigParser
import csv
import datetime as dt
import getopt
import importlib
import metdb
import numpy as np
import string
import sys

TFMT = '%Y-%m-%dT%H:%M:%S'   # time format string
MDI = np.ma.masked           # numPy missing data indicator
MPS2KTS = 3600./1852.        # m/s to knots conversion
sites = MDI


# ----------------------------------------------------------------------
class Settings():
    '''Settings class contains details from the config file.  Assumes
       unique names across all sections; these are then the attribute
       names.'''

    def __init__(self, cfg):
        self.parser = ConfigParser.SafeConfigParser()
        try:
            self.parser.read(cfg)
            for section in self.parser.sections():
                for name, value in self.parser.items(section):
                    setattr(self, name, value)

        except:
            print 'Error reading config file ', cfg
            sys.exit(2)

    def reset(self, sections):
        '''Updates the config file. sections is a dictionary of name, value
           pairs.
        '''
        for name, value in sections.iteritems():
            self.parser.set('REQUEST', name, value)

    def write_cfg(self, cfg):
        '''Writes to a config file.'''
        try:
            with open(cfg, 'wb') as configfile:
                self.parser.write(configfile)
        except:
            print 'Error writing config file ', cfg

    def __repr__(self):
        '''This produces a nicely formatted string of config settings
           for printing.
        '''
        output = ''
        for section in self.parser.sections():
            for name, value in self.parser.items(section):
                output += '{:15s} = {:s}\n'.format(name, value)
        return output


# ----------------------------------------------------------------------
class Elements():
    '''Elements class contains details read from the element mapping
       file.  This includes the CSV output column name and the MetDB
       elements and/or functions needed to produce data for that
       column.
       The element file consists of three columns separated by colons:
         number - gives the order of the output fields        - row[0]
         csv column name - as defined by the wow csv format   - row[1]
         metdb function/element names - to produce the output - row[2]
       The function must be in lower-case, the element names must be
       in upper-case.  Items preceded by % are fixed values.
       No nested functions (yet).

       The element_map is a dictionary with key = the csv column
       and value = a list of order and function.
    '''

    def __init__(self, elem):
        self.element_map = {}
        try:
            f = open(elem, 'r')
            e_reader = csv.reader(f, delimiter=':')
            for row in e_reader:
                key = row[1].strip()
                order = int(row[0])
                func = row[2].strip()
                self.element_map[key] = (order, func)
            self.rows = len(self.element_map)
            self.fields = []
            for k, v in sorted(self.element_map.iteritems(),
                               key=lambda (k, v): v[0]):
                self.fields.append(k)
        except:
            print 'ERROR reading elements file ', elem
            sys.exit(2)
        finally:
            f.close()

    @staticmethod
    def parse_elements(line):
        '''Parses a string containing a mix of functions and element
           names and returns a list of the element names.
        '''
        s = line.translate(None, string.ascii_lowercase)  # del lower-case
        s = s.translate(None, '().')                       # del brackets
        s = s.strip('_')                                   # del _
        s = s.strip(' ')                                   # del spaces
        return s.split(',')

    def get_element_names(self):
        '''This produces a list of unique element names by extracting
           the upper-case words from all functions in the mapping table.
        '''
        elements_list = []
        for k, v in self.element_map.iteritems():
            if '%' not in v[1]:
                elements = Elements.parse_elements(v[1])
                elements_list = list(set(elements_list + elements))
        return elements_list

    def __repr__(self):
        '''This produces a nicely formatted string containing the
           element details for printing.
        '''
        output = ''
        for k, v in self.element_map.iteritems():
            output += '{:35s}{:5s}{:40s}\n'.format(k, ' --- ', v[1])
        return output


# ----------------------------------------------------------------------
def time_from_ref(ref, hour=None, start=True):
    '''Creates a MetDB format date/time string using the given
       parameters:
       Ref    - is a datetime object.
       hour   - is optional, if not given then ref is used.
       start  - is optional, default is True.
       when it is a start time then the minutes start at 00,
       otherwise an end time is assumed and minutes is set to 59.
       If the requested hour is in the future then it uses the
       day before the ref date instead.
    '''
    copy_ref = ref
    if hour is None:
        hr = ref.hour
        min = ref.minute
    else:
        hr = hour
        if start:
            min = 0
        else:
            min = 59
    if ref.hour < hr:
        copy_ref = copy_ref - dt.timedelta(days=1)
    return copy_ref.strftime("%Y%m%d/") + \
        '{:02d}{:02d}'.format(hr, min) + 'Z'


# -------------------------------------------------------------------
def process_function(expression, obs, i):
    '''Gets arguments for the given function, evaluates it and
       returns the results as a string representation.
       obs is a numpy masked array as returned by the metdb call
       (therefore it can be indexed by element name).
    '''
    global sites
#   import pdb; pdb.set_trace()

# get the variable names from the expression...

    args_list = Elements.parse_elements(expression)
    args = []
    value = ''

# ...and put them in an ordered list

    for a in args_list:
        if '%' not in a:
            args.append(obs[a][i])
        else:
            args.append(a[1:])

# get the function name, if there is one

    parts = expression.split('(')
    if len(parts) == 1:
        func = None
    else:
        func = parts[0]
    if func:
        try:
            f = eval(func)
            value = f(*args)

        except:
            print 'Error calling function', func
            sys.exit(2)
    else:
        num = args[0]
        if num is not MDI:
            value = str(num)
    return value


# ----------------------------------------------------------------------
def main():
    '''Main program.
    '''

# sites is a global variable so that its functions can be used in the
# generic process_function call without having to pass the instance
# every time.

    global sites

# Obtain the name of the config file, must be supplied with -c <file>
    config_file = ''
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'hc:')
    except getopt.GetoptError:
        print sys.argv[0], ' -c <configfile>'
        sys.exit(2)

    for opt, arg in opts:
        if opt == '-h':
            print sys.argv[0], ' -c <configfile>'
            sys.exit()
        elif opt == '-c':
            config_file = arg

    if not config_file:
        print sys.argv[0], ' must supply config file with -c'
        sys.exit(2)

# Read config file to determine settings

    settings = Settings(config_file)
    print 'Config file - ', config_file
    print settings

# Load any external modules
    if hasattr(settings, 'package'):
        pkg = settings.package
	global package
	package = importlib.import_module(pkg)
	    

# Read site details if supplied                          
    if hasattr(settings, 'site_file'):
        site_file = settings.site_file
        sites = package.sites.Sites(settings.site_file)
        print sites.count, ' sites read from', settings.site_file
    else:
        sites = None

# Read element details

    elements = Elements(settings.element_file)
    print elements.rows, ' parameters read from', settings.element_file
    print elements
    elements_list = elements.get_element_names()

# Set up MetDB parameters

    last_run = settings.run_time
    last_run = dt.datetime.strptime(last_run, TFMT)

    now = dt.datetime.utcnow()
    this_run = now.strftime(TFMT)
    timestamp = now.strftime('%Y%m%dT%H%M%S')

    hours = settings.start_time
    hour_list = [int(t) for t in hours.split(',')]

    if hasattr(settings, 'platform'):
        platforms = settings.platform
        platforms = platforms.replace(',', ' ')
    else:
        platforms = None

    nobs = 0       # counts the total number of obs retrieved
    csv_list = []  # list of lines to output

# Loop over requests

    for t in hour_list:
        start_time = time_from_ref(now, hour=t, start=True)
        end_time = time_from_ref(now, hour=t, start=False)
        rcpt_time = time_from_ref(last_run)
        ob_dt = '{:02d}'.format(t)

        keywords = ['START TIME ' + start_time,
                    'END TIME ' + end_time,
                    'RECEIVED AFTER ' + rcpt_time]
	if platforms:
	    keywords.append('PLATFORM ' + platforms)

        try:
	    print 'Calling metdb',keywords,elements_list
            obs = metdb.obs(settings.contact,
                            settings.subtype,
                            keywords,
                            elements_list)
            print 'Retrieved ', len(obs), ' observations'
            nobs += len(obs)
        except IOError:
            print 'WARNING non-zero return from MetDB'
            continue  # with next hour

# Loop over observations
        print 'Looping',
        for i in range(len(obs)):
	    if i%100 == 0:
	        print '.',
		sys.stdout.flush()
            output_csv = {}
	    if (sites and sites.required(obs, i)) or sites is None:
                for k, v in elements.element_map.iteritems():
                    expression = v[1]
                    output_csv[k] = process_function(expression, obs, i)

                # end of loop over elements
                csv_list.append(output_csv)

        # end of loop over obs

# Output data to a file
        if nobs > 0:
            outdir = settings.output_dir
            output = outdir + '/' + settings.output_file
            output = output.replace('<timestamp>', timestamp)
            output = output.replace('<dt>', ob_dt)
            try:
                f = open(output, 'wt')
                writer = csv.DictWriter(f, fieldnames=elements.fields)
                header = dict((n, n) for n in elements.fields)
                writer.writerow(header)
                for row in csv_list:
                    writer.writerow(row)
                print ' '.join(keywords), ' output to ', output
            except:
                print 'ERROR writing output file', output
                sys.exit(2)
            finally:
                f.close()

    # end of loop over requests

# Update config file with run time
    settings.reset({'run_time': this_run})
    settings.write_cfg(config_file)

if __name__ == '__main__':
    main()
