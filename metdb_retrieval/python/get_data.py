# ----------------------------------------------------------------------
#
# PROGRAM       : get_data.py
#
# PURPOSE       : Retrieves metdb data and outputs a csv file
#
# USAGE         : get_data.py -c config.cfg
#
# REVISION INFO :
# MB-1818: Feb 2019 Expand environment variables if given in the 
#                   config file. Also, write to a temp file first and
#                   rename at the end (to allow asynchronous
#                   file transfers).                                SN
# MB-1891: Jan 2019 Cater for retrieval periods of > 1 hour and for
#                   "days ago" retrievals, both needed for AIRQAL and
#                   AIRQALEU ServiceHub retrievals.                 AM
# MB-1876: Dec 2018 Speed up CSV output by turning off type checking.
# MB-1804: Nov 2018 Move metdb module corrections out of this source.
#          Suppress printing of elements list unless in test mode.  SN
# MB-1824: Oct 2018 Add raw report text to METAR/SPECI              SN
# MB-1803: Oct 2018 Use a csv dialect for output to get the quoting
#          right for string containing commas.                      SN
# MB-1798: Aug 2018 New processing for replicated elements.         SN
# MB-1789: Aug 2018 Optional headers; new column in elements file;
#           test option; default start time; correct initialisation
#           of csv output list.                                     SN
# MB-1780: Jul 2018 Added _strptime to avoid import lock error.     SN
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
from datetime import timedelta
import getopt
import importlib
import metdb
import metdb_corrections
import numpy as np
import _strptime
import re
import string
import sys
import traceback
from unit_utils import *

# Local corrections for errors in the python subtypes module

metdb_corrections.local_updates()

TFMT = '%Y-%m-%dT%H:%M:%S'   # time format string
sites = MDI


# ----------------------------------------------------------------------
class Settings():
    '''Settings class contains details from the config file.  Assumes
       unique names across all sections; these are then the attribute
       names.'''

    def __init__(self, cfg):
        """Read config file and initialise attributes from it."""
        self.parser = ConfigParser.SafeConfigParser()
        try:
            self.parser.read(cfg)
            for section in self.parser.sections():
                for name, value in self.parser.items(section):
             
                    # Expand the environment variables checking if they exist
                    # and have been set first

                    if value.find('$') >= 0:
                        expanded = os.path.expandvars(value)
                        if expanded == '' or expanded.find('$') >=0:
                            print 'ERROR environment variable not set in ', value
                            sys.exit(2) 
                        setattr(self, name, expanded)
                    else:
                        setattr(self, name, value)

        except:
            print 'Error reading config file ', cfg
            traceback.print_exc()
            sys.exit(2)

    def reset(self, sections):
        """Update the config file. sections is a dictionary of name, value
           pairs.
        """
        for name, value in sections.iteritems():
            self.parser.set('REQUEST', name, value)

    def write_cfg(self, cfg):
        """Write to a config file."""
        try:
            with open(cfg, 'wb') as configfile:
                self.parser.write(configfile)
        except:
            print 'Error writing config file ', cfg

    def __repr__(self):
        """Produce a nicely formatted string of config settings
           for printing.
        """
        output = ''
        for section in self.parser.sections():
            for name, value in self.parser.items(section):
                output += '{:15s} = {:s}\n'.format(name, value)
        return output


# ----------------------------------------------------------------------
class Elements():
    """Element details.

       Contains details read from the element mapping file.
       This includes the CSV output column name and the MetDB
       elements and/or functions needed to produce data for that
       column.

       The element file consists of four columns separated by colons:
         number - not used in this version                    - row[0]
         csv column name - including optional units in brackets row[1]
         csv uom name - official unit abbreviation              row[2]
         metdb function/element names - to produce the output - row[3]
       The function must be in lower-case, the element names must be
       in upper-case.  Items preceded by % are fixed values.
       A set of functions for handling replicated data have the word
       'layer' in their name and they get special treatment.
       No nested functions (yet).

       Attributes:
           element_map (dict): key = the csv column and value = a
                               list of order, function and uom.
           layer_map (dict): key = layer name and value = a list
                             of max replications and process function
                             as a string.
    """

    def __init__(self, elem):
        """Read elements file and initialise dictionary.

           Args:
               elem (str): element filename
        """
        self.element_map = {}  # table as dict
        self.fields = []       # combined title and units
        self.titles = []       # just the title
        self.units = []        # just the units
        self.uom = []          # just the uom
        self.rows = 0          # count of rows
        self.layer_map = {}    # any layer elements defined here

        try:
            f = open(elem, 'r')

            # defines the csv format
            csv.register_dialect('elements', delimiter=':',
                                 escapechar='\\',
                                 quoting=csv.QUOTE_NONE)

            e_reader = csv.reader(f, dialect='elements')
            order = 0
            for row in e_reader:
                key = row[1].strip()
                uom = row[2].strip()
                func = row[3].strip()

                if 'define_layer' in func:

                    # process layer function. Do not add
                    # element names to the map here because
                    # this row of the table is just defining
                    # the replicated set

                    (key, value) = self.define_layer(func)
                    self.layer_map[key] = value

                elif 'from_layer' in func:

                    # expand element into a number of replications.
                    # Each new element will be prefixed with a count
                    # delimited by underscores e.g.' _01_cloud_base'

                    (new_entries, new_func) = self.from_layer(func)
                    layer_count = 0
                    for e in range(new_entries):
                        order += 1
                        layer_count += 1
                        prefix = '_' + str(layer_count) + '_'
                        self.element_map[prefix+key] = (order, new_func, uom)
                else:
                    order += 1
                    self.element_map[key] = (order, func, uom)
            self.rows = len(self.element_map)

            # create ordered lists of keys, titles and units
            # for the csv output

            for k, v in sorted(self.element_map.iteritems(),
                               key=lambda (k, v): v[0]):
                self.fields.append(k)
                (title, units) = self.get_title_and_units(k)
                self.titles.append(title)
                self.units.append(units)
                self.uom.append(v[2])
        except:
            print 'ERROR reading elements file ', elem
            traceback.print_exc()
            sys.exit(2)
        finally:
            f.close()

    @staticmethod
    def parse_elements(line):
        """Parse function text for upper-case element names.

           Args:
               line (str): line of text

           Returns:
               List of upper-case words
        """
        s = ''
        if 'layer' not in line:
            s = line.translate(None, string.ascii_lowercase)   # del lower-case
            s = s.translate(None, '().')                       # del brackets
            s = s.strip('_')                                   # del _
            s = s.strip(' ')                                   # del spaces
        return list(filter(None, s.split(',')))

    def get_element_names(self):
        """Extract unique set of element names for the metdb request.

           Returns:
               List of unique element names
        """
        elements_list = []
        for k, v in self.element_map.iteritems():
            elements = Elements.parse_elements(v[1])
            # delete any non-MetDB named elements
            for e in elements:
                if '%' in e:
                    elements.remove(e)

            elements_list = list(set(elements_list + elements))

        # add any layer elements
        for k, v in self.layer_map.iteritems():
            for e in v[0]:
                if e in elements_list:
                    elements_list.remove(e)
            elements_list.append(v)
        return elements_list

    def __repr__(self):
        """Format element dictionary.

           Returns:
              String containing a nicely formatted table of element
              details for printing.
        """
        output = ''
        for k, v in sorted(self.element_map.items(),
                           key=lambda(k, (v1, v2, v3)): v1):
            output += '{:35s}{:5s}{:40s}\n'.format(k, ' --- ', v[1])
        return output

    def get_title_and_units(self, field):
        """Split field into components of title and units where units
           are optional and enclosed in brackets.
        """
        pattern = re.compile("(.*)(\()(.*)(\))")
        m = pattern.match(field)
        if m:
            title = m.group(1)
            units = m.group(3)
        else:
            title = field
            units = ''
        return (title, units)

    def define_layer(self, expression):
        """Extract layer name and contents from the function expression
           given in the form (layer_name,contents...,count).
           First argument is layer name which is used internally to
           refer to a set of replicated elements, last is the max replication
           count and the ones in between are the elements on that
           layer.
           Returns a tuple consisting of the layer name and a
           nested list in the format required by the retieval module.
           For example:

           >>> define_layer(sig_wx,SIG_WX_INSY,SIG_WX_DSC,SIG_WX_PHNM,3)
           ('sig_wx',(('SIG_WX_INSY','SIG_WX_DSC','SIG_WX_PHNM'),3))

        """
        pattern = re.compile("(.*)(\()(.*)(\))")
        m = pattern.match(expression)
        if m:
            func = m.group(1)
            argstr = m.group(3)
        else:
            print 'Error in define_layer:', expression
            sys.exit(2)

        args = argstr.split(',')
        key = args[0]
        value = ((tuple(args[1:-1])), int(args[-1]))
        return (key, value)

    def from_layer(self, expression):
        """Extract the number of replications for this layer and
           the function required for this element from the function
           expression.
           parameter: function expression
           returns: the replication count and function.
           For example:

           >>> from_layer(sig_wx,code_lookup(%020192,SIG_WX_INSY_ID))
           (3,'code_lookup(%020192,SIG_WX_INSY_ID)')
        """
        layers = 0
        func = ''
        # parse the expression to find the layer name and the
        # element function
        pattern = re.compile("(.*?\()(.*?),(.*?\))")
        m = pattern.match(expression)
        if m:
            func = m.group(3)
            layer_name = m.group(2)
        else:
            print 'Error in from_layer:', expression
            sys.exit(2)
        layers = self.layer_map[layer_name][1]

        # remove outer function
        if '(' not in func:
            func = func.replace(')', '')

        return (layers, func)


# ----------------------------------------------------------------------
def time_from_ref(ref, hour=None, start=True, days_ago=0, time_span=1):
    """Create a MetDB format date/time string.

       Ref       - a datetime object.
       hour      - optional, if not given then ref is used.
       start     - optional, default is True.
       days_ago  - optional, default 0 days ago i.e. today
       time_span - optional, no of hours to be retrieved, default 1 hour
       When returning a start time then minutes are set to 00
       when returning an end time use the time_span to calculate the hour and
       set the minutes to 59.
       If the requested hour is in the future and we are retrieving from today
       (i.e. days_ago is 0) then use the day before the ref date instead.
    """
    ref = ref - timedelta(days=days_ago)
    copy_ref = ref

    if hour is None:
        hr = ref.hour
        min = ref.minute
    else:
        hr = hour
        if start:
            min = 0
        else:
            hr = (hr + time_span - 1)
            min = 59

    if ref.hour < hr and days_ago == 0:
        copy_ref = copy_ref - dt.timedelta(days=1)

    return copy_ref.strftime("%Y%m%d/") + \
        '{:02d}{:02d}'.format(hr, min) + 'Z'


# -------------------------------------------------------------------
def hours_back_from(now, hours):
    """Return the hour that is 'hours' back from now.
       Parameters: now - a datetime object representing the current
                         time
                   hours - a list of negative integers saying how
                           many hours to adjust by
       Returns: integer list - adjusted hours
    """
    hour_list = []

    ref = now
    for h in hours:
        hour_list.append((ref - dt.timedelta(hours=-h)).hour)
    hour_list.append(ref.hour)
    return hour_list


# -------------------------------------------------------------------
def process_function(expression, obs, i, number=None):
    """Get arguments for the given function, evaluate it and
       returns the results as a string representation.

       obs is a numpy masked array as returned by the metdb call
       (therefore it can be indexed by element name).
    """
    global sites

# get the variable names from the expression...
    args_list = Elements.parse_elements(expression)
    args = []
    value = ''

# ...and put them in an ordered list

    for a in args_list:
        if '%' not in a:
            if number >= 0:
                args.append(obs[a][i][number])
            else:
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
            print 'Error calling function', func, args
            traceback.print_exc()
            sys.exit(2)
    else:
        num = args[0]
        if num is not MDI:
            value = str(num)
    return value


# ----------------------------------------------------------------------
def get_data():
    """Main program."""

# sites is a global variable so that its functions can be used in the
# generic process_function call without having to pass the instance
# every time.

    global sites
    test = False

# Obtain the name of the config file, must be supplied with -c <file>
    config_file = ''
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'htc:')
    except getopt.GetoptError:
        print sys.argv[0], ' -c <configfile>'
        sys.exit(2)

    for opt, arg in opts:
        if opt in ('-h'):
            print sys.argv[0], ' [-t] -c <configfile>'
            print 'Options: -t test mode - settings not updated'
            sys.exit()
        elif opt in ('-t'):
            print 'TEST MODE'
            test = True
        elif opt in ('-c'):
            config_file = arg

    if not config_file:
        print sys.argv[0], ' must supply config file with -c'
        sys.exit(2)

# Read config file to determine settings

    settings = Settings(config_file)
    print 'Config file - ', config_file
    print settings

# check which header lines are required
    if hasattr(settings, 'header'):
        try:
            header_lines = int(settings.header)
        except:
            print 'Invalid header value:', settings.header
            sys.exit(2)
        if header_lines not in range(4):
            print 'Invalid header option (0,1,2,3):', settings.header
            sys.exit(2)
    else:
        header_lines = 0

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
    if test:
        print elements
    elements_list = elements.get_element_names()

# Set up MetDB parameters

    last_run = settings.run_time
    last_run = dt.datetime.strptime(last_run, TFMT)

    now = dt.datetime.utcnow()
    this_run = now.strftime(TFMT)
    timestamp = now.strftime('%Y%m%dT%H%M%S')

# hours is a list of request hours e.g. 00, 12, blank to use the
# current hour or negative numbers for that number of hours
# previously (and will include the current hour too).

    hour_list = []
    if hasattr(settings, 'start_time'):
        hours = settings.start_time
        if hours:
            hour_list = [int(t) for t in hours.split(',')]
            # cannot have negative hours and days_ago set
            if hasattr(settings, 'days_ago') and any(h < 0 for h in hour_list):
                print ('Cannot combine negative start_time hours with '
                       'days_ago parameter.')
                sys.exit(2)
            if all(h < 0 for h in hour_list):
                hour_list = hours_back_from(now, hour_list)

    if len(hour_list) == 0:
        hour_list = [now.hour]

# time_span (number of hours for the retrieval) must be a single positive
# integer. Defaults to a span of 1 hour

    time_span = 1
    if hasattr(settings, 'time_span'):
        try:
            time_span = int(settings.time_span)
        except:
            print 'Invalid time_span value: ' + settings.time_span
            sys.exit(2)
        if time_span < 0:
            print ('Invalid time_span (must be positive integer number of '
                   'hours): ' + settings.time_span)
            sys.exit(2)

# days_ago (number of whole days previously for the retrieval) must be a
# single positive integer. Defaults to 0 days ago, i.e. today. If set then
# cannot use negative hour values in start_time.

    days_ago = 0
    if hasattr(settings, 'days_ago'):
        try:
            days_ago = int(settings.days_ago)
        except:
            print 'Invalid days_ago value: ' + settings.days_ago
            sys.exit(2)
        if days_ago < 0:
            print ('Invalid days_ago (must be positive number of '
                   'days): ' + settings.days_ago)
            sys.exit(2)

# Get platform list, if any

    if hasattr(settings, 'platform'):
        platforms = settings.platform
        platforms = platforms.replace(',', ' ')
    else:
        platforms = None

    nobs = 0       # counts the total number of obs retrieved

# Loop over requests

    for t in hour_list:
        csv_list = []  # list of lines to output
        start_time = time_from_ref(now, days_ago=days_ago, hour=t, start=True)
        end_time = time_from_ref(now, days_ago=days_ago, hour=t, start=False,
                                 time_span=time_span)
        rcpt_time = time_from_ref(last_run)
        ob_dt = '{:02d}'.format(t)

        keywords = ['START TIME ' + start_time,
                    'END TIME ' + end_time,
                    'RECEIVED AFTER ' + rcpt_time]
        if platforms:
            keywords.append('PLATFORM ' + platforms)

        try:
            print 'Calling metdb', keywords, elements_list
            obs = metdb.obs(settings.contact,
                            settings.subtype,
                            keywords,
                            elements_list)
            print 'Retrieved ', len(obs), ' observations'
            nobs += len(obs)
        except IOError:
            print 'WARNING non-zero return from MetDB'
            continue  # with next period

# Loop over observations

        for i in range(len(obs)):
            output_csv = {}
            if (sites and sites.required(obs, i)) or sites is None:
                for k, v in elements.element_map.iteritems():
                    expression = v[1]
                    # check if this is a layer element by looking for a
                    # prefix on the key
                    if k[0] == '_':
                        numStr = k.split('_')[1]
                        num = int(numStr) - 1
                        output_csv[k] = process_function(expression,
                                                         obs, i,
                                                         number=num)
                    else:
                        output_csv[k] = process_function(expression, obs, i)

                # end of loop over elements
                csv_list.append(output_csv)

        # end of loop over obs

# Output data to a file

        if nobs > 0:
            outdir = settings.output_dir
            # create output directory if neccessary
            if not os.path.exists(outdir):
                os.makedirs(outdir)
            output = outdir + '/' + 'tmp.' + settings.output_file
            output = output.replace('<timestamp>', timestamp)
            output = output.replace('<dt>', ob_dt)

            try:
                f = open(output, 'wb')

                # prepare file with ordered list of column names
                writer = csv.DictWriter(f, fieldnames=elements.fields,
                                        extrasaction='ignore',
                                        dialect='excel')

                # add optional headers
                if header_lines == 1:
                    title = dict((n, n) for n in elements.fields)
                    writer.writerow(title)
                elif header_lines == 2:
                    title = dict(zip(elements.fields, elements.titles))
                    writer.writerow(title)
                    units = dict(zip(elements.fields, elements.units))
                    writer.writerow(units)
                elif header_lines == 3:
                    title = dict(zip(elements.fields, elements.titles))
                    writer.writerow(title)
                    units = dict(zip(elements.fields, elements.units))
                    writer.writerow(units)
                    uom = dict(zip(elements.fields, elements.uom))
                    writer.writerow(uom)

                # write the rows
                for row in csv_list:
                    writer.writerow(row)

                print ' '.join(keywords), ' output to ', output
            except:
                print 'ERROR writing output file', output
                traceback.print_exc()
                sys.exit(2)
            finally:
                f.close()
                os.rename(output, output.replace('tmp.', ''))

    # end of loop over requests

# Update config file with run time
    if not test:
        settings.reset({'run_time': this_run})
        settings.write_cfg(config_file)


if __name__ == '__main__':
    get_data()

