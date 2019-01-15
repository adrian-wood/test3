## MetDB Monthly Summary Reports

### Introduction

To demonstrate that the MetDB is meeting the SLA a monthly summary is produced by displaying Nagios XI reports on a web page.
This version replaces one previously held in the metdb-apps repository which also included graphs produced from daily stats.

### Code Location

The system runs from mdb-apps:/var/moods/metrics as user moodsf. This keeps it separate from the MOODS environment and outside the MOODS configuration management process so that changes can be made as and when required. It also allows the system to take advantages of Python packages not available on the MOODS systems (e.g.jinja2).

/var/moods/metrics - contains two scripts

/var/www/html/metrics - template, output pages and archive directory

### Process

Nagios XI reports and graph pages are e-mailed to the MetDB inbox on the 1st of the month.  The attached PDF documents must be saved in the archive folder using the standard naming convention:

   yyyymm01_sla.pdf
   
   yyyymm01_graphs.pdf
   
where the year and month are the previous month, i.e. the start of the period being reported.

### Scripts

#### metdb_monthly_report.py

This should be submitted after the PDFs have been downloaded.  It has no arguments.

#### create_summary.py
```
Usage: create_summary.py [options]
Description: creates monthly summary html.
Options:

  -h, --help            show this help message and exit
  -b BASE               Base directory.
  -t TEMPLATE           Template HTML file relative to BASE dir.
  -d DATE               date of image files yyyymmdd.
  -o OUTFILE, --outfile=OUTFILE output file relative to BASE dir.
```  
This uses the summary_template.html to produce the final monthly summary page and is called from metdb_monthly_report.py. 
